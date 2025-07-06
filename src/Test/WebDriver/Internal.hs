{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The HTTP/JSON plumbing used to implement the 'WD' monad.
--
-- These functions can be used to create your own 'WebDriver' instances, providing extra functionality for your application if desired. All exports
-- of this module are subject to change at any point.
--
module Test.WebDriver.Internal (
  mkRequest
  , getJSONResult
  , handleJSONErr
  , WDResponse(..)

  , WD(..)
  , runWD

  ) where

import Control.Applicative
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 as LBS (null)
import qualified Data.ByteString.Lazy.Internal as LBS (ByteString(..))
import Data.CallStack
import Data.Functor
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Network.HTTP.Client (Request(..), RequestBody(..), Response(..), httpLbs)
import qualified Network.HTTP.Client as HTTPClient
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status (Status(..))
import Prelude -- hides some "unused import" warnings
import Test.WebDriver.Exceptions.Internal
import Test.WebDriver.JSON
import Test.WebDriver.Types
import UnliftIO.Exception (Exception, SomeException(..), toException, throwIO, tryAny)

#if !MIN_VERSION_http_client(0,4,30)
import Data.Default (def)
#endif

-- | A state monad for WebDriver commands. This is a basic monad that has an
-- implementation of 'WebDriver'.
newtype WD a = WD (ReaderT Session IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadFix, MonadMask, MonadUnliftIO)

instance SessionState WD where
  getSession = WD ask

instance WebDriver WD where
  doCommand method path args =
    mkRequest method path args
    >>= (\req -> tryAny $ liftIO $ httpLbs req manager)
    >>= either throwIO return
    >>= getJSONResult
    >>= either throwIO return
    where
      manager = undefined

-- | Executes a 'WD' computation within the 'IO' monad, using the given
-- 'WDSession' as state for WebDriver requests.
runWD :: Session -> WD a -> IO a
runWD sess (WD wd) = runReaderT wd sess

-- | This is the defintion of fromStrict used by bytestring >= 0.10; we redefine it here to support bytestring < 0.10
fromStrict :: BS.ByteString -> LBS.ByteString
fromStrict bs | BS.null bs = LBS.Empty
              | otherwise = LBS.Chunk bs LBS.Empty


-- | Compatibility function to support http-client < 0.4.30
defaultRequest :: Request
#if MIN_VERSION_http_client(0,4,30)
defaultRequest = HTTPClient.defaultRequest
#else
defaultRequest = def
#endif

-- | Construct an HTTP 'Request' value when given a list of headers, method, and URL fragment.
mkRequest :: (Monad m, SessionState m, ToJSON a) => Method -> Text -> a -> m Request
mkRequest meth wdPath args = do
  Session {sessionDriver=(Driver {..})} <- getSession
  let body = case toJSON args of
        Null  -> ""   --passing Null as the argument indicates no request body
        other -> encode other
  return $ defaultRequest {
    host = BS.pack _driverHostname
    , port = _driverPort
    , path = BS.pack _driverBasePath `BS.append`  TE.encodeUtf8 wdPath
    , requestBody = RequestBodyLBS body
    , requestHeaders = _driverRequestHeaders ++ extraHeaders
    , method = meth
#if !MIN_VERSION_http_client(0,5,0)
    , checkStatus = \_ _ _ -> Nothing
#endif
    }
  where
    extraHeaders = [
      (hAccept, "application/json;charset=UTF-8")
      , (hContentType, "application/json;charset=UTF-8")
      ]

-- | Parse a 'WDResponse' object from a given HTTP response.
getJSONResult :: (HasCallStack, MonadIO m, FromJSON a) => Response ByteString -> m (Either SomeException a)
getJSONResult r
  -- Malformed request errors
  | code >= 400 && code < 500 = do
    let lastReq :: Maybe String = undefined
    returnErr . UnknownCommand . maybe reason show $ lastReq
  -- Server-side errors
  | code >= 500 && code < 600 =
    case lookup hContentType headers of
      Just ct
        | "application/json" `BS.isInfixOf` ct ->
          parseJSON'
            (maybe body fromStrict $ lookup "X-Response-Body-Start" headers)
          >>= handleJSONErr
          >>= maybe returnNull returnErr
        | otherwise ->
          returnHTTPErr ServerError
      Nothing ->
        returnHTTPErr (ServerError . ("HTTP response missing content type. Server reason was: "++))
  -- No Content response
  | code == 204 = returnNull
  -- HTTP Success
  | code >= 200 && code < 300 =
    if LBS.null body
      then returnNull
      else do
        rsp@WDResponse {rspVal = val} <- parseJSON' body
        handleJSONErr rsp >>= maybe
          (Right <$> fromJSON' val)
          returnErr
  -- other status codes: return error
  | otherwise = returnHTTPErr (HTTPStatusUnknown code)
  where
    -- Helper functions
    returnErr :: (Exception e, Monad m) => e -> m (Either SomeException a)
    returnErr = return . Left . toException

    returnHTTPErr errType = returnErr . errType $ reason

    returnNull = Right <$> fromJSON' Null

    -- HTTP response variables
    code = statusCode status
    reason = BS.unpack $ statusMessage status
    status = responseStatus r
    body = responseBody r
    headers = responseHeaders r


-- | Determine if a 'WDResponse' is errored, and return a suitable 'Exception' if so.
-- This will be a 'FailedCommand'.
handleJSONErr :: (HasCallStack, MonadIO m) => WDResponse -> m (Maybe SomeException)
handleJSONErr WDResponse{rspStatus = 0} = return Nothing
handleJSONErr WDResponse{rspVal = val, rspStatus = status} = do
  errInfo <- fromJSON' val
  let screen = B64.decodeLenient <$> errScreen errInfo
      seleniumStack = errStack errInfo
      errInfo' = errInfo {
        errSess = Nothing
        -- Append the Haskell stack frames to the ones returned from Selenium
        , errScreen = screen
        , errStack = seleniumStack ++ fmap callStackItemToStackFrame externalCallStack
        }
      e errType = toException $ FailedCommand errType errInfo'
  return . Just $ case status of
    7   -> e NoSuchElement
    8   -> e NoSuchFrame
    9   -> toException . UnknownCommand . errMsg $ errInfo
    10  -> e StaleElementReference
    11  -> e ElementNotVisible
    12  -> e InvalidElementState
    13  -> e UnknownError
    15  -> e ElementIsNotSelectable
    17  -> e JavascriptError
    19  -> e XPathLookupError
    21  -> e Timeout
    23  -> e NoSuchWindow
    24  -> e InvalidCookieDomain
    25  -> e UnableToSetCookie
    26  -> e UnexpectedAlertOpen
    27  -> e NoAlertOpen
    28  -> e ScriptTimeout
    29  -> e InvalidElementCoordinates
    30  -> e IMENotAvailable
    31  -> e IMEEngineActivationFailed
    32  -> e InvalidSelector
    33  -> e SessionNotCreated
    34  -> e MoveTargetOutOfBounds
    51  -> e InvalidXPathSelector
    52  -> e InvalidXPathSelectorReturnType
    _   -> e UnknownError


-- | Internal type representing the JSON response object.
data WDResponse = WDResponse {
  rspSessId :: Maybe SessionId
  , rspStatus :: Word8
  , rspVal    :: Value
  } deriving (Eq, Show)

instance FromJSON WDResponse where
  -- We try both options as the wire format changes depending on
  -- whether selenium is running as a hub or standalone
  parseJSON (Object o) = parseJSONSelenium o
                         <|> (o .: "value" >>= parseJSONWebDriver)
  parseJSON v = typeMismatch "WDResponse" v

parseJSONSelenium :: Object -> Parser WDResponse
parseJSONSelenium o = WDResponse <$> o .: "sessionId"
                                 <*> o .:?? "status" .!= 0
                                 <*> o .:?? "value" .!= Null

parseJSONWebDriver :: Value -> Parser WDResponse
parseJSONWebDriver (Object o) = do
  sessionId <- o .:?? "sessionId" .!= Nothing
  status <- o .:?? "status" .!= 0
  pure $ WDResponse sessionId status (Object o)
parseJSONWebDriver v = pure $ WDResponse Nothing 0 v
