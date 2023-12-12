{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE CPP #-}

-- | The HTTP/JSON plumbing used to implement the 'WD' monad.
--
-- These functions can be used to create your own 'WebDriver' instances, providing extra functionality for your application if desired. All exports
-- of this module are subject to change at any point.
module Test.WebDriver.Internal
       ( mkRequest, sendHTTPRequest
       , getJSONResult, handleJSONErr, handleRespSessionId
       , WDResponse(..)
       ) where
import Test.WebDriver.Class
import Test.WebDriver.JSON
import Test.WebDriver.Session
import Test.WebDriver.Exceptions.Internal

import Network.HTTP.Client (httpLbs, Request(..), RequestBody(..), Response(..))
import qualified Network.HTTP.Client as HTTPClient

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status (Status(..))

import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 as LBS (null)
import Data.CallStack
import Data.Text as T (Text)
import qualified Data.Text.Encoding as TE

import Control.Applicative
import Control.Exception (Exception, SomeException(..), toException, fromException, try)
import Control.Exception.Lifted (throwIO)
import Control.Monad.Base

#if !MIN_VERSION_http_client(0,4,30)
import Data.Default.Class
#endif

import Prelude -- hides some "unused import" warnings

--Compatability function to support http-client < 0.4.30
defaultRequest :: Request
#if MIN_VERSION_http_client(0,4,30)
defaultRequest = HTTPClient.defaultRequest
#else
defaultRequest = def
#endif

-- |Constructs an HTTP 'Request' value when given a list of headers, HTTP request method, and URL fragment
mkRequest :: (WDSessionState s, ToJSON a) =>
             Method -> Text -> a -> s Request
mkRequest meth wdPath args = do
  WDSession {..} <- getSession
  let body = case toJSON args of
        Null  -> ""   --passing Null as the argument indicates no request body
        other -> encode other
  return defaultRequest
    { host = wdSessHost
    , port = wdSessPort
    , path = wdSessBasePath `BS.append`  TE.encodeUtf8 wdPath
    , requestBody = RequestBodyLBS body
    , requestHeaders = wdSessRequestHeaders
                       ++ [ (hAccept, "application/json;charset=UTF-8")
                          , (hContentType, "application/json;charset=UTF-8") ]
    , method = meth
#if !MIN_VERSION_http_client(0,5,0)
    , checkStatus = \_ _ _ -> Nothing
#endif
    }

-- |Sends an HTTP request to the remote WebDriver server
sendHTTPRequest :: (WDSessionStateIO s) => Request -> s (Either SomeException (Response ByteString))
sendHTTPRequest req = do
  s@WDSession{..} <- getSession
  (nRetries, tryRes) <- liftBase . retryOnTimeout wdSessHTTPRetryCount $ httpLbs req wdSessHTTPManager
  let h = SessionHistory { histRequest = req
                         , histResponse = tryRes
                         , histRetryCount = nRetries
                         }
  putSession s { wdSessHist = wdSessHistUpdate h wdSessHist }
  return tryRes

retryOnTimeout :: Int -> IO a -> IO (Int, (Either SomeException a))
retryOnTimeout maxRetry go = retry' 0
  where
    retry' nRetries = do
      eitherV <- try go
      case eitherV of
        (Left e)
#if MIN_VERSION_http_client(0,5,0)
          | Just (HTTPClient.HttpExceptionRequest _ HTTPClient.ResponseTimeout) <- fromException e
#else
          | Just HTTPClient.ResponseTimeout <- fromException e
#endif
          , maxRetry > nRetries
          -> retry' (succ nRetries)
        other -> return (nRetries, other)

-- |Parses a 'WDResponse' object from a given HTTP response.
getJSONResult :: (HasCallStack, WDSessionStateControl s, FromJSON a) => Response ByteString -> s (Either SomeException a)
getJSONResult r =
  if LBS.null body
    then returnNull
    else do
      rsp@WDResponse {rspVal = val} <- parseJSON' body
      if code == 200
        then handleRespSessionId rsp >> Right <$> fromJSON' val
        else handleJSONErr rsp >>= returnErr
  where
    --helper functions
    returnErr :: (Exception e, Monad m) => e -> m (Either SomeException a)
    returnErr = return . Left . toException
    returnNull = Right <$> fromJSON' Null
    --HTTP response variables
    code = statusCode status
    status = responseStatus r
    body = responseBody r

handleRespSessionId :: (HasCallStack, WDSessionStateIO s) => WDResponse -> s ()
handleRespSessionId WDResponse{rspSessId = sessId'} = do
    sess@WDSession { wdSessId = sessId} <- getSession
    case (sessId, (==) <$> sessId <*> sessId') of
       -- if our monad has an uninitialized session ID, initialize it from the response object
       (Nothing, _)    -> putSession sess { wdSessId = sessId' }
       -- if the response ID doesn't match our local ID, throw an error.
       (_, Just False) -> throwIO . ServerError $ "Server response session ID (" ++ show sessId'
                                 ++ ") does not match local session ID (" ++ show sessId ++ ")"
       _ ->  return ()

handleJSONErr :: (HasCallStack, WDSessionStateControl s) => WDResponse -> s SomeException
handleJSONErr WDResponse{rspVal = val} = do
  sess <- getSession
  errInfo <- fromJSON' val
  let screen = B64.decodeLenient <$> errScreen errInfo
      seleniumStack = errStack errInfo
      errInfo' = errInfo { errSess = Just sess
                         -- Append the Haskell stack frames to the ones returned from Selenium
                         , errScreen = screen
                         , errStack = seleniumStack ++ fmap callStackItemToStackFrame callStack }
      e errType = toException $ FailedCommand errType errInfo'
  return $ e $ fromTypeString $ errType errInfo

-- |Internal type representing the JSON response object
data WDResponse = WDResponse {
                               rspSessId :: Maybe SessionId
                             , rspVal    :: Value
                             }
                  deriving (Eq, Show)

instance FromJSON WDResponse where
  parseJSON (Object o) = WDResponse <$> o .:?? "sessionId" .!= Nothing
                                    <*> o .:?? "value" .!= Null
  parseJSON v = typeMismatch "WDResponse" v
