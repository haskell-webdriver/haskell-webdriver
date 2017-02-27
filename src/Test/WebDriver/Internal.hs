{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards, ScopedTypeVariables, ConstraintKinds, PatternGuards, CPP #-}
-- |The HTTP/JSON plumbing used to implement the 'WD' monad.
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

import Network.HTTP.Types.Header
import Network.HTTP.Types.Status (Status(..))
import Data.Aeson
import Data.Aeson.Types (typeMismatch)

import Data.Text as T (Text, splitOn, null)
import qualified Data.Text.Encoding as TE
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 as LBS (length, unpack, null)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy.Internal as LBS (ByteString(..)) 

import Control.Monad.Base
import Control.Exception.Lifted (throwIO)
import Control.Applicative
import Control.Exception (Exception, SomeException(..), toException, fromException, try)

import Data.String (fromString)
import Data.Word (Word8)

#if !MIN_VERSION_http_client(0,4,30)
import Data.Default.Class
#endif

import Prelude -- hides some "unused import" warnings

--This is the defintion of fromStrict used by bytestring >= 0.10; we redefine it here to support bytestring < 0.10
fromStrict :: BS.ByteString -> LBS.ByteString
fromStrict bs | BS.null bs = LBS.Empty
              | otherwise = LBS.Chunk bs LBS.Empty

              
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
getJSONResult :: (WDSessionStateControl s, FromJSON a) => Response ByteString -> s (Either SomeException a)
getJSONResult r
  --malformed request errors
  | code >= 400 && code < 500 = do
    lastReq <- mostRecentHTTPRequest <$> getSession
    returnErr . UnknownCommand . maybe reason show $ lastReq
  --server-side errors
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
  --redirect case (used as a response to createSession requests) 
  | code == 302 || code == 303 =
    case lookup hLocation headers of
      Nothing ->  returnErr . HTTPStatusUnknown code $ LBS.unpack body
      Just loc -> do
        let sessId = last . filter (not . T.null) . splitOn "/" . fromString $ BS.unpack loc
        modifySession $ \sess -> sess {wdSessId = Just (SessionId sessId)}
        returnNull
  -- No Content response
  | code == 204 = returnNull
  -- HTTP Success
  | code >= 200 && code < 300 = 
    if LBS.null body
      then returnNull
      else do
        rsp@WDResponse {rspVal = val} <- parseJSON' body  
        handleJSONErr rsp >>= maybe  
          (handleRespSessionId rsp >> Right <$> fromJSON' val)
          returnErr
  -- other status codes: return error
  | otherwise = returnHTTPErr (HTTPStatusUnknown code)
  where
    --helper functions
    returnErr :: (Exception e, Monad m) => e -> m (Either SomeException a)
    returnErr = return . Left . toException
    returnHTTPErr errType = returnErr . errType $ reason
    returnNull = Right <$> fromJSON' Null
    --HTTP response variables
    code = statusCode status
    reason = BS.unpack $ statusMessage status
    status = responseStatus r  
    body = responseBody r  
    headers = responseHeaders r

handleRespSessionId :: (WDSessionStateIO s) => WDResponse -> s ()
handleRespSessionId WDResponse{rspSessId = sessId'} = do
    sess@WDSession { wdSessId = sessId} <- getSession
    case (sessId, (==) <$> sessId <*> sessId') of
       -- if our monad has an uninitialized session ID, initialize it from the response object
       (Nothing, _)    -> putSession sess { wdSessId = sessId' }
       -- if the response ID doesn't match our local ID, throw an error.
       (_, Just False) -> throwIO . ServerError $ "Server response session ID (" ++ show sessId'
                                 ++ ") does not match local session ID (" ++ show sessId ++ ")"
       _ ->  return ()
    
handleJSONErr :: (WDSessionStateControl s) => WDResponse -> s (Maybe SomeException)
handleJSONErr WDResponse{rspStatus = 0} = return Nothing
handleJSONErr WDResponse{rspVal = val, rspStatus = status} = do
  sess <- getSession
  errInfo <- fromJSON' val
  let screen = B64.decodeLenient <$> errScreen errInfo
      errInfo' = errInfo { errSess = Just sess
                         , errScreen = screen }
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


-- |Internal type representing the JSON response object
data WDResponse = WDResponse { 
                               rspSessId :: Maybe SessionId
                             , rspStatus :: Word8
                             , rspVal    :: Value
                             }
                  deriving (Eq, Show)

instance FromJSON WDResponse where
  parseJSON (Object o) = WDResponse <$> o .:?? "sessionId" .!= Nothing
                                    <*> o .: "status"
                                    <*> o .:?? "value" .!= Null
  parseJSON v = typeMismatch "WDResponse" v


