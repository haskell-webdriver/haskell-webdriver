{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveDataTypeable,
             RecordWildCards, ScopedTypeVariables #-}
-- |The HTTP/JSON plumbing used to implement the 'WD' monad.
--
-- These functions can be used to create your own 'WebDriver' instances, providing extra functionality for your application if desired. All exports
-- of this module are subject to change at any point.
module Test.WebDriver.Internal
       ( mkRequest, sendHTTPRequest
       , getJSONResult, handleJSONErr, handleRespSessionId
       , WDResponse(..)

       , InvalidURL(..), HTTPStatusUnknown(..), HTTPConnError(..)
       , UnknownCommand(..), ServerError(..)

       , FailedCommand(..), failedCommand, mkFailedCommandInfo
       , FailedCommandType(..), FailedCommandInfo(..), StackFrame(..)
       ) where
import Test.WebDriver.Class
import Test.WebDriver.JSON
import Test.WebDriver.Session
import Test.WebDriver.Config

import Network.HTTP.Client (httpLbs, Request(..), RequestBody(..), Response(..))
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status (Status(..))
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)

import Data.Text as T (Text, unpack, splitOn, null)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 as LBS (length, unpack, null)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Text.Lazy.Encoding as TL

import Control.Monad.Base
import Control.Exception.Lifted (throwIO)
import Control.Applicative
import Control.Exception (SomeException, toException)

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Word (Word, Word8)
import Data.Default

-- |Constructs an HTTP 'Request' value when given a list of headers, HTTP request method, and URL fragment
mkRequest :: (WDSessionState s, WDConfigReader s, ToJSON a) =>
             RequestHeaders -> Method -> Text -> a -> s Request
mkRequest headers meth wdPath args = do
  WDConfig {..} <- askConfig
  let body = case toJSON args of
        Null  -> ""   --passing Null as the argument indicates no request body
        other -> encode other
  return def { host = fromString wdHost
                  , port = fromIntegral wdPort
                  , path = fromString $ wdBasePath ++ T.unpack wdPath
                  , requestBody = RequestBodyLBS body
                  , requestHeaders = headers ++ [ (hAccept, "application/json;charset=UTF-8")
                                                                 , (hContentType, "application/json;charset=UTF-8")
                                                                 , (hContentLength, fromString . show . LBS.length $ body) ]
                  , method = meth }

-- |Sends an HTTP request to the remote WebDriver server
sendHTTPRequest :: (WDSessionState s, WDConfigReader s) => Request -> s (Response ByteString)
sendHTTPRequest req = do
  m <- getManager
  res <- liftBase $ httpLbs req m
  modifySession $ \s -> s {wdSessHist = (req, res) : if wdKeepSessHist s then wdSessHist s else []} 
  return res
  
 
-- |Parses a 'WDResponse' object from a given HTTP response.
getJSONResult :: (WDSessionState s, WDConfigReader s, FromJSON a) => Response ByteString -> s (Either SomeException a)
getJSONResult r
  --malformed request errors
  | code >= 400 && code < 500 = do
    lastReq <- lastHTTPRequest <$> getSession
    returnErr . UnknownCommand . maybe reason show $ lastReq
  --server-side errors
  | code >= 500 && code < 600 = 
    case lookup hContentType headers of
      Just ct
        | "application/json;" `BS.isInfixOf` ct ->
          parseJSON' body
          >>= handleJSONErr
          >>= maybe noReturn returnErr
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
        noReturn
  -- No Content response
  | code == 204 = noReturn
  -- HTTP Success
  | code >= 200 && code < 300 = 
    if LBS.null body
      then noReturn
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
    noReturn = Right <$> fromJSON' Null
    --HTTP response variables
    code = statusCode status
    reason = BS.unpack $ statusMessage status
    status = responseStatus r  
    body = responseBody r  
    headers = responseHeaders r

handleRespSessionId :: (WDSessionState s) => WDResponse -> s ()
handleRespSessionId WDResponse{rspSessId = sessId'} = do
    sess@WDSession { wdSessId = sessId} <- getSession
    case (sessId, (==) <$> sessId <*> sessId') of
       -- if our monad has an uninitialized session ID, initialize it from the response object
       (Nothing, _)    -> putSession sess { wdSessId = sessId' }
       -- if the response ID doesn't match our local ID, throw an error.
       (_, Just False) -> throwIO . ServerError $ "Server response session ID (" ++ show sessId'
                                 ++ ") does not match local session ID (" ++ show sessId ++ ")"
       _ ->  return ()
    
handleJSONErr :: (WDSessionState s, WDConfigReader s) => WDResponse -> s (Maybe SomeException)
handleJSONErr WDResponse{rspStatus = 0} = return Nothing
handleJSONErr WDResponse{rspVal = val, rspStatus = status} = do
  sess <- getSession
  conf <- askConfig
  errInfo <- fromJSON' val
  let screen = B64.decodeLenient <$> errScreen errInfo
      errInfo' = errInfo { errSess = Just (conf, sess)
                                 , errScreen = screen }
      e errType = Just . toException $ FailedCommand errType errInfo'
  return $ case status of
    7   -> e NoSuchElement
    8   -> e NoSuchFrame
    9   -> Just . toException . UnknownCommand . errMsg $ errInfo
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
    405 -> e MethodNotAllowed
    _   -> e UnknownError


-- |Internal type representing the JSON response object
data WDResponse = WDResponse { 
                               rspSessId :: Maybe SessionId
                             , rspStatus :: Word8
                             , rspVal    :: Value
                             }
                  deriving (Eq, Show)

instance FromJSON WDResponse where
  parseJSON (Object o) = WDResponse <$> o .:? "sessionId" .!= Nothing
                                    <*> o .: "status"
                                    <*> o .:? "value" .!= Null
  parseJSON v = typeMismatch "WDResponse" v


instance Exception InvalidURL
-- |An invalid URL was given
newtype InvalidURL = InvalidURL String
                deriving (Eq, Show, Typeable)

instance Exception HTTPStatusUnknown
-- |An unexpected HTTP status was sent by the server.
data HTTPStatusUnknown = HTTPStatusUnknown Int String
                       deriving (Eq, Show, Typeable)

instance Exception HTTPConnError
-- |HTTP connection errors.
data HTTPConnError = HTTPConnError String Int
                   deriving (Eq, Show, Typeable)

instance Exception UnknownCommand
-- |A command was sent to the WebDriver server that it didn't recognize.
newtype UnknownCommand = UnknownCommand String
                    deriving (Eq, Show, Typeable)

instance Exception ServerError
-- |A server-side exception occured
newtype ServerError = ServerError String
                      deriving (Eq, Show, Typeable)

instance Exception FailedCommand
-- |This exception encapsulates a broad variety of exceptions that can
-- occur when a command fails.
data FailedCommand = FailedCommand FailedCommandType FailedCommandInfo
                   deriving (Show, Typeable)

-- |The type of failed command exception that occured.
data FailedCommandType = NoSuchElement
                       | NoSuchFrame
                       | UnknownFrame
                       | StaleElementReference
                       | ElementNotVisible
                       | InvalidElementState
                       | UnknownError
                       | ElementIsNotSelectable
                       | JavascriptError
                       | XPathLookupError
                       | Timeout
                       | NoSuchWindow
                       | InvalidCookieDomain
                       | UnableToSetCookie
                       | UnexpectedAlertOpen
                       | NoAlertOpen
                       | ScriptTimeout
                       | InvalidElementCoordinates
                       | IMENotAvailable
                       | IMEEngineActivationFailed
                       | InvalidSelector
                       | SessionNotCreated
                       | MoveTargetOutOfBounds
                       | InvalidXPathSelector
                       | InvalidXPathSelectorReturnType
                       | MethodNotAllowed
                       deriving (Eq, Ord, Enum, Bounded, Show)

-- |Detailed information about the failed command provided by the server.
data FailedCommandInfo =
  FailedCommandInfo { -- |The error message.
                      errMsg    :: String
                      -- |The session associated with
                      -- the exception.
                    , errSess :: Maybe (WDConfig, WDSession)
                      -- |A screen shot of the focused window
                      -- when the exception occured,
                      -- if provided.
                    , errScreen :: Maybe ByteString
                      -- |The "class" in which the exception
                      -- was raised, if provided.
                    , errClass  :: Maybe String
                      -- |A stack trace of the exception.
                    , errStack  :: [StackFrame]
                    }

-- |Provides a readable printout of the error information, useful for
-- logging.
instance Show FailedCommandInfo where
  show i = showChar '\n'
           . showString "Session: " . sess
           . showChar '\n'
           . showString className . showString ": " . showString (errMsg i)
           . showChar '\n'
           . foldl (\f s-> f . showString "  " . shows s) id (errStack i)
           $ ""
    where
      className = fromMaybe "<unknown exception>" . errClass $ i

      sess = case errSess i of
        Nothing -> showString "None"
        Just (WDConfig {wdHost = host, wdPort = port}, WDSession {wdSessId = msid }) ->
            let  sessId = maybe "<no session id>" show msid
            in showString sessId . showString " at "
                . showString host . showChar ':' . shows port


-- |Constructs a FailedCommandInfo from only an error message.
mkFailedCommandInfo :: (WDSessionState s, WDConfigReader s) => String -> s FailedCommandInfo
mkFailedCommandInfo m = do
  sess <- getSession
  conf <- askConfig
  return $ FailedCommandInfo {errMsg = m , errSess = Just (conf, sess) , errScreen = Nothing
                             , errClass  = Nothing , errStack  = [] }

-- |Convenience function to throw a 'FailedCommand' locally with no server-side
-- info present.
failedCommand :: (WDSessionState s, WDConfigReader s) => FailedCommandType -> String -> s a
failedCommand t m = throwIO . FailedCommand t =<< mkFailedCommandInfo m

-- |An individual stack frame from the stack trace provided by the server
-- during a FailedCommand.
data StackFrame = StackFrame { sfFileName   :: String
                             , sfClassName  :: String
                             , sfMethodName :: String
                             , sfLineNumber :: Word
                             }
                deriving (Eq)


instance Show StackFrame where
  show f = showString (sfClassName f) . showChar '.'
           . showString (sfMethodName f) . showChar ' '
           . showParen True ( showString (sfFileName f) . showChar ':'
                              . shows (sfLineNumber f))
           $ "\n"


instance FromJSON FailedCommandInfo where
  parseJSON (Object o) =
    FailedCommandInfo <$> (req "message" >>= maybe (return "") return)
                      <*> pure Nothing
                      <*> (fmap TL.encodeUtf8 <$> opt "screen" Nothing)
                      <*> opt "class"      Nothing
                      <*> opt "stackTrace" []
    where req :: FromJSON a => Text -> Parser a
          req = (o .:)            --required key
          opt :: FromJSON a => Text -> a -> Parser a
          opt k d = o .:? k .!= d --optional key
  parseJSON v = typeMismatch "FailedCommandInfo" v

instance FromJSON StackFrame where
  parseJSON (Object o) = StackFrame <$> reqStr "fileName"
                                    <*> reqStr "className"
                                    <*> reqStr "methodName"
                                    <*> req    "lineNumber"
    where req :: FromJSON a => Text -> Parser a
          req = (o .:) -- all keys are required
          reqStr :: Text -> Parser String
          reqStr k = req k >>= maybe (return "") return
  parseJSON v = typeMismatch "StackFrame" v
