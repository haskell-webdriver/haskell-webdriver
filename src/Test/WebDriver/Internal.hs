{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveDataTypeable #-}
module Test.WebDriver.Internal
       ( mkWDUri, mkRequest, sendHTTPRequest
       , handleHTTPErr, handleJSONErr, handleHTTPResp
       , WDResponse(..)

       , InvalidURL(..), HTTPStatusUnknown(..), HTTPConnError(..)
       , UnknownCommand(..), ServerError(..)

       , FailedCommand(..), failedCommand, mkFailedCommandInfo
       , FailedCommandType(..), FailedCommandInfo(..), StackFrame(..)
       ) where
import Test.WebDriver.Classes
import Test.WebDriver.JSON

import Network.HTTP (simpleHTTP, Request(..), Response(..))
import Network.HTTP.Headers (findHeader, Header(..), HeaderName(..))
import Network.Stream (ConnError)
import Network.URI
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)

import Data.Text as T (Text, unpack, splitOn, null)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 as LBS (length, unpack, null)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Text.Lazy.Encoding as TL

import Control.Monad.Base
import Control.Exception.Lifted (throwIO)
import Control.Applicative

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Word (Word, Word8)

-- |Take a URL path and construct a full URL using the current session state
mkWDUri :: (SessionState s) => String -> s URI
mkWDUri wdPath = do 
  WDSession{wdHost = host, 
            wdPort = port,
            wdBasePath = basePath
           } <- getSession
  let urlStr   = "http://" ++ host ++ ":" ++ show port
      relPath  = basePath ++ wdPath
      mBaseURI = parseAbsoluteURI urlStr
      mRelURI  = parseRelativeReference relPath
  case (mBaseURI, mRelURI) of
    (Nothing, _) -> throwIO $ InvalidURL urlStr
    (_, Nothing) -> throwIO $ InvalidURL relPath
    (Just baseURI, Just relURI) -> return $ relURI `relativeTo` baseURI

-- |Constructs a 'Request' value when given a list of headers, HTTP request method, and URL path
mkRequest :: (SessionState s, ToJSON a) =>
             [Header] -> RequestMethod -> Text -> a -> s (Request ByteString)
mkRequest headers method wdPath args = do
  uri <- mkWDUri (T.unpack wdPath)
  let body = case toJSON args of
        Null  -> ""   --passing Null as the argument indicates no request body
        other -> encode other
  return Request { rqURI = uri             --todo: normalization of headers
                 , rqMethod = method
                 , rqBody = body
                 , rqHeaders = headers ++ [ Header HdrAccept
                                            "application/json;charset=UTF-8"
                                          , Header HdrContentType
                                            "application/json;charset=UTF-8"
                                          , Header HdrContentLength
                                            . show . LBS.length $ body
                                          ]
                 }



sendHTTPRequest :: SessionState s => Request ByteString -> s (Response ByteString) 
sendHTTPRequest req = do
  r <- liftBase (simpleHTTP req) >>= either (throwIO . HTTPConnError) return
  modifySession $ \s -> s {lastHTTPRequest = Just req} -- update lastHTTPRequest field
  return r

handleHTTPErr :: SessionState s => Response ByteString -> s ()
handleHTTPErr r@Response{rspBody = body, rspCode = code, rspReason = reason} =
  case code of
    (4,_,_)  -> do
      lastReq <- lastHTTPRequest <$> getSession            
      throwIO . UnknownCommand . maybe reason show  
        $ lastReq
    (5,_,_)  ->
      case findHeader HdrContentType r of
        Just ct
          | "application/json;" `isInfixOf` ct -> parseJSON' body
                                                  >>= handleJSONErr
          | otherwise -> err ServerError
        Nothing ->
          err (ServerError . ("Missing content type. Server response: "++))

    (2,_,_)  -> return ()
    (3,0,x) | x `elem` [2,3] 
             -> return ()
    _        -> err (HTTPStatusUnknown code)
    where
      err errType = throwIO $ errType reason

handleHTTPResp ::  (SessionState s, FromJSON a) => Response ByteString -> s a
handleHTTPResp resp@Response{rspBody = body, rspCode = code} =
  case code of
    (2,0,4) -> noReturn
    (3,0,x)
      | x `elem` [2,3] ->
        case findHeader HdrLocation resp of        
          Nothing -> throwIO . HTTPStatusUnknown code
                     $ (LBS.unpack body)
          Just loc -> do
            let sessId = last . filter (not . T.null) . splitOn "/" . fromString $ loc
            modifySession $ \sess -> sess {wdSessId = Just (SessionId sessId)}
            fromJSON' . String $ sessId
    _
      | LBS.null body -> noReturn
      | otherwise -> do
        sess@WDSession { wdSessId = sessId}   <- getSession      -- get current session state
        WDResponse { rspSessId = sessId'
                   , rspVal = val}  <- parseJSON' body -- parse response body
        case (sessId, (==) <$> sessId <*> sessId') of
            -- if our monad has an uninitialized session ID, initialize it from the response object
            (Nothing, _)    -> putSession sess { wdSessId = sessId' }
            -- if the response ID doesn't match our local ID, throw an error.
            (_, Just False) -> throwIO . ServerError $ "Server response session ID (" ++ show sessId' 
                                                       ++ ") does not match local session ID (" ++ show sessId ++ ")"
            -- otherwise nothing needs to be done
            _ ->  return ()
        fromJSON' val

  where
    noReturn = fromJSON' Null

handleJSONErr :: SessionState s => WDResponse -> s ()
handleJSONErr WDResponse{rspStatus = 0} = return ()
handleJSONErr WDResponse{rspVal = val, rspStatus = status} = do
  sess <- getSession
  errInfo <- fromJSON' val
  let screen = B64.decodeLenient <$> errScreen errInfo
      errInfo' = errInfo { errSess = sess
                         , errScreen = screen }
      e errType = throwIO $ FailedCommand errType errInfo'
  case status of
    7   -> e NoSuchElement
    8   -> e NoSuchFrame
    9   -> throwIO . UnknownCommand . errMsg $ errInfo
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
data WDResponse = WDResponse { rspSessId :: Maybe SessionId
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
data HTTPStatusUnknown = HTTPStatusUnknown (Int, Int, Int) String
                       deriving (Eq, Show, Typeable)

instance Exception HTTPConnError
-- |HTTP connection errors.
newtype HTTPConnError = HTTPConnError ConnError
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
                    , errSess :: WDSession
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

      sess = showString sessId . showString " at "
             . showString host . showChar ':' . shows port
        where
          sessId = case msid of
            Just (SessionId sid) -> T.unpack sid
            Nothing -> "<no session id>"
          WDSession {wdHost = host, wdPort = port, wdSessId = msid } = errSess i


-- |Constructs a FailedCommandInfo from only an error message.
mkFailedCommandInfo :: SessionState s => String -> s FailedCommandInfo
mkFailedCommandInfo m = do
  sess <- getSession
  return $ FailedCommandInfo {errMsg = m , errSess = sess , errScreen = Nothing
                             , errClass  = Nothing , errStack  = [] }

-- |Convenience function to throw a 'FailedCommand' locally with no server-side
-- info present.
failedCommand :: SessionState s => FailedCommandType -> String -> s a
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
                      <*> pure undefined
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
