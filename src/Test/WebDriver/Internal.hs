{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveDataTypeable #-}
module Test.WebDriver.Internal
       ( mkWDUri, mkRequest
       , handleHTTPErr, handleJSONErr, handleHTTPResp

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
import Data.Aeson.Types (Parser, typeMismatch, emptyArray)

import Data.Text as T (Text, unpack)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 as BS (length, unpack, null)
import qualified Data.ByteString.Char8 as SBS (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Vector as V

import Control.Monad.Base
import Control.Exception.Lifted (throwIO)
import Control.Applicative

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Word (Word, Word8)

mkWDUri :: (SessionState s) => String -> s URI
mkWDUri path = do 
  WDSession{wdHost = host, 
            wdPort = port,
            wdBasePath = basePath
           } <- getSession
  let urlStr   = "http://" ++ host ++ ":" ++ show port
      relPath  = basePath ++ path
      mBaseURI = parseAbsoluteURI urlStr
      mRelURI  = parseRelativeReference relPath
  case (mBaseURI, mRelURI) of
    (Nothing, _) -> throwIO $ InvalidURL urlStr
    (_, Nothing) -> throwIO $ InvalidURL relPath
    (Just baseURI, Just relURI) -> return $ relURI `relativeTo` baseURI

mkRequest :: (SessionState s, ToJSON a) =>
             [Header] -> RequestMethod -> Text -> a -> s (Response ByteString)
mkRequest headers method path args = do
  uri <- mkWDUri (T.unpack path)
  let body = case toJSON args of
        Array v | V.null v -> ""   --an ugly corner case to allow empty requests
        other              -> encode other
      req = Request { rqURI = uri             --todo: normalization of headers
                    , rqMethod = method
                    , rqBody = body
                    , rqHeaders = headers ++ [ Header HdrAccept
                                               "application/json;charset=UTF-8"
                                             , Header HdrContentType
                                               "application/json;charset=UTF-8"
                                             , Header HdrContentLength
                                               . show . BS.length $ body
                                             ]
                    }
  r <- liftBase (simpleHTTP req) >>= either (throwIO . HTTPConnError) return
  return r

handleHTTPErr :: SessionState s => Response ByteString -> s ()
handleHTTPErr r@Response{rspBody = body, rspCode = code, rspReason = reason} =
  case code of
    (4,_,_)  -> err UnknownCommand
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
    (2,0,4) -> returnEmptyArray
    (3,0,x)
      | x `elem` [2,3] -> 
        fromJSON' =<< maybe statusErr (return . String . fromString)
        (findHeader HdrLocation resp)
      where
        statusErr = throwIO . HTTPStatusUnknown code
                             $ (BS.unpack body)
    other
      | BS.null body -> returnEmptyArray
      | otherwise -> fromJSON' . rspVal =<< parseJSON' body
  where
    returnEmptyArray = fromJSON' emptyArray

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
    34  -> e MoveTargetOutOfBounds
    51  -> e InvalidXPathSelector
    52  -> e InvalidXPathSelectorReturnType
    405 -> e MethodNotAllowed
    _   -> e UnknownError


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
                   deriving (Eq, Show, Typeable)

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
                    , errScreen :: Maybe SBS.ByteString
                      -- |The "class" in which the exception
                      -- was raised, if provided.
                    , errClass  :: Maybe String
                      -- |A stack trace of the exception.
                    , errStack  :: [StackFrame]
                    }
  deriving (Eq)

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
                      <*> opt "screen"     Nothing
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
