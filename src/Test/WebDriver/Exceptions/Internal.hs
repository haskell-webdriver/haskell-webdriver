{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}

module Test.WebDriver.Exceptions.Internal
       ( InvalidURL(..), HTTPStatusUnknown(..), HTTPConnError(..)
       , UnknownCommand(..), ServerError(..)

       , FailedCommand(..), failedCommand, mkFailedCommandInfo
       , FailedCommandType(..), FailedCommandInfo(..), StackFrame(..)
       , fromTypeString, toTypeString
       , externalCallStack, callStackItemToStackFrame
       ) where
import Test.WebDriver.Session
import Test.WebDriver.JSON

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.CallStack
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text.Lazy.Encoding as TLE

import Control.Applicative
import Control.Exception (Exception)
import Control.Exception.Lifted (throwIO)

import Data.Maybe (fromMaybe, catMaybes)
import Data.Typeable (Typeable)

import Prelude -- hides some "unused import" warnings

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
data FailedCommandType = ElementClickIntercepted
                       | ElementNotInteractable
                       | InsecureCertificate
                       | InvalidArgument
                       | InvalidCookieDomain
                       | InvalidElementState
                       | InvalidSelector
                       | InvalidSessionId
                       | JavascriptError
                       | MoveTargetOutOfBounds
                       | NoSuchAlert
                       | NoSuchCookie
                       | NoSuchElement
                       | NoSuchFrame
                       | NoSuchWindow
                       | ScriptTimeout
                       | SessionNotCreated
                       | StaleElementReference
                       | Timeout
                       | UnableToSetCookie
                       | UnableToCaptureScreen
                       | UnexpectedAlertOpen
                       | UnknownCommandType
                       | UnknownError
                       | UnknownMethod
                       | UnsupportedOperation
                       deriving (Eq, Show)

toTypeString :: FailedCommandType -> String
toTypeString t =
  case t of
    ElementClickIntercepted -> "element click intercepted"
    ElementNotInteractable -> "element not interactable"
    InsecureCertificate -> "insecure certificate"
    InvalidArgument -> "invalid argument"
    InvalidCookieDomain -> "invalid cookie domain"
    InvalidElementState -> "invalid element state"
    InvalidSelector -> "invalid selector"
    InvalidSessionId -> "invalid session id"
    JavascriptError -> "javascript error"
    MoveTargetOutOfBounds -> "move target out of bounds"
    NoSuchAlert -> "no such alert"
    NoSuchCookie -> "no such cookie"
    NoSuchElement -> "no such element"
    NoSuchFrame -> "no such frame"
    NoSuchWindow -> "no such window"
    ScriptTimeout -> "script timeout"
    SessionNotCreated -> "session not created"
    StaleElementReference -> "stale element reference"
    Timeout -> "timeout"
    UnableToSetCookie -> "unable to set cookie"
    UnableToCaptureScreen -> "unable to capture screen"
    UnexpectedAlertOpen -> "unexpected alert open"
    UnknownCommandType -> "unknown command type"
    UnknownError -> "unknown error"
    UnknownMethod -> "unknown method"
    UnsupportedOperation -> "unsupported operation"

fromTypeString :: String -> FailedCommandType
fromTypeString s =
  case s of
    "element click intercepted" -> ElementClickIntercepted
    "element not interactable" -> ElementNotInteractable
    "insecure certificate" -> InsecureCertificate
    "invalid argument" -> InvalidArgument
    "invalid cookie domain" -> InvalidCookieDomain
    "invalid element state" -> InvalidElementState
    "invalid selector" -> InvalidSelector
    "invalid session id" -> InvalidSessionId
    "javascript error" -> JavascriptError
    "move target out of bounds" -> MoveTargetOutOfBounds
    "no such alert" -> NoSuchAlert
    "no such cookie" -> NoSuchCookie
    "no such element" -> NoSuchElement
    "no such frame" -> NoSuchFrame
    "no such window" -> NoSuchWindow
    "script timeout" -> ScriptTimeout
    "session not created" -> SessionNotCreated
    "stale element reference" -> StaleElementReference
    "timeout" -> Timeout
    "unable to set cookie" -> UnableToSetCookie
    "unable to capture screen" -> UnableToCaptureScreen
    "unexpected alert open" -> UnexpectedAlertOpen
    "unknown command type" -> UnknownCommandType
    "unknown error" -> UnknownError
    "unknown method" -> UnknownMethod
    "unsupported operation" -> UnsupportedOperation
    _ -> UnknownError

-- |Detailed information about the failed command provided by the server.
data FailedCommandInfo =
  FailedCommandInfo { -- |The error message.
                      errMsg    :: String
                      -- |The session associated with
                      -- the exception.
                    , -- |The error message.
                      errType   :: String
                      -- |The session associated with
                      -- the exception.
                    , errSess :: Maybe WDSession
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
           . showString "CallStack (from HasCallStack):\n"
           . foldl (\f s-> f . showString "  " . shows s) id (errStack i)
           $ ""
    where
      className = fromMaybe "<unknown exception>" . errClass $ i

      sess = case errSess i of
        Nothing -> showString "None"
        Just WDSession{..} ->
            let sessId = maybe "<no session id>" show wdSessId
            in showString sessId . showString " at "
                . shows wdSessHost . showChar ':' . shows wdSessPort


-- |Constructs a FailedCommandInfo from only an error message.
mkFailedCommandInfo :: (WDSessionState s) => FailedCommandType -> String -> CallStack -> s FailedCommandInfo
mkFailedCommandInfo t m cs = do
  sess <- getSession
  return $ FailedCommandInfo { errMsg = m
                             , errType = toTypeString t
                             , errSess = Just sess
                             , errScreen = Nothing
                             , errClass = Nothing
                             , errStack = fmap callStackItemToStackFrame cs
                             }

-- |Use GHC's CallStack capabilities to return a callstack to help debug a FailedCommand.
-- Drops all stack frames inside Test.WebDriver modules, so the first frame on the stack
-- should be where the user called into Test.WebDriver
externalCallStack :: HasCallStack => CallStack
externalCallStack = dropWhile isWebDriverFrame callStack
  where isWebDriverFrame :: ([Char], SrcLoc) -> Bool
        isWebDriverFrame (_, SrcLoc {srcLocModule}) = "Test.WebDriver" `L.isPrefixOf` srcLocModule

-- |Convenience function to throw a 'FailedCommand' locally with no server-side
-- info present.
failedCommand :: (HasCallStack, WDSessionStateIO s) => FailedCommandType -> String -> s a
failedCommand t m = do
  throwIO . FailedCommand t =<< mkFailedCommandInfo t m callStack

-- |An individual stack frame from the stack trace provided by the server
-- during a FailedCommand.
data StackFrame = StackFrame { sfFileName   :: String
                             , sfClassName  :: String
                             , sfMethodName :: String
                             , sfLineNumber :: Int
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
                      <*> (req "error" >>= maybe (return "") return)
                      <*> pure Nothing
                      <*> (fmap TLE.encodeUtf8 <$> opt "screen" Nothing)
                      <*> opt "class"      Nothing
                      <*> (catMaybes <$> opt "stackTrace" [])
    where req :: FromJSON a => Text -> Parser a
          req = (o .:) . fromText  --required key
          opt :: FromJSON a => Text -> a -> Parser a
          opt k d = o .:?? k .!= d --optional key
  parseJSON v = typeMismatch "FailedCommandInfo" v

instance FromJSON StackFrame where
  parseJSON (Object o) = StackFrame <$> reqStr "fileName"
                                    <*> reqStr "className"
                                    <*> reqStr "methodName"
                                    <*> req    "lineNumber"
    where req :: FromJSON a => Text -> Parser a
          req = (o .:) . fromText -- all keys are required
          reqStr :: Text -> Parser String
          reqStr k = req k >>= maybe (return "") return
  parseJSON v = typeMismatch "StackFrame" v


callStackItemToStackFrame :: (String, SrcLoc) -> StackFrame
callStackItemToStackFrame (functionName, SrcLoc {..}) = StackFrame { sfFileName = srcLocFile
                                                                   , sfClassName = srcLocModule
                                                                   , sfMethodName = functionName
                                                                   , sfLineNumber = srcLocStartLine
                                                                   }
