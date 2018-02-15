{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable, ConstraintKinds, FlexibleContexts, NamedFieldPuns #-}
module Test.WebDriver.Exceptions.Internal
       ( InvalidURL(..), HTTPStatusUnknown(..), HTTPConnError(..)
       , UnknownCommand(..), ServerError(..)

       , FailedCommand(..), failedCommand, mkFailedCommandInfo
       , FailedCommandType(..), FailedCommandInfo(..), StackFrame(..)
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

import Debug.Trace

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
                       deriving (Eq, Ord, Enum, Bounded, Show)

-- |Detailed information about the failed command provided by the server.
data FailedCommandInfo =
  FailedCommandInfo { -- |The error message.
                      errMsg    :: String
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
mkFailedCommandInfo :: (WDSessionState s) => String -> CallStack -> s FailedCommandInfo
mkFailedCommandInfo m cs = do
  sess <- getSession
  return $ FailedCommandInfo { errMsg = m
                             , errSess = Just sess
                             , errScreen = Nothing
                             , errClass = Nothing
                             , errStack = fmap callStackItemToStackFrame cs }

-- |Use GHC's CallStack capabilities to return a callstack to help debug a FailedCommand.
-- Drops all stack frames inside Test.WebDriver modules, so the first frame on the stack
-- should be where the user called into Test.WebDriver
externalCallStack :: (HasCallStack) => CallStack
externalCallStack = dropWhile isWebDriverFrame callStack
  where isWebDriverFrame :: ([Char], SrcLoc) -> Bool
        isWebDriverFrame (_, SrcLoc {srcLocModule}) = "Test.WebDriver" `L.isPrefixOf` srcLocModule

-- |Convenience function to throw a 'FailedCommand' locally with no server-side
-- info present.
failedCommand :: (HasCallStack, WDSessionStateIO s) => FailedCommandType -> String -> s a
failedCommand t m = do
  throwIO . FailedCommand t =<< mkFailedCommandInfo m callStack

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
                      <*> pure Nothing
                      <*> (fmap TLE.encodeUtf8 <$> opt "screen" Nothing)
                      <*> opt "class"      Nothing
                      <*> (catMaybes <$> opt "stackTrace" [])
    where req :: FromJSON a => Text -> Parser a
          req = (o .:)            --required key
          opt :: FromJSON a => Text -> a -> Parser a
          opt k d = o .:?? k .!= d --optional key
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


callStackItemToStackFrame :: (String, SrcLoc) -> StackFrame
callStackItemToStackFrame (functionName, SrcLoc {..}) = StackFrame { sfFileName = srcLocFile
                                                                   , sfClassName = srcLocModule
                                                                   , sfMethodName = functionName
                                                                   , sfLineNumber = srcLocStartLine
                                                                   }
