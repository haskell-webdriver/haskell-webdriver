
module Test.WebDriver.Exceptions (
  InvalidURL(..)
  , NoSessionId(..)
  , BadJSON(..)

  , HTTPStatusUnknown(..)

  , UnknownCommand(..)
  , ServerError(..)

  , FailedCommand(..)
  , FailedCommandType(..)

  , FailedCommandInfo(..)
  , StackFrame(..)

  , mkFailedCommandInfo
  , failedCommand
  ) where

import Control.Applicative
import Control.Exception (Exception)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.CallStack
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Typeable (Typeable)
import Prelude -- hides some "unused import" warnings
import Test.WebDriver.JSON
import Test.WebDriver.Types
import UnliftIO.Exception (throwIO)


-- | An invalid URL was given
newtype InvalidURL = InvalidURL String
  deriving (Eq, Show, Typeable)
instance Exception InvalidURL

-- | An unexpected HTTP status was sent by the server.
data HTTPStatusUnknown = HTTPStatusUnknown Int String
  deriving (Eq, Show, Typeable)
instance Exception HTTPStatusUnknown

-- | A command was sent to the WebDriver server that it didn't recognize.
newtype UnknownCommand = UnknownCommand String
  deriving (Eq, Show, Typeable)
instance Exception UnknownCommand

-- | A server-side exception occured
newtype ServerError = ServerError String
  deriving (Eq, Show, Typeable)
instance Exception ServerError

-- | This exception encapsulates a broad variety of exceptions that can
-- occur when a command fails.
data FailedCommand = FailedCommand FailedCommandType FailedCommandInfo
  deriving (Show, Typeable)
instance Exception FailedCommand

-- | A command requiring a session ID was attempted when no session ID was
-- available.
data NoSessionId = NoSessionId String CallStack
  deriving (Eq, Show, Typeable)
instance Exception NoSessionId

-- | The type of failed command exception that occured.
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

-- | Detailed information about the failed command provided by the server.
data FailedCommandInfo = FailedCommandInfo {
  -- | The error message.
  errMsg :: String
  -- | The session associated with
  -- the exception.
  , errSess :: Maybe Session
  -- | A screen shot of the focused window
  -- when the exception occured,
  -- if provided.
  , errScreen :: Maybe ByteString
  -- | The "class" in which the exception
  -- was raised, if provided.
  , errClass  :: Maybe String
  -- | A stack trace of the exception.
  , errStack  :: [StackFrame]
  }

-- | Provides a readable printout of the error information, useful for
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
        Just s -> \x -> show s <> ": " <> x

-- | Constructs a FailedCommandInfo from only an error message.
mkFailedCommandInfo :: (Monad m, SessionState m) => String -> CallStack -> m FailedCommandInfo
mkFailedCommandInfo m cs = do
  sess <- getSession
  return $ FailedCommandInfo {
    errMsg = m
    , errSess = Just sess
    , errScreen = Nothing
    , errClass = Nothing
    , errStack = fmap callStackItemToStackFrame cs
    }

-- | Convenience function to throw a 'FailedCommand' locally with no server-side
-- info present.
failedCommand :: (HasCallStack, SessionState m, MonadIO m) => FailedCommandType -> String -> m a
failedCommand t m = throwIO . FailedCommand t =<< mkFailedCommandInfo m externalCallStack


instance FromJSON FailedCommandInfo where
  parseJSON (Object o) =
    FailedCommandInfo <$> (req "message" >>= maybe (return "") return)
                      <*> pure Nothing
                      <*> (fmap TLE.encodeUtf8 <$> opt "screen" Nothing)
                      <*> opt "class"      Nothing
                      <*> (catMaybes <$> opt "stackTrace" [])
    where req :: FromJSON a => Text -> Parser a
          req = (o .:) . aesonKeyFromText  --required key
          opt :: FromJSON a => Text -> a -> Parser a
          opt k d = o .:?? k .!= d --optional key
  parseJSON v = typeMismatch "FailedCommandInfo" v
