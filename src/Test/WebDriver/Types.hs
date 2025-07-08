{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.WebDriver.Types (
  WebDriverContext(..)
  -- , HasWebDriverContext
  , mkEmptyWebDriverContext

  , Driver(..)
  , DriverConfig(..)

  -- * SessionState class
  , SessionState(..)
  , SessionStatePut(..)

  -- ** WebDriver sessions
  , SessionId(..)
  , Session(..)

  -- * Exceptions
  , SessionException(..)

  -- * Stack frames
  , StackFrame(..)
  , callStackItemToStackFrame
  , externalCallStack

  -- * WebDriver class
  , WebDriver
  , WebDriverBase(..)
  , Method
  , methodDelete
  , methodGet
  , methodPost
  ) where

import Control.Monad.IO.Unlift
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.ByteString.Lazy as LBS
import Data.CallStack
import qualified Data.List as L
import Data.Map as M
import Data.String
import Data.String.Interpolate
import Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Types (RequestHeaders)
import Network.HTTP.Types.Method (methodDelete, methodGet, methodPost, Method)
import Test.WebDriver.Util.Aeson (aesonKeyFromText)
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.Process


data WebDriverContext = WebDriverContext {
  _webDriverSessions :: MVar (Map String Session)
  , _webDriverSelenium :: MVar (Maybe Driver)
  , _webDriverChromedriver :: MVar (Maybe Driver)
  }

mkEmptyWebDriverContext :: MonadIO m => m WebDriverContext
mkEmptyWebDriverContext = WebDriverContext
  <$> newMVar mempty
  <*> newMVar Nothing
  <*> newMVar Nothing

data Driver = Driver {
  _driverHostname :: String
  , _driverPort :: Int
  , _driverBasePath :: String
  , _driverRequestHeaders :: RequestHeaders
  , _driverAuthHeaders :: RequestHeaders
  , _driverManager :: Manager
  , _driverProcess :: ProcessHandle
  , _driverConfig :: DriverConfig
  , _driverLogAsync :: Async ()
  }

data DriverConfig =
  DriverConfigSeleniumJar {
    driverConfigJava :: FilePath
    , driverConfigSeleniumJar :: FilePath
    , driverConfigSubDrivers :: [DriverConfig]
    , driverConfigLogDir :: FilePath
    }
  | DriverConfigGeckodriver {
      driverConfigGeckodriver :: FilePath
      , driverConfigFirefox :: FilePath
      , driverConfigLogDir :: FilePath
      }
  | DriverConfigChromedriver {
      driverConfigChromedriver :: FilePath
      , driverConfigChrome :: FilePath
      , driverConfigLogDir :: FilePath
      }

-- | An opaque identifier for a WebDriver session. These handles are produced by
-- the server on session creation, and act to identify a session in progress.
newtype SessionId = SessionId T.Text
  deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)
instance IsString SessionId where
  fromString s = SessionId (T.pack s)

data Session = Session {
  sessionDriver :: Driver
  , sessionId :: SessionId
  , sessionName :: String
  }
instance Show Session where
  show (Session {sessionDriver=(Driver {..}), ..}) = [i|Session<[#{sessionId}] at #{_driverHostname}:#{_driverPort}#{_driverBasePath}>|]

-- class HasLens ctx a where
--   getLens :: Lens' ctx a

data SessionException =
  SessionNameAlreadyExists
  | SessionCreationFailed (Response LBS.ByteString)
  | SessionCreationResponseHadNoSessionId (Response LBS.ByteString)
  deriving (Show)
instance Exception SessionException


class SessionState m where
  getSession :: m Session

class (SessionState m) => SessionStatePut m where
  withModifiedSession :: (Session -> Session) -> m a -> m a

type WebDriver m = (WebDriverBase m, SessionState m)

-- | A class for monads that can handle wire protocol requests. This is the
-- operation underlying all of the high-level commands exported in
-- "Test.WebDriver.Commands".
class (MonadUnliftIO m) => WebDriverBase m where
  doCommandBase :: (
    HasCallStack, ToJSON a
    )
    => Driver
    -- | HTTP request method
    -> Method
    -- | URL of request
    -> Text
    -- | JSON parameters passed in the body of the request. Note that, as a
    -- special case, anything that converts to Data.Aeson.Null will result in an
    -- empty request body.
    -> a
    -- | The response of the HTTP request.
    -> m (Response LBS.ByteString)



-- | Use GHC's 'CallStack' capabilities to return a callstack to help debug a 'FailedCommand'.
-- Drops all stack frames inside Test.WebDriver modules, so the first frame on the stack
-- should be where the user called into Test.WebDriver
externalCallStack :: (HasCallStack) => CallStack
externalCallStack = L.dropWhile isWebDriverFrame callStack
  where
    isWebDriverFrame :: ([Char], SrcLoc) -> Bool
    isWebDriverFrame (_, SrcLoc {srcLocModule}) = "Test.WebDriver" `L.isPrefixOf` srcLocModule

-- | An individual stack frame from the stack trace provided by the server
-- during a FailedCommand.
data StackFrame = StackFrame {
  sfFileName :: String
  , sfClassName  :: String
  , sfMethodName :: String
  , sfLineNumber :: Int
  } deriving (Eq)
instance Show StackFrame where
  show f = showString (sfClassName f) . showChar '.'
           . showString (sfMethodName f) . showChar ' '
           . showParen True ( showString (sfFileName f) . showChar ':'
                              . shows (sfLineNumber f))
           $ "\n"
instance FromJSON StackFrame where
  parseJSON (Object o) = StackFrame <$> reqStr "fileName"
                                    <*> reqStr "className"
                                    <*> reqStr "methodName"
                                    <*> req    "lineNumber"
    where req :: FromJSON a => Text -> Parser a
          req = (o .:) . aesonKeyFromText -- all keys are required
          reqStr :: Text -> Parser String
          reqStr k = req k >>= maybe (return "") return
  parseJSON v = typeMismatch "StackFrame" v

callStackItemToStackFrame :: (String, SrcLoc) -> StackFrame
callStackItemToStackFrame (functionName, SrcLoc {..}) = StackFrame {
  sfFileName = srcLocFile
  , sfClassName = srcLocModule
  , sfMethodName = functionName
  , sfLineNumber = srcLocStartLine
  }
