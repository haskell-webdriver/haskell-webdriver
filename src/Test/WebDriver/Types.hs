{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.WebDriver.Types (
  WebDriverContext(..)
  -- , HasWebDriverContext
  , mkEmptyWebDriverContext

  , Driver(..)
  , DriverConfig(..)
  , SeleniumVersion(..)

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
import Data.Map as M
import Data.String
import Data.String.Interpolate
import Data.Text as T
import GHC.Stack
import Network.HTTP.Client
import Network.HTTP.Types (RequestHeaders)
import Network.HTTP.Types.Method (methodDelete, methodGet, methodPost, Method)
import Test.WebDriver.Util.Aeson (aesonKeyFromText)
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.Process


-- | The 'WebDriverContext' is an opaque type used by this library for
-- bookkeeping purposes. It tracks all the processes we spin up and all the
-- sessions we create.
--
-- Currently, we will create at most 1 Selenium or Chromedriver process per
-- 'WebDriverContext', and N Geckodriver processes, where N is the number of
-- Firefox sessions you request.
data WebDriverContext = WebDriverContext {
  _webDriverSessions :: MVar (Map String Session)
  , _webDriverSelenium :: MVar (Maybe Driver)
  , _webDriverChromedriver :: MVar (Maybe Driver)
  }

-- | Create a new 'WebDriverContext'.
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
  , _driverManager :: Manager
  , _driverProcess :: Maybe ProcessHandle
  , _driverLogAsync :: Maybe (Async ())
  , _driverConfig :: DriverConfig
  }

data SeleniumVersion =
  Selenium3
  | Selenium4
  deriving (Show, Eq)

-- | Configuration for how to launch a given driver.
data DriverConfig =
  -- | For launching a WebDriver via "java -jar selenium.jar".
  -- Selenium can launch other drivers on your behalf. You should pass these as 'driverConfigSubDrivers'.
  DriverConfigSeleniumJar {
    -- | Path to @java@ binary.
    driverConfigJava :: FilePath
    -- | Extra flags to pass to @java@.
    , driverConfigJavaFlags :: [String]
    -- | Path to @selenium.jar@ file.
    , driverConfigSeleniumJar :: FilePath
    -- | Specify if this is Selenium 3 or 4. If this is not provided, we'll try to autodetect.
    , driverConfigSeleniumVersion :: Maybe SeleniumVersion
    -- | Drivers to configure Selenium to use.
    , driverConfigSubDrivers :: [DriverConfig]
    -- | Directory in which to place driver logs.
    , driverConfigLogDir :: Maybe FilePath
    }
  | DriverConfigGeckodriver {
      -- | Path to @geckodriver@ binary.
      driverConfigGeckodriver :: FilePath
      -- | Extra flags to pass to @geckodriver@.
      , driverConfigGeckodriverFlags :: [String]
      -- | Path to @firefox@ binary.
      , driverConfigFirefox :: FilePath
      -- | Directory in which to place driver logs.
      , driverConfigLogDir :: Maybe FilePath
      }
  | DriverConfigChromedriver {
      -- | Path to @chromedriver@ binary.
      driverConfigChromedriver :: FilePath
      -- | Extra flags to pass to @chromedriver@.
      , driverConfigChromedriverFlags :: [String]
      -- | Path to @chrome@ binary.
      , driverConfigChrome :: FilePath
      -- | Directory in which to place driver logs.
      , driverConfigLogDir :: Maybe FilePath
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
  , sessionWebSocketUrl :: Maybe String
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
