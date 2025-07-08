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
import qualified Data.ByteString.Lazy as LBS
import Data.CallStack
import Data.Map as M
import Data.String
import Data.String.Interpolate
import Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Types (RequestHeaders)
import Network.HTTP.Types.Method (methodDelete, methodGet, methodPost, Method)
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
