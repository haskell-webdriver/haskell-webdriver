{-# LANGUAGE TemplateHaskell #-}

module Test.WebDriver.Config (
  -- * WebDriver configuration
  WDConfig(..)
  , wdHost
  , wdPort
  , wdRequestHeaders
  , wdAuthHeaders
  , wdCapabilities
  , wdBasePath
  , wdHTTPManager
  , defaultConfig

  -- * Overloadable configuration
  , WebDriverConfig(..)
  ) where

import Control.Monad.IO.Class
import Data.String (fromString)
import Data.String.Interpolate
import Lens.Micro.TH
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Types (RequestHeaders)
import Test.WebDriver.Capabilities
import Test.WebDriver.Monad


-- | WebDriver session configuration
data WDConfig = WDConfig {
  -- | Host name of the WebDriver server for this session (default 127.0.0.1).
  _wdHost :: String
  -- | Port number of the server (default 4444).
  , _wdPort :: Int
  -- | Capabilities to use for this session.
  , _wdCapabilities :: Capabilities
  -- | Base path for all API requests (use "\/wd\/hub" for Selenium).
  , _wdBasePath :: String
  -- | Custom request headers to add to every HTTP request.
  , _wdRequestHeaders :: RequestHeaders
  -- | Custom request headers to add *only* to session creation requests. This
  --  is usually done when a WebDriver server requires HTTP auth.
  , _wdAuthHeaders :: RequestHeaders
  -- | Use the given http-client 'Manager' instead of automatically creating
  -- one.
  , _wdHTTPManager :: Maybe Manager
  }
makeLenses ''WDConfig

instance Show WDConfig where
  show (WDConfig {..}) = [i|WDConfig<#{_wdHost}:#{_wdPort}#{_wdBasePath}>|]

-- | A default session config connects to localhost on port 4444, and hasn't been
-- initialized server-side. This value is the same as 'def' but with a less
-- polymorphic type.
defaultConfig :: WDConfig
defaultConfig = WDConfig {
  _wdHost = "127.0.0.1"
  , _wdPort = 4444
  , _wdRequestHeaders = []
  , _wdAuthHeaders = []
  , _wdCapabilities = defaultCaps
  , _wdBasePath = ""
  , _wdHTTPManager = Nothing
  }

-- | Class of types that can configure a WebDriver session.
class WebDriverConfig c where
  -- | Produces a 'Capabilities' from the given configuration.
  mkCaps :: MonadIO m => c -> m Capabilities

  -- | Produces a 'WDSession' from the given configuration.
  mkSession :: MonadIO m => c -> SessionId -> m WDSession

instance WebDriverConfig WDConfig where
  mkCaps (WDConfig {..}) = return _wdCapabilities

  mkSession (WDConfig {..}) sessId = do
    manager <- maybe createManager return _wdHTTPManager
    return WDSession {
      wdSessHost = fromString $ _wdHost
      , wdSessPort = _wdPort
      , wdSessRequestHeaders = _wdRequestHeaders
      , wdSessAuthHeaders = _wdAuthHeaders
      , wdSessBasePath = fromString $ _wdBasePath
      , wdSessId = sessId
      , wdSessHTTPManager = manager
      }
    where
      createManager = liftIO $ newManager defaultManagerSettings
