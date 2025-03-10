
module Test.WebDriver.Config (
  -- * WebDriver configuration
  WDConfig(..)
  , defaultConfig

  -- * SessionHistoryConfig options
  , SessionHistoryConfig
  , noHistory
  , unlimitedHistory
  , onlyMostRecentHistory

  -- * Overloadable configuration
  , WebDriverConfig(..)
  ) where

import Control.Monad.IO.Class
import Data.Default (Default, def)
import Data.String (fromString)
import Data.String.Interpolate
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Types (RequestHeaders)
import Test.WebDriver.Capabilities
import Test.WebDriver.Session


-- | WebDriver session configuration
data WDConfig = WDConfig {
  -- | Host name of the WebDriver server for this
  -- session (default 127.0.0.1)
  wdHost :: String
  -- | Port number of the server (default 4444)
  , wdPort :: Int
  -- | Capabilities to use for this session
  , wdCapabilities :: Capabilities
  -- | Base path for all API requests (default "\/wd\/hub")
  , wdBasePath :: String
  -- | Custom request headers to add to every HTTP request.
  , wdRequestHeaders :: RequestHeaders
  -- | Custom request headers to add *only* to session creation requests. This is usually done
  --  when a WebDriver server requires HTTP auth.
  , wdAuthHeaders :: RequestHeaders
  -- | Specifies behavior of HTTP request/response history. By default we use 'unlimitedHistory'.
  , wdHistoryConfig :: SessionHistoryConfig
  -- | Use the given http-client 'Manager' instead of automatically creating one.
  , wdHTTPManager :: Maybe Manager
  -- | Number of times to retry a HTTP request if it times out (default 0)
  , wdHTTPRetryCount :: Int
  -- | Selenium version to target.
  , wdSeleniumVersion :: SeleniumVersion
  }

instance Show WDConfig where
  show (WDConfig {..}) = [i|WDConfig<#{wdHost}:#{wdPort}#{wdBasePath}>|]

instance Default WDConfig where
  def = WDConfig {
    wdHost = "127.0.0.1"
    , wdPort = 4444
    , wdRequestHeaders = []
    , wdAuthHeaders = []
    , wdCapabilities = defaultCaps
    , wdHistoryConfig = unlimitedHistory
    , wdBasePath = "/wd/hub"
    , wdHTTPManager = Nothing
    , wdHTTPRetryCount = 0
    , wdSeleniumVersion = Selenium3
    }

-- | A default session config connects to localhost on port 4444, and hasn't been
-- initialized server-side. This value is the same as 'def' but with a less
-- polymorphic type.
defaultConfig :: WDConfig
defaultConfig = def

-- | Class of types that can configure a WebDriver session.
class WebDriverConfig c where
  -- | Produces a 'Capabilities' from the given configuration.
  mkCaps :: MonadIO m => c -> m Capabilities

  -- | Produces a 'WDSession' from the given configuration.
  mkSession :: MonadIO m => c -> m WDSession

instance WebDriverConfig WDConfig where
  mkCaps (WDConfig {..}) = return wdCapabilities

  mkSession (WDConfig {..}) = do
    manager <- maybe createManager return wdHTTPManager
    return WDSession {
      wdSessHost = fromString $ wdHost
      , wdSessPort = wdPort
      , wdSessRequestHeaders = wdRequestHeaders
      , wdSessAuthHeaders = wdAuthHeaders
      , wdSessBasePath = fromString $ wdBasePath
      , wdSessId = Nothing
      , wdSessHist = []
      , wdSessHistUpdate = wdHistoryConfig
      , wdSessHTTPManager = manager
      , wdSessHTTPRetryCount = wdHTTPRetryCount
      , wdSessSeleniumVersion = wdSeleniumVersion
      }
    where
      createManager = liftIO $ newManager defaultManagerSettings
