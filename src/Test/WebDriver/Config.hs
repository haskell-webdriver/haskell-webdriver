{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts #-}
module Test.WebDriver.Config(
    -- * WebDriver configuration
    WDConfig(..), defaultConfig, mkSession
    ) where
import Test.WebDriver.Session
import Test.WebDriver.Capabilities

import Data.Default (Default, def)
import Data.String (fromString)

import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Types (RequestHeaders)

import Control.Monad.Base (MonadBase, liftBase)

-- |WebDriver session configuration
data WDConfig = WDConfig {
     -- |Host name of the WebDriver server for this
     -- session (default 127.0.0.1)
      wdHost :: String
     -- |Port number of the server (default 4444)
    , wdPort :: Int
     -- |Additional request headers to send to the server (default [])
    , wdRequestHeaders :: RequestHeaders
     -- |Capabilities to use for this session
    , wdCapabilities :: Capabilities
     -- |Whether or not we should keep a history of HTTP requests/responses
     --
     -- By default, only the last request/response pair is stored (O(1) heap consumption).
     -- Enable this option for more detailed debugging info for HTTP requests.
    , wdKeepSessHist :: Bool
     -- |Base path for all API requests (default "/wd/hub")
    , wdBasePath :: String
     -- |Use the given http-client 'Manager' instead of the default
    , wdHTTPManager :: Maybe Manager

}

instance Default WDConfig where
    def = WDConfig {
      wdHost              = "127.0.0.1"
    , wdPort              = 4444
    , wdRequestHeaders    = []
    , wdCapabilities      = def
    , wdKeepSessHist      = False
    , wdBasePath          = "/wd/hub"
    , wdHTTPManager       = Nothing
    }

{- |A default session config connects to localhost on port 4444, and hasn't been
initialized server-side. This value is the same as 'def' but with a less
polymorphic type. -}
defaultConfig :: WDConfig
defaultConfig = def

-- |Constructs a new 'WDSession' from a given 'WDConfig'
mkSession :: MonadBase IO m => WDConfig -> m WDSession
mkSession WDConfig{..} = do
  manager <- maybe createManager return wdHTTPManager
  return WDSession { wdSessHost = fromString $ wdHost
                   , wdSessPort = wdPort
                   , wdSessBasePath = fromString $ wdBasePath
                   , wdSessId = Nothing
                   , wdSessHist = []
                   , wdSessHistUpdate = histUpdate
                   , wdSessHTTPManager = manager }
  where
    createManager = liftBase $ newManager defaultManagerSettings
    histUpdate
      | wdKeepSessHist = (:)
      | otherwise      = \x _ -> [x]






