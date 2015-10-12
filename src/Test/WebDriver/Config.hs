{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts, TypeFamilies, ConstraintKinds, UndecidableInstances #-}
module Test.WebDriver.Config(
    -- * WebDriver configuration
      WDConfig(..), defaultConfig
    -- * Capabilities helpers
    , modifyCaps, useBrowser, useVersion, usePlatform, useProxy
    -- * SessionHistoryConfig options
    , SessionHistoryConfig, noHistory, unlimitedHistory, onlyMostRecentHistory
    ) where
import Test.WebDriver.Capabilities
import Test.WebDriver.Browser
import Test.WebDriver.Session.History

import Data.Default (Default, def)

import Network.HTTP.Client (Manager)
import Network.HTTP.Types (RequestHeaders)

-- |WebDriver session configuration
data WDConfig b = WDConfig {
     -- |Host name of the WebDriver server for this
     -- session (default 127.0.0.1)
      wdHost :: String
     -- |Port number of the server (default 4444)
    , wdPort :: Int
     -- |Additional request headers to send to the server during session creation (default [])
    , wdRequestHeaders :: RequestHeaders
     -- |Capabilities to use for this session
    , wdCapabilities :: Capabilities b
     -- |Specifies behavior of HTTP request/response history. By default we use 'unlimitedHistory'.
    , wdHistoryConfig :: SessionHistoryConfig
     -- |Base path for all API requests (default "/wd/hub")
    , wdBasePath :: String
     -- |Use the given http-client 'Manager' instead of automatically creating one.
    , wdHTTPManager :: Maybe Manager
     -- |Number of times to retry a HTTP request if it times out (default 0)
    , wdHTTPRetryCount :: Int
}

instance BrowserTag b => HasBrowserTag (WDConfig b) where
    type BrowserTagOf (WDConfig b) = b

instance GetCapabilities (WDConfig b) where
  getCaps = wdCapabilities

instance SetCapabilities (WDConfig b) where
  setCaps caps conf = conf { wdCapabilities = caps }

-- |A function used to append new requests/responses to session history.
type SessionHistoryConfig = SessionHistory -> [SessionHistory] -> [SessionHistory]

-- |No session history is saved.
noHistory :: SessionHistoryConfig
noHistory _ _ = []

-- |Keep unlimited history
unlimitedHistory :: SessionHistoryConfig
unlimitedHistory = (:)

-- |Saves only the most recent history
onlyMostRecentHistory :: SessionHistoryConfig
onlyMostRecentHistory h _ = [h]

instance Default (Browser b) => Default (WDConfig b) where
    def = WDConfig {
      wdHost              = "127.0.0.1"
    , wdPort              = 4444
    , wdRequestHeaders    = []
    , wdCapabilities      = def
    , wdHistoryConfig     = unlimitedHistory
    , wdBasePath          = "/wd/hub"
    , wdHTTPManager       = Nothing
    , wdHTTPRetryCount    = 0
    }

{- |A default session config connects to localhost on port 4444, and hasn't been
initialized server-side. This value is the same as 'def' but with a less
polymorphic type. -}
defaultConfig :: Default (Browser b) => WDConfig b
defaultConfig = def
