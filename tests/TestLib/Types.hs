{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestLib.Types (
  StaticServerContext(..)
  , staticServer
  , HasStaticServerContext

  , BrowserDependencies(..)
  , browserDependencies
  , HasBrowserDependencies

  , WebDriverContext(..)
  , webdriver
  , HasWebDriverContext

  , WDSession(..)
  , wdSession

  , SessionSpec
  , SpecWithWebDriver

  , getWDConfig
  , getWDConfig'
  ) where

import Control.Exception.Safe
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status as N
import Network.Socket (PortNumber)
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.WebDriver
import Test.WebDriver.Class
import Test.WebDriver.Internal
import Test.WebDriver.Session
import TestLib.Types.Cli
import UnliftIO.IORef


-- * StaticServer

data StaticServerContext = StaticServerContext {
  staticServerHostname :: String
  , staticServerPort :: PortNumber
  }

staticServer :: Label "staticServer" StaticServerContext
staticServer = Label

type HasStaticServerContext context = HasLabel context "staticServer" StaticServerContext

-- * BrowserDependencies

data BrowserDependencies = BrowserDependenciesChrome {
  browserDependenciesChromeChrome :: FilePath
  , browserDependenciesChromeChromedriver :: FilePath
  }
  | BrowserDependenciesFirefox {
      browserDependenciesFirefoxFirefox :: FilePath
      , browserDependenciesFirefoxGeckodriver :: FilePath
      }
  deriving (Show)

browserDependencies :: Label "browserDependencies" BrowserDependencies
browserDependencies = Label

type HasBrowserDependencies context = HasLabel context "browserDependencies" BrowserDependencies

-- * WebDriver

data WebDriverContext = WebDriverContext {
  webDriverHostname :: String
  , webDriverPort :: PortNumber
  }

webdriver :: Label "webdriver" WebDriverContext
webdriver = Label

type HasWebDriverContext context = HasLabel context "webdriver" WebDriverContext

-- * Session

wdSession :: Label "wdSession" (IORef WDSession)
wdSession = Label

type HasWDSession context = HasLabel context "wdSession" (IORef WDSession)

-- * Instances

instance (HasWDSession context, MonadIO m) => WDSessionState (ExampleT context m) where
  getSession = do
    sessVar <- getContext wdSession
    readIORef sessVar

  putSession sess = do
    sessVar <- getContext wdSession
    writeIORef sessVar sess

instance (HasWDSession context, MonadIO m, MonadCatch m) => WebDriver (ExampleT context m) where
  doCommand method path args = do
    req <- mkRequest method path args
    debug [i|--> #{HC.method req} #{HC.path req}#{HC.queryString req} (#{showRequestBody (HC.requestBody req)})|]
    response <- sendHTTPRequest req >>= either throwIO return
    let (N.Status code _) = HC.responseStatus response
    debug [i|<-- #{code} #{HC.responseBody response}|]
    getJSONResult response >>= either throwIO return
    where
      showRequestBody :: HC.RequestBody -> ByteString
      showRequestBody (HC.RequestBodyLBS bytes) = BL.toStrict bytes
      showRequestBody (HC.RequestBodyBS bytes) = bytes
      showRequestBody _ = "<request body>"

-- * Config

getWDConfig :: (
  MonadIO m, MonadReader context m, MonadLogger m
  , HasWebDriverContext context, HasCommandLineOptions context UserOptions
  ) => BrowserDependencies -> m WDConfig
getWDConfig browserDeps = do
  wdc <- getContext webdriver
  getWDConfig' wdc browserDeps

getWDConfig' :: (
  MonadIO m, MonadReader context m, MonadLogger m
  , HasCommandLineOptions context UserOptions
  ) => WebDriverContext -> BrowserDependencies -> m WDConfig
getWDConfig' (WebDriverContext {..}) browserDeps = do
  UserOptions {..} <- getUserCommandLineOptions
  caps <- getCapabilities (fromMaybe False optHeadlessTests) browserDeps
  debug [i|Using browser capabilities: #{caps}|]
  pure $ defaultConfig {
    wdHost = webDriverHostname
    , wdPort = fromIntegral webDriverPort
    , wdCapabilities = caps
    }

getCapabilities :: MonadIO m => Bool -> BrowserDependencies -> m Capabilities
getCapabilities headless (BrowserDependenciesChrome {..}) = pure $ defaultCaps {
  browser = chrome {
      chromeBinary = Just browserDependenciesChromeChrome
      , chromeOptions = (if headless then ["--headless"] else []) <> (chromeOptions chrome)
      }
  }
getCapabilities headless (BrowserDependenciesFirefox {..}) = pure $ defaultCaps {
  browser = firefox {
      ffBinary = Just browserDependenciesFirefoxFirefox
      }
  , additionalCaps = if headless then headlessCaps else []
  }
  where
    headlessCaps = [("moz:firefoxOptions", A.object [("args", A.Array (V.fromList ["-headless"]))])]

-- * Spec types

type SpecWithWebDriver = forall context. (
  HasBaseContext context
  , HasCommandLineOptions context UserOptions
  , HasBrowserDependencies context
  , HasWebDriverContext context
  , HasStaticServerContext context
  , HasFile context "google-chrome-stable"
  , HasNixContext context
  ) => SpecFree context IO ()

type SessionSpec = forall context. (
  HasBaseContext context
  , HasCommandLineOptions context UserOptions
  , HasBrowserDependencies context
  , HasWebDriverContext context
  , HasStaticServerContext context
  ) => SpecFree context IO ()
