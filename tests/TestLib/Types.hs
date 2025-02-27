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
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status as N
import Network.Socket (PortNumber)
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.WebDriver
import Test.WebDriver.Capabilities
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
  webDriverSeleniumVersion :: SeleniumVersion
  , webDriverHostname :: String
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
    -- debug [i|--> Full request: #{req} (#{showRequestBody (HC.requestBody req)})|]
    debug [i|--> #{HC.method req} #{HC.path req}#{HC.queryString req} (#{showRequestBody (HC.requestBody req)})|]
    response <- sendHTTPRequest req >>= either throwIO return
    let (N.Status code _) = HC.responseStatus response
    -- debug [i|<-- #{code} Full response: #{response}|]
    getJSONResult response >>= \case
      Left e -> do
        warn [i|<-- #{code} Exception: #{e}|]
        throwIO e
      Right result -> do
        debug [i|<-- #{code} #{A.encode result}|]
        return result

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
    , wdSeleniumVersion = webDriverSeleniumVersion
    }

getCapabilities :: MonadIO m => Bool -> BrowserDependencies -> m Capabilities
getCapabilities headless (BrowserDependenciesChrome {..}) = pure $ defaultCaps {
  capabilitiesGoogChromeOptions = Just $ defaultChromeOptions {
      chromeOptionsArgs = Just ((if headless then ["--headless"] else []) <> (fromMaybe [] (chromeOptionsArgs defaultChromeOptions)))
      , chromeOptionsBinary = Just browserDependenciesChromeChrome
      }
  }
getCapabilities headless (BrowserDependenciesFirefox {..}) = pure $ defaultCaps {
  capabilitiesMozFirefoxOptions = Just $ defaultFirefoxOptions {
      firefoxOptionsBinary = Just browserDependenciesFirefoxFirefox
      , firefoxOptionsArgs = Just ((if headless then ["--headless"] else []) <> (fromMaybe [] (firefoxOptionsArgs defaultFirefoxOptions)))
      }
  }

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
