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

  , driverConfig
  , HasDriverConfig

  , webdriverContext
  , HasWebDriverContext

  , getCapabilities

  , session
  , HasSession

  , SeleniumVersion(..)

  , SessionSpec
  , SpecWithWebDriver
  ) where

import Control.Exception.Safe
import Control.Monad.IO.Unlift
import Data.Aeson as A
import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate
import Lens.Micro
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status as N
import Network.Socket (PortNumber)
import Test.Sandwich
import Test.Sandwich.Contexts.Nix
import Test.WebDriver
import Test.WebDriver.Capabilities
import Test.WebDriver.Internal
import Test.WebDriver.Types
import TestLib.Types.Cli


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
  , browserDependenciesChromeNoSandbox :: Maybe Bool
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

driverConfig :: Label "driverConfig" DriverConfig
driverConfig = Label

type HasDriverConfig context = HasLabel context "driverConfig" DriverConfig

-- * WebDriver

webdriverContext :: Label "webdriver" WebDriverContext
webdriverContext = Label

type HasWebDriverContext context = HasLabel context "webdriver" WebDriverContext

-- * Session

session :: Label "session" Session
session = Label

type HasSession context = HasLabel context "session" Session

-- * SeleniumVersion

data SeleniumVersion =
  Selenium3
  | Selenium4
  deriving (Show, Eq)

-- * Instances

instance (HasSession context, MonadIO m) => SessionState (ExampleT context m) where
  getSession = getContext session

  -- putSession sess = do
  --   sessVar <- getContext wdSession
  --   writeIORef sessVar sess

instance (MonadUnliftIO m, MonadCatch m) => WebDriverBase (ExampleT context m) where
  doCommandBase driver method path args = do
    let req = mkDriverRequest driver method path args
    -- debug [i|--> Full request: #{req} (#{showRequestBody (HC.requestBody req)})|]
    debug [i|--> #{HC.method req} #{HC.path req}#{HC.queryString req} (#{showRequestBody (HC.requestBody req)})|]
    response <- tryAny (liftIO $ HC.httpLbs req (_driverManager driver)) >>= either throwIO return
    let (N.Status code _) = HC.responseStatus response
    warn [i|<-- #{code} #{response}|]
    return response

    where
      showRequestBody :: HC.RequestBody -> ByteString
      showRequestBody (HC.RequestBodyLBS bytes) = BL.toStrict bytes
      showRequestBody (HC.RequestBodyBS bytes) = bytes
      showRequestBody _ = "<request body>"

instance (HasSession context, MonadUnliftIO m, MonadCatch m) => WebDriver (ExampleT context m) where
  doCommand method path args = do
    Session {sessionDriver} <- getContext session

    let req = mkDriverRequest sessionDriver method path args
    -- debug [i|--> Full request: #{req} (#{showRequestBody (HC.requestBody req)})|]
    debug [i|--> #{HC.method req} #{HC.path req}#{HC.queryString req} (#{showRequestBody (HC.requestBody req)})|]
    response <- tryAny (liftIO $ HC.httpLbs req (_driverManager sessionDriver)) >>= either throwIO return
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

getCapabilities :: MonadIO m => Bool -> BrowserDependencies -> m Capabilities
getCapabilities headless (BrowserDependenciesChrome {..}) = pure $ defaultCaps {
  _capabilitiesBrowserName = Just "chrome"
  , _capabilitiesGoogChromeOptions = Just $
    defaultChromeOptions
      & over (chromeOptionsArgs . non []) (if headless then (\xs -> "--headless" : [i|--window-size=1920,1080|] : xs) else id)
      & over (chromeOptionsArgs . non []) (if browserDependenciesChromeNoSandbox == Just True then ("--no-sandbox" :) else id)
      & set chromeOptionsBinary (Just browserDependenciesChromeChrome)
  }
getCapabilities headless (BrowserDependenciesFirefox {..}) = pure $ defaultCaps {
  _capabilitiesBrowserName = Just "firefox"
  , _capabilitiesMozFirefoxOptions = Just $
    defaultFirefoxOptions
      & set firefoxOptionsBinary (Just browserDependenciesFirefoxFirefox)
      & over (firefoxOptionsArgs . non []) (if headless then ("-headless" :) else id)
  }

-- * Spec types

type SpecWithWebDriver = forall context. (
  HasBaseContext context
  , HasCommandLineOptions context UserOptions
  , HasBrowserDependencies context
  , HasDriverConfig context
  , HasWebDriverContext context
  , HasStaticServerContext context
  , HasNixContext context
  ) => SpecFree context IO ()

type SessionSpec = forall context. (
  HasBaseContext context
  , HasCommandLineOptions context UserOptions
  , HasBrowserDependencies context
  , HasDriverConfig context
  , HasWebDriverContext context
  , HasStaticServerContext context
  ) => SpecFree context IO ()
