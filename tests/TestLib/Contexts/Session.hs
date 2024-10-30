{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TestLib.Contexts.Session where

import Control.Exception.Safe
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status as N
import Test.Sandwich
import Test.WebDriver
import Test.WebDriver.Class
import Test.WebDriver.Config
import Test.WebDriver.Internal
import Test.WebDriver.Session
import TestLib.Contexts.BrowserDependencies
import TestLib.Contexts.WebDriver
import TestLib.Types
import TestLib.Types.Cli
import UnliftIO.IORef


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
    debug [i|--> #{HC.method req} #{HC.path req}#{HC.queryString req}|]
    response <- sendHTTPRequest req >>= either throwIO return
    let (N.Status code _) = HC.responseStatus response
    debug [i|<-- #{code} #{HC.responseBody response}|]
    getJSONResult response >>= either throwIO return

getWDConfig :: (
  MonadIO m, MonadReader context m, MonadLogger m
  , HasWebDriverContext context, HasCommandLineOptions context UserOptions
  ) => BrowserDependencies -> m WDConfig
getWDConfig browserDeps = do
  WebDriverContext {..} <- getContext webdriver
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

introduceSession :: forall m context. (
  MonadUnliftIO m, MonadCatch m
  , HasBrowserDependencies context, HasWebDriverContext context, HasCommandLineOptions context UserOptions
  ) => SpecFree (LabelValue "wdSession" (IORef WDSession) :> context) m () -> SpecFree context m ()
introduceSession = introduce "Introduce session" wdSession alloc cleanup
  where
    alloc = do
      browserDeps <- getContext browserDependencies

      wdConfig <- getWDConfig browserDeps
      baseSessionVar <- mkSession wdConfig >>= newIORef

      session' <- pushContext wdSession baseSessionVar $
        createSession (wdCapabilities wdConfig)

      info [i|Created session: #{session'}|]

      writeIORef baseSessionVar session'

      pure baseSessionVar

    cleanup var = pushContext wdSession var closeSession
