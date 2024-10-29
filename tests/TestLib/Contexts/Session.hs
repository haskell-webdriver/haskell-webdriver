{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TestLib.Contexts.Session where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.String.Interpolate
import Test.Sandwich
import Test.WebDriver
import Test.WebDriver.Class
import Test.WebDriver.Config
import Test.WebDriver.Monad
import Test.WebDriver.Session
import TestLib.Contexts.BrowserDependencies
import TestLib.Contexts.WebDriver
import TestLib.Types
import UnliftIO.IORef


instance (HasWDSession context, MonadIO m) => WDSessionState (ExampleT context m) where
  getSession = do
    sessVar <- getContext wdSession
    readIORef sessVar

  putSession sess = do
    sessVar <- getContext wdSession
    writeIORef sessVar sess

instance (HasWDSession context, MonadIO m, MonadCatch m) => WebDriver (ExampleT context m) where
  doCommand rm t a = do
    sess <- getSession

    -- Running this in the WD monad creates a StateT which temporarily takes over holding
    -- our session state. But when it completes, we get it back and write it into our own
    -- session var.
    (ret, sess') <- liftIO $ runWD' sess $ doCommand rm t a

    putSession sess'

    pure ret

getWDConfig :: (MonadIO m, MonadReader context m, HasWebDriverContext context) => BrowserDependencies -> m WDConfig
getWDConfig browserDeps = do
  WebDriverContext {..} <- getContext webdriver
  caps <- getCapabilities browserDeps
  pure $ defaultConfig {
    wdHost = webDriverHostname
    , wdPort = fromIntegral webDriverPort
    , wdCapabilities = caps
    }

getCapabilities :: MonadIO m => BrowserDependencies -> m Capabilities
getCapabilities (BrowserDependenciesChrome {..}) = pure $ defaultCaps {
  browser = chrome {
      chromeBinary = Just browserDependenciesChromeChrome
      -- , chromeOptions = "--headless=new":[i|--window-size=#{w},#{h}|]:(chromeOptions chrome)
      }
  }
getCapabilities (BrowserDependenciesFirefox {..}) = pure $ defaultCaps {
  browser = firefox {
      ffBinary = Just browserDependenciesFirefoxFirefox
      }
  }

introduceSession :: forall m context. (
  MonadUnliftIO m, MonadCatch m
  , HasBrowserDependencies context, HasWebDriverContext context
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
