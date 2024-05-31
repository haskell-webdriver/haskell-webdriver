{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TestLib.Contexts.Session where


import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Test.Sandwich
import Test.WebDriver
import Test.WebDriver.Class
import Test.WebDriver.Config
import Test.WebDriver.Session
import TestLib.Contexts.BrowserDependencies
import TestLib.Contexts.WebDriver
import TestLib.Types
import UnliftIO.IORef


instance (HasWDSession context, MonadIO m, MonadBaseControl IO m) => WDSessionState (ExampleT context m) where
  getSession = do
    sessVar <- getContext wdSession
    readIORef sessVar
  putSession sess = do
    sessVar <- getContext wdSession
    writeIORef sessVar sess

instance (HasWDSession context, MonadIO m, MonadBaseControl IO m) => WebDriver (ExampleT context m) where
  doCommand rm t a = do
    sess <- getContext wdSession >>= readIORef
    liftIO $ runWD sess $ doCommand rm t a

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
  browser = (chrome {
                chromeBinary = Just browserDependenciesChromeChrome
                -- , chromeOptions = "--headless=new":[i|--window-size=#{w},#{h}|]:(chromeOptions chrome)
                })
  }
getCapabilities (BrowserDependenciesFirefox {..}) = pure $ defaultCaps {
  browser = (firefox {
                ffBinary = Just browserDependenciesFirefoxFirefox
                })
  }

introduceSession :: forall m context. (
  MonadUnliftIO m, MonadBaseControl IO m
  , HasBaseContext context, HasBrowserDependencies context, HasWebDriverContext context
  ) => SpecFree (LabelValue "wdSession" (IORef WDSession) :> context) m () -> SpecFree context m ()
introduceSession = introduce "Introduce session" wdSession alloc cleanup
  where
    alloc = do
      browserDeps <- getContext browserDependencies

      wdConfig <- getWDConfig browserDeps
      baseSessionVar <- mkSession wdConfig >>= newIORef

      session' <- pushContext wdSession baseSessionVar $
        createSession (wdCapabilities wdConfig)

      writeIORef baseSessionVar session'

      pure baseSessionVar

    cleanup var = do
      pushContext wdSession var $
        closeSession
