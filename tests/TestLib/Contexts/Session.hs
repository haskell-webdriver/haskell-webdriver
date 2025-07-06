{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}

module TestLib.Contexts.Session (
  introduceSession
  , introduceSession'

  , introduceMobileSession

  , pendingOnNonSelenium
  ) where

import Control.Exception.Safe
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Maybe
import Data.String.Interpolate
import Lens.Micro
import Test.Sandwich
import Test.WebDriver
import Test.WebDriver.Capabilities
import Test.WebDriver.Types
import TestLib.Types
import TestLib.Types.Cli
import UnliftIO.IORef

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap          as KM
#else
import qualified Data.HashMap.Strict        as KM
#endif


introduceSession :: forall m context. (
  MonadUnliftIO m, MonadCatch m
  , HasBrowserDependencies context, HasTestWebDriverContext context, HasCommandLineOptions context UserOptions
  ) => SpecFree (LabelValue "wdSession" (IORef WDSession) :> context) m () -> SpecFree context m ()
introduceSession = introduceSession' return

introduceSession' :: forall m context. (
  MonadUnliftIO m, MonadCatch m
  , HasBrowserDependencies context, HasTestWebDriverContext context, HasCommandLineOptions context UserOptions
  ) => (WDConfig -> ExampleT context m WDConfig) -> SpecFree (LabelValue "wdSession" (IORef WDSession) :> context) m () -> SpecFree context m ()
introduceSession' modifyConfig = introduce "Introduce session" wdSession alloc cleanup
  where
    alloc = do
      browserDeps <- getContext browserDependencies

      wdConfig <- getWDConfig browserDeps >>= modifyConfig
      sessId <- createSession (_wdCapabilities wdConfig)
      info [i|Created session ID: #{sessId}|]
      mkSession wdConfig sessId >>= newIORef

    cleanup var = pushContext wdSession var closeSession


introduceMobileSession :: forall m context. (
  MonadUnliftIO m, MonadCatch m
  , HasBrowserDependencies context, HasTestWebDriverContext context, HasCommandLineOptions context UserOptions
  ) => SpecFree (LabelValue "wdSession" (IORef WDSession) :> context) m () -> SpecFree context m ()
introduceMobileSession = introduceSession' modifyConfig
  where
    modifyConfig :: WDConfig -> ExampleT context m WDConfig
    modifyConfig x = x
                   & over (wdCapabilities . capabilitiesGoogChromeOptions . _Just) modifyChromeOptions
                   & over (wdCapabilities . capabilitiesMozFirefoxOptions . _Just) modifyFirefoxOptions
                   & return

    modifyChromeOptions :: ChromeOptions -> ChromeOptions
    modifyChromeOptions x = x
                          & set chromeOptionsMobileEmulation (Just (ChromeMobileEmulationSpecificDevice "Pixel 7"))

    modifyFirefoxOptions :: FirefoxOptions -> FirefoxOptions
    modifyFirefoxOptions x = x
                           & over firefoxOptionsPrefs (Just . fromMaybe mempty)
                           & over (firefoxOptionsPrefs . _Just) (KM.insert "general.useragent.override" "Mozilla/5.0 (iPhone; CPU iPhone OS 14_0 like Mac OS X)")

                           & over firefoxOptionsArgs (Just . fromMaybe [])
                           & over (firefoxOptionsArgs . _Just) (\xs -> "--width=375" : "--height=667" : xs)

pendingOnNonSelenium :: (MonadReader ctx m, HasTestWebDriverContext ctx, MonadIO m) => m ()
pendingOnNonSelenium = do
  TestWebDriverContext {..} <- getContext webdriver
  case webDriverDriverType of
    DriverTypeSeleniumJar {} -> return ()
    _ -> pending
