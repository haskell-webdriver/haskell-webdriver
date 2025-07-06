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
import Data.Function
import Data.Maybe
import Lens.Micro
import Test.Sandwich
import Test.WebDriver
import Test.WebDriver.Capabilities
import TestLib.Types
import TestLib.Types.Cli

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap          as KM
#else
import qualified Data.HashMap.Strict        as KM
#endif


introduceSession :: forall m context. (
  MonadUnliftIO m, MonadMask m
  , HasBrowserDependencies context, HasWebDriverContext context, HasDriverConfig context, HasCommandLineOptions context UserOptions
  ) => SpecFree (LabelValue "session" Session :> context) m () -> SpecFree context m ()
introduceSession = introduceSession' return

introduceSession' :: forall m context. (
  MonadUnliftIO m, MonadMask m
  , HasBrowserDependencies context, HasWebDriverContext context, HasDriverConfig context, HasCommandLineOptions context UserOptions
  ) => (Capabilities -> ExampleT context m Capabilities) -> SpecFree (LabelValue "session" Session :> context) m () -> SpecFree context m ()
introduceSession' modifyCaps = introduce "Introduce session" session alloc cleanup
  where
    alloc = do
      browserDeps <- getContext browserDependencies
      wdc <- getContext webdriverContext

      UserOptions {..} <- getUserCommandLineOptions
      caps <- getCapabilities (fromMaybe False optHeadlessTests) browserDeps >>= modifyCaps

      dc <- getContext driverConfig

      startSession' wdc dc caps "session1"

    cleanup sess = do
      wdc <- getContext webdriverContext
      closeSession' wdc sess


introduceMobileSession :: forall m context. (
  MonadUnliftIO m, MonadMask m
  , HasBrowserDependencies context, HasWebDriverContext context, HasDriverConfig context, HasCommandLineOptions context UserOptions
  ) => SpecFree (LabelValue "session" Session :> context) m () -> SpecFree context m ()
introduceMobileSession = introduceSession' modifyCaps
  where
    modifyCaps :: Capabilities -> ExampleT context m Capabilities
    modifyCaps x = x
                   & over (capabilitiesGoogChromeOptions . _Just) modifyChromeOptions
                   & over (capabilitiesMozFirefoxOptions . _Just) modifyFirefoxOptions
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

pendingOnNonSelenium :: (MonadReader ctx m, HasDriverConfig ctx, MonadIO m) => m ()
pendingOnNonSelenium = do
  getContext driverConfig >>= \case
    DriverConfigSeleniumJar {} -> return ()
    _ -> pending
