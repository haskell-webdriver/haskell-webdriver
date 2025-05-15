{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TestLib.Contexts.Session (
  introduceSession
  , introduceSession'

  , introduceMobileSession
  ) where

import Control.Exception.Safe
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Lens.Micro
import Test.Sandwich
import Test.WebDriver
import Test.WebDriver.Capabilities
import Test.WebDriver.Config
import Test.WebDriver.Session
import TestLib.Types
import TestLib.Types.Cli
import UnliftIO.IORef


introduceSession :: forall m context. (
  MonadUnliftIO m, MonadCatch m
  , HasBrowserDependencies context, HasWebDriverContext context, HasCommandLineOptions context UserOptions
  ) => SpecFree (LabelValue "wdSession" (IORef WDSession) :> context) m () -> SpecFree context m ()
introduceSession = introduceSession' return

introduceSession' :: forall m context. (
  MonadUnliftIO m, MonadCatch m
  , HasBrowserDependencies context, HasWebDriverContext context, HasCommandLineOptions context UserOptions
  ) => (WDConfig -> ExampleT context m WDConfig) -> SpecFree (LabelValue "wdSession" (IORef WDSession) :> context) m () -> SpecFree context m ()
introduceSession' modifyConfig = introduce "Introduce session" wdSession alloc cleanup
  where
    alloc = do
      browserDeps <- getContext browserDependencies

      wdConfig <- getWDConfig browserDeps >>= modifyConfig
      baseSessionVar <- mkSession wdConfig >>= newIORef

      session' <- pushContext wdSession baseSessionVar $
        createSession (_wdCapabilities wdConfig)

      info [i|Created session: #{session'}|]

      writeIORef baseSessionVar session'

      pure baseSessionVar

    cleanup var = pushContext wdSession var closeSession


introduceMobileSession :: forall m context. (
  MonadUnliftIO m, MonadCatch m
  , HasBrowserDependencies context, HasWebDriverContext context, HasCommandLineOptions context UserOptions
  ) => SpecFree (LabelValue "wdSession" (IORef WDSession) :> context) m () -> SpecFree context m ()
introduceMobileSession = introduceSession' modifyConfig
  where
    modifyConfig :: WDConfig -> ExampleT context m WDConfig
    modifyConfig x = x
                   & over (wdCapabilities . capabilitiesGoogChromeOptions . _Just) modifyChromeOptions
                   & return

    modifyChromeOptions :: ChromeOptions -> ChromeOptions
    modifyChromeOptions x = x
                          & set chromeOptionsMobileEmulation (Just (ChromeMobileEmulationSpecificDevice "Pixel 7"))
