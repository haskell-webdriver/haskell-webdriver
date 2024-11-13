{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TestLib.Contexts.Session where

import Control.Exception.Safe
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Test.Sandwich
import Test.WebDriver
import Test.WebDriver.Config
import Test.WebDriver.Session
import TestLib.Types
import TestLib.Types.Cli
import UnliftIO.IORef


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
