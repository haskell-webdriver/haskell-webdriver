{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module TestLib.Contexts.WebDriver (
  introduceWebDriverContext
  ) where

import Control.Monad
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift
import GHC.Stack
import Test.Sandwich hiding (BrowserToUse(..))
import Test.WebDriver
import TestLib.Types
import UnliftIO.Exception


introduceWebDriverContext :: forall context m. (
  HasCallStack, MonadUnliftIO m, MonadCatch m
  )
  => SpecFree (LabelValue "webdriver" WebDriverContext :> context) m () -> SpecFree context m ()
introduceWebDriverContext = introduceWith "Introduce WebDriver" webdriverContext withAlloc
  where
    withAlloc action = bracket mkEmptyWebDriverContext teardownWebDriverContext (void . action)
