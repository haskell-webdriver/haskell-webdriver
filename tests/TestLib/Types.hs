{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TestLib.Types where

import Data.IORef
import Test.Sandwich hiding (BrowserToUse(..))
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.WebDriver.Session
import TestLib.Contexts.BrowserDependencies
import TestLib.Contexts.WebDriver
import TestLib.Types.Cli



wdSession :: Label "wdSession" (IORef WDSession)
wdSession = Label

type HasWDSession context = HasLabel context "wdSession" (IORef WDSession)

-- * Spec types

type SpecWithWebDriver = forall context. (
  HasBaseContext context
  , HasCommandLineOptions context UserOptions
  , HasBrowserDependencies context
  , HasWebDriverContext context
  , HasFile context "google-chrome-stable"
  , HasNixContext context
  ) => SpecFree context IO ()

type SessionSpec = forall context. (
  HasBaseContext context
  , HasCommandLineOptions context UserOptions
  , HasBrowserDependencies context
  , HasWebDriverContext context
  ) => SpecFree context IO ()
