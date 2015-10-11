{-|
This module serves as the top-level interface to the Haskell WebDriver bindings,
providing most of the functionality you're likely to want.
-}
module Test.WebDriver
  ( -- * WebDriver monad
    WD(..) 
    -- * Running WebDriver commands
  , runWD, runSession, withSession
    -- * WebDriver configuration
  , WDConfig(..), defaultConfig
    -- ** Configuration helper functions
    -- | Instead of working with the 'Capabilities' record directly, you can use 
    --   these config modifier functions to specify common options.
  , useBrowser, useProxy, useVersion, usePlatform 
    -- ** Session history configuration
  , SessionHistoryConfig, noHistory, unlimitedHistory, onlyMostRecentHistory
    -- * WebDriver commands
  , module Test.WebDriver.Commands
    -- * Capabilities (advanced configuration)
  , Capabilities(..), defaultCaps, allCaps, modifyCaps
  , Platform(..), ProxyType(..)
    -- ** Browser-specific capabilities
  , Browser(..), LogLevel(..)
    -- *** Browser defaults
  , firefox, chrome, ie, opera, iPhone, iPad, android
   -- * Exception handling
  , finallyClose, closeOnException
  , module Test.WebDriver.Exceptions
    -- * Accessing session history
  , getSessionHistory, dumpSessionHistory
  ) where

import Test.WebDriver.Types
import Test.WebDriver.Commands
import Test.WebDriver.Monad
import Test.WebDriver.Exceptions
import Test.WebDriver.Config
import Test.WebDriver.Session
