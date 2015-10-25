{-|
This module serves as the top-level interface to the Haskell WebDriver bindings,
providing most of the functionality you're likely to want.
-}
module Test.WebDriver
  ( -- * WebDriver monad
    WD(..) 
    -- * Running WebDriver commands
  , runSession, withSession, runWD
    -- * WebDriver configuration
  , WDConfig, WDConfig'(..), defaultConfig
    -- ** Session history configuration
  , SessionHistoryConfig, noHistory, unlimitedHistory, onlyMostRecentHistory
    -- ** HTTP request header utilities
  , withRequestHeaders, withAuthHeaders
    -- * WebDriver commands
  , module Test.WebDriver.Commands
    -- * Capabilities (advanced configuration)
  , module Test.WebDriver.Capabilities
   -- * Exception handling
  , finallyClose, closeOnException
  , module Test.WebDriver.Exceptions
    -- * Accessing session history
  , SessionHistory(..), getSessionHistory, dumpSessionHistory
  ) where

import Test.WebDriver.Types
import Test.WebDriver.Commands
import Test.WebDriver.Monad
import Test.WebDriver.Exceptions
import Test.WebDriver.Config
import Test.WebDriver.Session
import Test.WebDriver.Capabilities
