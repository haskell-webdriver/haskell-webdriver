{-| 
This module serves as the top-level interface to the Haskell WebDriver bindings,
providing most of the functionality you're likely to want.
-}
module Test.WebDriver 
       ( -- * WebDriver sessions
         WD(..), WDSession(..), defaultSession, SessionId(..)
         -- * Running WebDriver tests
       , runWD, runSession, withSession, finallyClose, closeOnException
         -- * WebDriver commands
       , module Test.WebDriver.Commands
         -- * Capabilities and configuration
       , Capabilities(..), defaultCaps, allCaps
       , Platform(..), ProxyType(..)
         -- ** Browser-specific configuration
       , Browser(..), LogPref(..)
       , firefox, chrome, ie, opera, iPhone, iPad, android
         -- * Exceptions
       , module Test.WebDriver.Exceptions
       ) where

import Test.WebDriver.Types
import Test.WebDriver.Commands
import Test.WebDriver.Monad
import Test.WebDriver.Exceptions