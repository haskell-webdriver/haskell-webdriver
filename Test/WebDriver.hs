{-| 
This module serves as the top-level interface to the Haskell WebDriver bindings,
providing most of the functionality you're likely to want.
-}
module Test.WebDriver 
       ( -- * WebDriver sessions
         WD(..), WDSession(..), defaultSession, SessionId(..)
         -- * Running WebDriver tests
       , module Test.WebDriver
         -- * Capabilities and configuration
       , Capabilities(..), defaultCaps, allCaps
       , Platform(..), ProxyType(..)
         -- ** Browser-specific configuration
       , Browser(..), firefox, chrome, ie, opera, iPhone, iPad, android
         -- * WebDriver commands
       , module Test.WebDriver.Commands
         -- * Exceptions
       , InvalidURL(..), NoSessionId(..), BadJSON(..)
       , HTTPStatusUnknown(..), HTTPConnError(..)
       , UnknownCommand(..), ServerError(..)
       , FailedCommand(..), FailedCommandType(..)
       , FailedCommandInfo(..), StackFrame(..)
       , mkFailedCommandInfo, failedCommand
       ) where

import Test.WebDriver.Types
import Test.WebDriver.Commands

import Control.Applicative
import Control.Monad.State.Strict
import Control.Exception.Lifted

import Prelude hiding (catch)

-- |Executes a 'WD' computation within the 'IO' monad, using the given 
-- 'WDSession'.
runWD :: WDSession -> WD a -> IO a
runWD sess (WD wd) = evalStateT wd sess


-- |Like 'runWD', but automatically creates a session beforehand and closes it
-- afterwards. This is a very common use case.
runSession :: WDSession -> Capabilities -> WD a -> IO a
runSession s caps wd = runWD s $ createSession caps >> wd  <* closeSession

-- |Locally sets a 'WDSession' for use within the given 'WD' action.
-- The state of the outer action is unaffected by this function.
-- This function is useful if you need to work with multiple sessions at once.
withSession :: WDSession -> WD a -> WD a
withSession s' (WD wd) = WD . lift $ evalStateT wd s'

-- |A finalizer ensuring that the session is always closed at the end of 
-- the given 'WD' action.
finallyClose:: WD a -> WD a 
finallyClose wd = closeOnException wd <* closeSession


-- |A variant of 'finallyClose' that only closes the session when an 
-- asynchronous exception is thrown
closeOnException :: WD a -> WD a
closeOnException wd = wd `onException` closeSession
