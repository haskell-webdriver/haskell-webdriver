{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Test.WebDriver.Monad (
  -- * WebDriver monad
  WD(..)
  , runWD
  , runWD'
  , runSession
  , finallyClose
  , closeOnException
  , getSessionHistory
  , dumpSessionHistory
  ) where

import Control.Applicative
import Control.Exception.Safe
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put, runStateT)
import Data.CallStack
import Prelude -- hides some "unused import" warnings
import Test.WebDriver.Class
import Test.WebDriver.Commands
import Test.WebDriver.Config
import Test.WebDriver.Internal
import Test.WebDriver.Session


{- | A state monad for WebDriver commands. -}
newtype WD a = WD (StateT WDSession IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadFix, MonadMask)

instance WDSessionState WD where
  getSession = WD get
  putSession = WD . put

instance WebDriver WD where
  doCommand method path args =
    mkRequest method path args
    >>= sendHTTPRequest
    >>= either throwIO return
    >>= getJSONResult
    >>= either throwIO return

-- | Executes a 'WD' computation within the 'IO' monad, using the given
-- 'WDSession' as state for WebDriver requests.
runWD :: WDSession -> WD a -> IO a
runWD sess (WD wd) = evalStateT wd sess

-- | Same as 'runWD', but also returns the final 'WDSession'.
runWD' :: WDSession -> WD a -> IO (a, WDSession)
runWD' sess (WD wd) = runStateT wd sess

-- | Executes a 'WD' computation within the 'IO' monad, automatically creating a new session beforehand.
--
-- NOTE: session is not automatically closed when complete. If you want this behavior, use 'finallyClose'.
-- Example:
--
-- >    runSessionThenClose action = runSession myConfig . finallyClose $ action
runSession :: HasCallStack => WebDriverConfig conf => conf -> WD a -> IO a
runSession conf wd = do
  sess <- mkSession conf
  caps <- mkCaps conf
  runWD sess $ createSession caps >> wd

-- | A finalizer ensuring that the session is always closed at the end of
-- the given 'WD' action, regardless of any exceptions.
finallyClose :: (HasCallStack, WebDriver wd, MonadMask wd) => wd a -> wd a
finallyClose wd = closeOnException wd <* closeSession

-- | Exception handler that closes the session when an
-- asynchronous exception is thrown, but otherwise leaves the session open
-- if the action was successful.
closeOnException :: (HasCallStack, MonadMask wd) => WebDriver wd => wd a -> wd a
closeOnException wd = wd `onException` closeSession

-- | Gets the command history for the current session.
getSessionHistory :: (WDSessionState wd) => wd [SessionHistory]
getSessionHistory = fmap wdSessHist getSession

-- | Prints a history of API requests to stdout after computing the given action.
dumpSessionHistory :: (HasCallStack, WDSessionStateControl wd, MonadMask wd) => wd a -> wd a
dumpSessionHistory = (`finally` (getSession >>= liftIO . print . wdSessHist))
