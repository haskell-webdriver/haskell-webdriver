{-# LANGUAGE FlexibleContexts, TypeFamilies, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses #-}
module Test.WebDriver.Monad
       ( WD(..), runWD, runSession, withSession, finallyClose, closeOnException, dumpSessionHistory
       ) where

import Test.WebDriver.Class
import Test.WebDriver.Session
import Test.WebDriver.Config
import Test.WebDriver.Commands
import Test.WebDriver.Capabilities
import Test.WebDriver.Internal

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl(..), StM)
import Control.Monad.State.Strict (StateT, MonadState, evalStateT, get, put)
--import Control.Monad.IO.Class (MonadIO)
import Control.Exception.Lifted
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Applicative

import Data.Default

{- |A monadic interface to the WebDriver server. This monad is simply a
    state monad transformer over 'IO', threading session information between sequential webdriver commands
-}
newtype WD a = WD (StateT WDSession (ReaderT WDConfig IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadBase IO WD where
  liftBase = WD . liftBase

instance MonadBaseControl IO WD where
  data StM WD a = StWD {unStWD :: StM (StateT WDSession (ReaderT WDConfig IO)) a}

  liftBaseWith f = WD $
    liftBaseWith $ \runInBase ->
    f (\(WD sT) -> liftM StWD . runInBase $ sT)

  restoreM = WD . restoreM . unStWD

instance WDSessionState WD where
  getSession = WD get
  putSession = WD . put
  
instance WDConfigReader WD where
  askConfig = WD ask

instance WebDriver WD where
  doCommand method path args =
    mkRequest [] method path args
    >>= sendHTTPRequest
    >>= getJSONResult
    >>= either throwIO return


-- |Executes a 'WD' computation within the 'IO' monad, using the given
-- ''WDConfig' and 'WDSession'.
runWD :: WDConfig -> WDSession -> WD a -> IO a
runWD conf sess (WD wd) = runReaderT (evalStateT wd sess) conf

-- |Automatically creates a new session beforehand and closes it
-- afterwards. Most common use case
runSession :: WDConfig -> Capabilities -> WD a -> IO a
runSession c caps wd = runWD c def $ closeOnException (createSession caps) >> finallyClose wd

-- |Locally sets a 'WDSession' for use within the given 'WD' action.
-- The state of the outer action is unaffected by this function.
-- This function is useful if you need to work with multiple sessions at once.
withSession :: WDSession -> WD a -> WD a
withSession s' (WD wd) = WD . lift $ evalStateT wd s'

-- |A finalizer ensuring that the session is always closed at the end of
-- the given 'WD' action, regardless of any exceptions.
finallyClose:: WebDriver wd => wd a -> wd a
finallyClose wd = closeOnException wd <* closeSession

-- |Exception handler that closes the session when an
-- asynchronous exception is thrown, but otherwise leaves the session open
-- if the action was successful.
closeOnException :: WebDriver wd => wd a -> wd a
closeOnException wd = wd `onException` closeSession

-- |Prints a history of API requests to stdout after computing the given action.
dumpSessionHistory :: (MonadIO wd, WebDriver wd) => wd a -> wd a
dumpSessionHistory wd = do
    v <- wd
    getSession >>= liftIO . print . wdSessHist
    return v
