{-# LANGUAGE FlexibleContexts, TypeFamilies, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses #-}
module Test.WebDriver.Monad
       ( WD(..), runWD, runSession, withSession, finallyClose, closeOnException
       )where

import Test.WebDriver.Classes
import Test.WebDriver.Commands
import Test.WebDriver.Capabilities
import Test.WebDriver.Internal

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad (liftM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Control (MonadBaseControl(..), StM)
import Control.Monad.State.Strict (StateT, MonadState, evalStateT, get, put)
import Control.Monad.IO.Class (MonadIO)
import Control.Exception.Lifted
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Applicative

{- |A monadic interface to the WebDriver server. This monad is simply a
    state monad transformer over 'IO', threading session information between sequential webdriver commands
-}
newtype WD a = WD (StateT WDSession IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadCatchIO)



{- |Free monad.  This does not actually execute the webdriver session,
    but generates a python script that does.
-}
newtype WDF a = WDF (StateT [String] IO a)
  deriving (...)

{- |This is a bit silly, but it's enough to demonstrate my
    requirements.
-}
newtype UnitTest = UnitTest [String]
  deriving (Monoid, ...)

instance WebDriver WDF where
  doCommand method path args = do
        WDF $ modify ((show (method, path, args)):)

        -- FIXME: since we are not actually running the session, we
        -- can't really return anything here.  (not sure how to fix
        -- this...)

        let JS.Success v = JS.fromJSON JS.Null
        return v

  doSessCommand = ...
  doElemCommand = ...
  doWinCommand = ...

runWDF :: WDF a -> IO UnitTest
runWDF = ...



instance MonadBase IO WD where
  liftBase = WD . liftBase

instance MonadBaseControl IO WD where
  data StM WD a = StWD {unStWD :: StM (StateT WDSession IO) a}

  liftBaseWith f = WD $
    liftBaseWith $ \runInBase ->
    f (\(WD sT) -> liftM StWD . runInBase $ sT)

  restoreM = WD . restoreM . unStWD

instance SessionState WD where
  getSession = WD get
  putSession = WD . put

instance WebDriver WD where
  doCommand method path args = do
    req <- mkRequest [] method path args
    res <- sendHTTPRequest req
    handleHTTPErr res
    handleHTTPResp res

  doSessCommand :: (WebDriver wd, ToJSON a, FromJSON b) =>
                    RequestMethod -> Text -> a -> wd b
  doSessCommand method path args = do
    WDSession { wdSessId = mSessId } <- getSession
    case mSessId of
        Nothing -> throwIO . NoSessionId $ msg
          where
            msg = "doSessCommand: No session ID found for relative URL "
                  ++ show path
        Just (SessionId sId) -> doCommand method
                                (T.concat ["/session/", urlEncode sId, path]) args

  doElemCommand :: (WebDriver wd, ToJSON a, FromJSON b) =>
                    RequestMethod -> Element -> Text -> a -> wd b
  doElemCommand m (Element e) path a =
    doSessCommand m (T.concat ["/element/", urlEncode e, path]) a

  doWinCommand :: (WebDriver wd, ToJSON a, FromJSON b) =>
                   RequestMethod -> WindowHandle -> Text -> a -> wd b
  doWinCommand m (WindowHandle w) path a =
    doSessCommand m (T.concat ["/window/", urlEncode w, path]) a


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
-- the given 'WD' action, regardless of any exceptions.
finallyClose:: WebDriver wd => wd a -> wd a
finallyClose wd = closeOnException wd <* closeSession

-- |A variant of 'finallyClose' that only closes the session when an
-- asynchronous exception is thrown, but otherwise leaves the session open
-- if the action was successful.
closeOnException :: WebDriver wd => wd a -> wd a
closeOnException wd = wd `onException` closeSession
