{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.WebDriver.Session (
  withRequestHeaders
  , withAuthHeaders
  ) where

import Network.HTTP.Types (RequestHeaders)
import Prelude -- hides some "redundant import" warnings
import Test.WebDriver.Monad


-- | Set a temporary list of custom 'RequestHeaders' to use within the given action.
-- All previous custom headers are temporarily removed, and then restored at the end.
withRequestHeaders :: (WDSessionStatePut m) => RequestHeaders -> m a -> m a
withRequestHeaders h action = do
  withModifiedSession (\s -> s { wdSessRequestHeaders = h }) action

-- | Makes all webdriver HTTP requests in the given action use the session\'s auth headers, typically
-- configured by setting the 'wdAuthHeaders' config. This is useful if you want to temporarily use
-- the same auth headers you used for session creation with other HTTP requests.
withAuthHeaders :: (Monad m, WDSessionStatePut m) => m a -> m a
withAuthHeaders wd = do
  authHeaders <- fmap wdSessAuthHeaders getSession
  withRequestHeaders authHeaders wd

-- -- | A finalizer ensuring that the session is always closed at the end of
-- -- the given 'WD' action, regardless of any exceptions.
-- finallyClose :: (HasCallStack, WebDriver wd, MonadMask wd) => wd a -> wd a
-- finallyClose wd = closeOnException wd <* closeSession

-- -- | Exception handler that closes the session when an
-- -- asynchronous exception is thrown, but otherwise leaves the session open
-- -- if the action was successful.
-- closeOnException :: (HasCallStack, MonadMask wd) => WebDriver wd => wd a -> wd a
-- closeOnException wd = wd `onException` closeSession
