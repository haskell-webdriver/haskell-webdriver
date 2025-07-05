{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.WebDriver.Session (
  withRequestHeaders
  , withAuthHeaders
  ) where

import Control.Applicative
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS.Lazy as LRWS
import Control.Monad.Trans.RWS.Strict as SRWS
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy as LS
import Control.Monad.Trans.State.Strict as SS
import Control.Monad.Trans.Writer.Lazy as LW
import Control.Monad.Trans.Writer.Strict as SW
import Data.Aeson
import Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (listToMaybe)
import Data.Monoid
import Data.String
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack
import Network.HTTP.Client (Manager, Request, Response)
import Network.HTTP.Types (RequestHeaders)
import Prelude -- hides some "redundant import" warnings
import Test.WebDriver.Monad
import UnliftIO.Exception (SomeException, onException, try, throwIO)


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
