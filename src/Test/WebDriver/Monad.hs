{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Test.WebDriver.Monad (
  -- * WDSessionState class
  WDSessionState(..)
  , WDSessionStatePut(..)
  , WDSessionStateUnliftIO

  -- ** WebDriver sessions
  , SessionId(..)
  , WDSession(..)

  -- * WebDriver class
  , WebDriver(..)
  , WebDriverBase(..)
  , Method
  , methodDelete
  , methodGet
  , methodPost
  ) where

import Control.Applicative
import Control.Exception.Safe
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString as BS (ByteString)
import Data.CallStack
import Data.String
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro
import Network.HTTP.Client (Manager, Request, Response)
import Network.HTTP.Types (RequestHeaders)
import Network.HTTP.Types.Method (methodDelete, methodGet, methodPost, Method)
import Prelude -- hides some "unused import" warnings

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid) -- for some reason "import Prelude" trick doesn't work with "import Data.Monoid"
#endif


-- | An opaque identifier for a WebDriver session. These handles are produced by
-- the server on session creation, and act to identify a session in progress.
newtype SessionId = SessionId Text
  deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)
instance IsString SessionId where
  fromString s = SessionId (T.pack s)

-- | The local state of a WebDriver session. This structure is passed
-- implicitly through all 'WD' computations.
data WDSession = WDSession {
  -- server hostname
  wdSessHost :: BS.ByteString
  -- server port
  , wdSessPort :: Int
  -- Base path for API requests
  , wdSessBasePath :: BS.ByteString
  -- | An opaque reference identifying the session to use with 'WD' commands.
  , wdSessId   :: SessionId
  -- | HTTP 'Manager' used for connection pooling by the http-client library.
  , wdSessHTTPManager :: Manager
  -- | Number of times to retry a HTTP request if it times out
  , wdSessHTTPRetryCount :: Int
  -- | Custom request headers to add to every HTTP request.
  , wdSessRequestHeaders :: RequestHeaders
  -- | Custom request headers to add *only* to session creation requests. This is
  -- usually done when a WebDriver server requires HTTP auth.
  , wdSessAuthHeaders :: RequestHeaders
  }

instance Show WDSession where
  show (WDSession {..}) = [i|WDSession<[#{wdSessId}] at #{wdSessHost}:#{wdSessPort}#{wdSessBasePath}>|]

class HasLens ctx a where
  getLens :: Lens' ctx a

class WDSessionState m where
  getSession :: m WDSession

class (WDSessionState m) => WDSessionStatePut m where
  withSession :: WDSession -> m a -> m a
  withModifiedSession :: (WDSession -> WDSession) -> m a -> m a

-- type WDSessionState ctx m = (MonadReader ctx m, HasLens ctx WDSession)

-- | Constraint synonym the common pairing of 'WDSessionState' and 'MonadUnliftIO'.
type WDSessionStateUnliftIO m = (WDSessionState m, MonadUnliftIO m)

-- | A class for monads that can handle wire protocol requests. This is the
-- operation underlying all of the high-level commands exported in
-- "Test.WebDriver.Commands".
class (WDSessionState m, MonadUnliftIO m) => WebDriver m where
  doCommand :: (
    HasCallStack, ToJSON a, FromJSON b, ToJSON b
    )
    -- | HTTP request method
    => Method
    -- | URL of request
    -> Text
    -- | JSON parameters passed in the body of the request. Note that, as a
    -- special case, anything that converts to Data.Aeson.Null will result in an
    -- empty request body.
    -> a
    -- | The JSON result of the HTTP request.
    -> m b

-- | A class for monads that can handle wire protocol requests. This is the
-- operation underlying all of the high-level commands exported in
-- "Test.WebDriver.Commands".
class (MonadUnliftIO m) => WebDriverBase m where
  doCommandBase :: (
    HasCallStack, ToJSON a, FromJSON b, ToJSON b
    )
    -- | HTTP request method
    => Method
    -- | URL of request
    -> Text
    -- | JSON parameters passed in the body of the request. Note that, as a
    -- special case, anything that converts to Data.Aeson.Null will result in an
    -- empty request body.
    -> a
    -- | The JSON result of the HTTP request.
    -> m b
