{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, ScopedTypeVariables,
             GeneralizedNewtypeDeriving, RecordWildCards, ConstraintKinds, CPP #-}

#ifndef CABAL_BUILD_DEVELOPER
{-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
#endif

module Test.WebDriver.Session
  ( -- * WDSessionState class
    WDSessionState(..), WDSessionStateIO, WDSessionStateControl, modifySession, withSession
    -- ** WebDriver sessions
  , WDSession(..), mostRecentHistory, mostRecentHTTPRequest, SessionId(..), SessionHistory(..)
    -- * SessionHistoryConfig options
  , SessionHistoryConfig, noHistory, unlimitedHistory, onlyMostRecentHistory
    -- * Using custom HTTP request headers
  , withRequestHeaders, withAuthHeaders
  ) where

import Test.WebDriver.Session.History

import Data.Aeson
import Data.ByteString as BS(ByteString) 
import Data.Text (Text)
import Data.Maybe (listToMaybe)
import Data.Monoid

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Error
import Control.Monad.Trans.Except
--import Control.Monad.Cont
import Control.Monad.Trans.Writer.Strict as SW
import Control.Monad.Trans.Writer.Lazy as LW
import Control.Monad.Trans.State.Strict as SS
import Control.Monad.Trans.State.Lazy as LS
import Control.Monad.Trans.RWS.Strict as SRWS
import Control.Monad.Trans.RWS.Lazy as LRWS

import Control.Exception.Lifted (SomeException, try, throwIO)

--import Network.HTTP.Types.Header (RequestHeaders)
import Network.HTTP.Client (Manager, Request)
import Network.HTTP.Types (RequestHeaders)

import Prelude -- hides some "redundant import" warnings

{- |An opaque identifier for a WebDriver session. These handles are produced by
the server on session creation, and act to identify a session in progress. -}
newtype SessionId = SessionId Text
                  deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

{- |The local state of a WebDriver session. This structure is passed
implicitly through all 'WD' computations -}
data WDSession = WDSession {
                             -- server hostname
                             wdSessHost :: BS.ByteString
                             -- server port
                           , wdSessPort :: Int
                             -- Base path for API requests
                           , wdSessBasePath :: BS.ByteString
                             -- |An opaque reference identifying the session to
                             -- use with 'WD' commands.
                             -- A value of Nothing indicates that a session
                             -- hasn't been created yet.
                             -- Sessions can be created within 'WD' via
                             -- 'Test.WebDriver.createSession', or created
                             -- automatically with 'Test.WebDriver.runSession'
                           , wdSessId   :: Maybe SessionId
                             -- |The complete history of HTTP requests and
                             -- responses, most recent first.
                           , wdSessHist :: [SessionHistory]
                             -- |Update function used to append new entries to session history
                           , wdSessHistUpdate :: SessionHistoryConfig
                             -- |HTTP 'Manager' used for connection pooling by the http-client library.
                           , wdSessHTTPManager :: Manager
                             -- |Number of times to retry a HTTP request if it times out
                           , wdSessHTTPRetryCount :: Int
                             -- |Custom request headers to add to every HTTP request. 
                           , wdSessRequestHeaders :: RequestHeaders
                             -- |Custom request headers to add *only* to session creation requests. This is usually done
                             --  when a WebDriver server requires HTTP auth.
                           , wdSessAuthHeaders :: RequestHeaders
                           }


-- |A function used by 'wdHistoryConfig' to append new entries to session history.
type SessionHistoryConfig = SessionHistory -> [SessionHistory] -> [SessionHistory]

-- |No session history is saved.
noHistory :: SessionHistoryConfig
noHistory _ _ = []

-- |Keep unlimited history
unlimitedHistory :: SessionHistoryConfig
unlimitedHistory = (:)

-- |Saves only the most recent history
onlyMostRecentHistory :: SessionHistoryConfig
onlyMostRecentHistory h _ = [h]

-- |A class for monads that carry a WebDriver session with them. The
-- MonadBaseControl superclass is used for exception handling through
-- the lifted-base package.
class (Monad m, Applicative m) => WDSessionState m where
  
  -- |Retrieves the current session state of the monad
  getSession :: m WDSession
  
  -- |Sets a new session state for the monad
  putSession :: WDSession -> m ()

-- |Constraint synonym for the common pairing of 'WDSessionState' and 'MonadBase' 'IO'.
type WDSessionStateIO s = (WDSessionState s, MonadBase IO s)

-- |Constraint synonym for another common pairing of 'WDSessionState' and 'MonadBaseControl' 'IO'. This
-- is commonly used in library types to indicate use of lifted exception handling.
type WDSessionStateControl s = (WDSessionState s, MonadBaseControl IO s) 

modifySession :: WDSessionState s => (WDSession -> WDSession) -> s ()
modifySession f = getSession >>= putSession . f

-- |Locally sets a session state for use within the given action.
-- The state of any outside action is unaffected by this function.
-- This function is useful if you need to work with multiple sessions simultaneously.
withSession :: WDSessionStateControl m => WDSession -> m a -> m a
withSession s m = do
  s' <- getSession
  putSession s
  (a :: Either SomeException a) <- try m
  putSession s'
  either throwIO return a

-- |The most recent SessionHistory entry recorded by this session, if any.
mostRecentHistory :: WDSession -> Maybe SessionHistory
mostRecentHistory = listToMaybe . wdSessHist
    
-- |The most recent HTTP request issued by this session, if any.
mostRecentHTTPRequest :: WDSession -> Maybe Request
mostRecentHTTPRequest = fmap histRequest . mostRecentHistory

-- |Set a temporary list of custom 'RequestHeaders' to use within the given action.
-- All previous custom headers are temporarily removed, and then restored at the end.
withRequestHeaders :: WDSessionStateControl m => RequestHeaders -> m a -> m a
withRequestHeaders h m = do
  h' <- fmap wdSessRequestHeaders getSession
  modifySession $ \s -> s { wdSessRequestHeaders = h }
  (a :: Either SomeException a) <- try m
  modifySession $ \s -> s { wdSessRequestHeaders = h' }
  either throwIO return a

-- |Makes all webdriver HTTP requests in the given action use the session\'s auth headers, typically
-- configured by setting the 'wdAuthHeaders' config. This is useful if you want to temporarily use
-- the same auth headers you used for session creation with other HTTP requests.
withAuthHeaders :: WDSessionStateControl m => m a -> m a
withAuthHeaders wd = do
  authHeaders <- fmap wdSessAuthHeaders getSession
  withRequestHeaders authHeaders wd
                            
instance WDSessionState m => WDSessionState (LS.StateT s m) where
  getSession = lift getSession
  putSession = lift . putSession
  
instance WDSessionState m => WDSessionState (SS.StateT s m) where
  getSession = lift getSession
  putSession = lift . putSession
    
instance WDSessionState m => WDSessionState (MaybeT m) where
  getSession = lift getSession
  putSession = lift . putSession

instance WDSessionState m => WDSessionState (IdentityT m) where
  getSession = lift getSession
  putSession = lift . putSession

instance WDSessionState m => WDSessionState (ListT m) where
  getSession = lift getSession
  putSession = lift . putSession
  
instance (Monoid w, WDSessionState m) => WDSessionState (LW.WriterT w m) where
  getSession = lift getSession
  putSession = lift . putSession

instance (Monoid w, WDSessionState m) => WDSessionState (SW.WriterT w m) where
  getSession = lift getSession
  putSession = lift . putSession
  
instance WDSessionState m => WDSessionState (ReaderT r m) where
  getSession = lift getSession
  putSession = lift . putSession
  
instance (Error e, WDSessionState m) => WDSessionState (ErrorT e m) where
  getSession = lift getSession
  putSession = lift . putSession

instance WDSessionState m => WDSessionState (ExceptT r m) where
  getSession = lift getSession
  putSession = lift . putSession

instance (Monoid w, WDSessionState m) => WDSessionState (SRWS.RWST r w s m) where
  getSession = lift getSession
  putSession = lift . putSession
  
instance (Monoid w, WDSessionState wd) => WDSessionState (LRWS.RWST r w s wd) where
  getSession = lift getSession
  putSession = lift . putSession
