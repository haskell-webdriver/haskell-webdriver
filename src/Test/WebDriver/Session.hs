{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts,
             GeneralizedNewtypeDeriving, RecordWildCards #-}
module Test.WebDriver.Session(
         -- * WDSessionState class
         WDSessionState(..), modifySession
         -- ** WebDriver sessions
       , WDSession(..), lastHTTPRequest, SessionId(..)
    ) where

import Data.Aeson
import Data.ByteString as BS(ByteString) 
import Data.ByteString.Lazy as LBS(ByteString)
import Data.Text (Text)
import Data.Maybe

import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.Error
--import Control.Monad.Cont
import Control.Monad.Writer.Strict as SW
import Control.Monad.Writer.Lazy as LW
import Control.Monad.State.Strict as SS
import Control.Monad.State.Lazy as LS
import Control.Monad.RWS.Strict as SRWS
import Control.Monad.RWS.Lazy as LRWS

import Network.HTTP.Client (Manager, Request, Response)

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
                           , wdSessHist :: [(Request, Response LBS.ByteString)]
                             -- |Update function used to append new entries to session history
                           , wdSessHistUpdate :: (Request, Response LBS.ByteString)
                                                 -> [(Request, Response LBS.ByteString)]
                                                 -> [(Request, Response LBS.ByteString)]
                             -- |HTTP 'Manager' used for connection pooling by the http-client library.
                           , wdSessHTTPManager :: Manager
                           }
    
-- |The last HTTP request issued by this session, if any.
lastHTTPRequest :: WDSession -> Maybe Request
lastHTTPRequest = fmap fst . listToMaybe . wdSessHist


-- |A class for monads that carry a WebDriver session with them. The
-- MonadBaseControl superclass is used for exception handling through
-- the lifted-base package.
class MonadBaseControl IO s => WDSessionState s where
  getSession :: s WDSession
  putSession :: WDSession -> s ()

modifySession :: WDSessionState s => (WDSession -> WDSession) -> s ()
modifySession f = getSession >>= putSession . f
                            
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
  
instance (Monoid w, WDSessionState m) => WDSessionState (LW.WriterT w m) where
  getSession = lift getSession
  putSession = lift . putSession
  
instance WDSessionState m => WDSessionState (ReaderT r m) where
  getSession = lift getSession
  putSession = lift . putSession
  
instance (Error e, WDSessionState m) => WDSessionState (ErrorT e m) where
  getSession = lift getSession
  putSession = lift . putSession
  
instance (Monoid w, WDSessionState m) => WDSessionState (SRWS.RWST r w s m) where
  getSession = lift getSession
  putSession = lift . putSession
  
instance (Monoid w, WDSessionState wd) => WDSessionState (LRWS.RWST r w s wd) where
  getSession = lift getSession
  putSession = lift . putSession
  
