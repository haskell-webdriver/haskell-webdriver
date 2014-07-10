{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts,
             GeneralizedNewtypeDeriving, RecordWildCards #-}
module Test.WebDriver.Session(
         -- * WDSessionState class
         WDSessionState(..), modifySession
         -- ** WebDriver sessions
       , WDSession(..), lastHTTPRequest, SessionId(..), defaultSession
       , getManager, setManager
    ) where
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Default
import Data.Maybe

import Network.HTTP.Client (Request, Response, newManager, defaultManagerSettings)

import Control.Monad.Trans.Control
import Control.Monad.Base
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

import Network.HTTP.Client (Manager)

{- |An opaque identifier for a WebDriver session. These handles are produced by
the server on session creation, and act to identify a session in progress. -}
newtype SessionId = SessionId Text
                  deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

{- |The local state of a WebDriver session. This structure is passed
implicitly through all 'WD' computations, and is also used to configure the 'WD'
monad before execution. -}
data WDSession = WDSession {
                             -- |An opaque reference identifying the session to
                             -- use with 'WD' commands.
                             -- A value of Nothing indicates that a session
                             -- hasn't been created yet.
                             -- Sessions can be created within 'WD' via
                             -- 'Test.WebDriver.createSession', or created
                             -- and closed automatically with
                             -- 'Test.WebDriver.runSession'
                             wdSessId   :: Maybe SessionId
                             -- |The complete history of HTTP requests and
                             -- responses (updated in 'doCommand', most recent
                             -- first).
                           , wdSessHist :: [(Request, Response ByteString)]
                             -- |If 'wdKeepSessHist' is 'True', 'wdSessHist'
                             -- contains the full session history.
                             -- Otherwise, only the last request/response
                             -- pair is stored (O(1) heap consumption).
                           , wdKeepSessHist :: Bool

                             -- |HTTP 'Manager' used for connection pooling by the http-client library.
                           , wdHTTPManager :: Maybe Manager
                           } -- deriving (Show)
                           
instance Default WDSession where
  def = WDSession {
    wdSessId        = Nothing
  , wdSessHist      = []
  , wdKeepSessHist  = False
  , wdHTTPManager   = Nothing
  }

{- |A default session state that hasn't been initialized server-side connects to localhost on port 4444 -}
defaultSession :: WDSession
defaultSession = def

instance Show WDSession where
    show (WDSession {..}) = "{ wdSessId = " ++ show wdSessId ++ ", wdSessHist = " ++ show wdSessHist ++ ", wdKeepSessHist = " ++ show wdKeepSessHist ++ ", wdHttpManager = " ++ (if isNothing wdHTTPManager then "Nothing" else "Just Manager") ++ "}"

                  
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

-- |Gets the HTTP 'Manager' for this session. If the manager is uninitialized, a default manager is created
getManager :: WDSessionState s => s Manager
getManager = do
    s <- getSession
    maybe (createManager s) return . wdHTTPManager $ s
    where createManager s = do
              m <- liftBase $ newManager defaultManagerSettings
              putSession s { wdHTTPManager = Just m }
              return m
              
-- |Use the given HTTP 'Manager' for subsequent webdriver requests
setManager :: WDSessionState s => Manager -> s ()
setManager m = modifySession (\s-> s {wdHTTPManager = Just m})
                            
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
  
