{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Test.WebDriver.Classes
       ( WebDriver(..), RequestMethod(..),
         SessionState(..), modifySession
       , WDSession(..), SessionId(..), defaultSession
       , doSessCommand
         -- * No Session Exception
       , NoSessionId(..)
       ) where

--import Test.WebDriver.Internal
import Data.Aeson
import Network.HTTP (RequestMethod(..))

import qualified Data.Text as T
import Data.Text (Text)

import Control.Monad.Trans.Control
import Control.Exception.Lifted (Exception, throwIO)
import Data.Typeable

import Control.Monad.Trans.Maybe
import Control.Monad.List
import Control.Monad.Trans.Identity
import Control.Monad.Reader
import Control.Monad.Error
--import Control.Monad.Cont
import Control.Monad.Writer.Strict as SW
import Control.Monad.Writer.Lazy as LW
import Control.Monad.State.Strict as SS
import Control.Monad.State.Lazy as LS
import Control.Monad.RWS.Strict as SRWS
import Control.Monad.RWS.Lazy as LRWS

import Data.Default
import Data.Word


-- |A class for monads that carry a WebDriver session with them. The
-- MonadBaseControl superclass is used for exception handling through
-- the lifted-base package.
class MonadBaseControl IO s => SessionState s where
  getSession :: s WDSession
  putSession :: WDSession -> s ()

-- |A class for monads that can handle wire protocol requests. This is the
-- operation underlying all of the high-level commands exported in
-- "Test.WebDriver.Commands". For more information on the wire protocol see
-- <http://code.google.com/p/selenium/wiki/JsonWireProtocol>
class SessionState wd => WebDriver wd where
  doCommand :: (ToJSON a, FromJSON b) => 
                RequestMethod -- ^HTTP request method 
                -> Text       -- ^URL of request 
                -> a          -- ^JSON parameters passed in the body 
                              -- of the request. Note that, as a special case,
                              -- () will result in an empty request body.
                -> wd b 


modifySession :: SessionState s => (WDSession -> WDSession) -> s ()
modifySession f = getSession >>= putSession . f

{- |Information about a WebDriver session. This structure is passed
implicitly through all 'WD' computations, and is also used to configure the 'WD'
monad before execution. -}
data WDSession = WDSession { 
                             -- |Host name of the WebDriver server for this 
                             -- session
                             wdHost   :: String
                             -- |Port number of the server
                           , wdPort   :: Word16
                             -- |An opaque reference identifying the session to
                             -- use with 'WD' commands.
                             -- A value of Nothing indicates that a session 
                             -- hasn't been created yet.
                             -- Sessions can be created within 'WD' via
                             -- 'Test.WebDriver.createSession', or created
                             -- and closed automatically with 
                             -- 'Test.WebDriver.runSession'
                           , wdSessId :: Maybe SessionId 
                           } deriving (Eq, Show)

instance Default WDSession where
  def = WDSession { wdHost   = "127.0.0.1"
                  , wdPort   = 4444
                  , wdSessId = Nothing
                  }

{- |A default session connects to localhost on port 4444, and hasn't been 
initialized server-side. This value is the same as 'def' but with a less
polymorphic type. -}
defaultSession :: WDSession
defaultSession = def


{- |An opaque identifier for a WebDriver session. These handles are produced by 
the server on session creation, and act to identify a session in progress. -} 
newtype SessionId = SessionId Text
                  deriving (Eq, Ord, Show, Read, 
                            FromJSON, ToJSON)


instance Exception NoSessionId
-- |A command requiring a session ID was attempted when no session ID was 
-- available.
newtype NoSessionId = NoSessionId String 
                 deriving (Eq, Show, Typeable)


-- |This a convenient wrapper around doCommand that automatically prepends
-- the session URL parameter to the wire command URL. For example, passing
-- a URL of "/refresh" will expand to "/session/:sessionId/refresh".
doSessCommand :: (WebDriver wd, ToJSON a, FromJSON b) => 
                  RequestMethod -> Text -> a -> wd b
doSessCommand method path args = do
  WDSession { wdSessId = mSessId } <- getSession
  case mSessId of 
      Nothing -> throwIO . NoSessionId $ msg
        where 
          msg = "No session ID found when making request for relative URL "
                ++ show path
      Just (SessionId sId) -> doCommand method 
                              (T.concat ["/session/", sId, path]) args


instance SessionState m => SessionState (LS.StateT s m) where
  getSession = lift getSession
  putSession = lift . putSession

instance WebDriver wd => WebDriver (LS.StateT s wd) where
  doCommand rm t a = lift (doCommand rm t a)


instance SessionState m => SessionState (SS.StateT s m) where
  getSession = lift getSession
  putSession = lift . putSession

instance WebDriver wd => WebDriver (SS.StateT s wd) where
  doCommand rm t a = lift (doCommand rm t a)

instance SessionState m => SessionState (MaybeT m) where
  getSession = lift getSession
  putSession = lift . putSession

instance WebDriver wd => WebDriver (MaybeT wd) where
  doCommand rm t a = lift (doCommand rm t a)


instance SessionState m => SessionState (IdentityT m) where
  getSession = lift getSession
  putSession = lift . putSession

instance WebDriver wd => WebDriver (IdentityT wd) where
  doCommand rm t a = lift (doCommand rm t a)


instance (Monoid w, SessionState m) => SessionState (LW.WriterT w m) where
  getSession = lift getSession
  putSession = lift . putSession

instance (Monoid w, WebDriver wd) => WebDriver (LW.WriterT w wd) where
  doCommand rm t a = lift (doCommand rm t a)


instance SessionState m => SessionState (ReaderT r m) where
  getSession = lift getSession
  putSession = lift . putSession

instance WebDriver wd => WebDriver (ReaderT r wd) where
  doCommand rm t a = lift (doCommand rm t a)


instance (Error e, SessionState m) => SessionState (ErrorT e m) where
  getSession = lift getSession
  putSession = lift . putSession

instance (Error e, WebDriver wd) => WebDriver (ErrorT e wd) where
  doCommand rm t a = lift (doCommand rm t a)


--instance SessionState m => SessionState (ContT r m) where
--  getSession = lift getSession
--  putSession = lift . putSession

--instance WebDriver wd => WebDriver (ContT r wd) where
--  doCommand rm t a = lift (doCommand rm t a)


instance (Monoid w, SessionState m) => SessionState (SRWS.RWST r w s m) where
  getSession = lift getSession
  putSession = lift . putSession

instance (Monoid w, WebDriver wd) => WebDriver (SRWS.RWST r w s wd) where
  doCommand rm t a = lift (doCommand rm t a)


instance (Monoid w, SessionState m) => SessionState (LRWS.RWST r w s m) where
  getSession = lift getSession
  putSession = lift . putSession

instance (Monoid w, WebDriver wd) => WebDriver (LRWS.RWST r w s wd) where
  doCommand rm t a = lift (doCommand rm t a)

