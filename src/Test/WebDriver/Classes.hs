{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts,
             GeneralizedNewtypeDeriving #-}
module Test.WebDriver.Classes
       ( -- * WebDriver class
         WebDriver(..), RequestMethod(..),
         -- * SessionState class
         SessionState(..), modifySession
         -- ** WebDriver sessions
       , WDSession(..), SessionId(..), defaultSession
       ) where

--import Test.WebDriver.Internal
import Data.Aeson
import Network.HTTP (RequestMethod(..))
import Network.HTTP.Base (Request)
import Data.ByteString.Lazy (ByteString)

import Data.Text (Text)

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
                              -- anything that converts to Data.Aeson.Null will 
                              -- result in an empty request body.
                -> wd b       -- ^The JSON result of the HTTP request.

modifySession :: SessionState s => (WDSession -> WDSession) -> s ()
modifySession f = getSession >>= putSession . f

{- |Information about a WebDriver session. This structure is passed
implicitly through all 'WD' computations, and is also used to configure the 'WD'
monad before execution. -}
data WDSession = WDSession {
                             -- |Host name of the WebDriver server for this
                             -- session (default 127.0.0.1)
                             wdHost     :: String
                             -- |Port number of the server (default 4444)
                           , wdPort     :: Word16
                             -- |Base path (default "/wd/hub")
                           , wdBasePath :: String
                             -- |An opaque reference identifying the session to
                             -- use with 'WD' commands.
                             -- A value of Nothing indicates that a session
                             -- hasn't been created yet.
                             -- Sessions can be created within 'WD' via
                             -- 'Test.WebDriver.createSession', or created
                             -- and closed automatically with
                             -- 'Test.WebDriver.runSession'
                           , wdSessId   :: Maybe SessionId
                             -- |The last HTTP request issued by this session. 
                           , lastHTTPRequest :: Maybe (Request ByteString)
                           } deriving (Show)

instance Default WDSession where
  def = WDSession { wdHost          = "127.0.0.1"
                  , wdPort          = 4444
                  , wdBasePath      = "/wd/hub"
                  , wdSessId        = Nothing
                  , lastHTTPRequest = Nothing
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
