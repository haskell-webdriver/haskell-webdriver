{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving #-}
module Test.WebDriver.Classes
       ( WebDriver(..), RequestMethod(..),
         SessionState(..), modifySession, doSessCommand
       , WDSession(..), SessionId(..), defaultSession
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

import Data.Default
import Data.Word


class MonadBaseControl IO s => SessionState s where
  getSession :: s WDSession
  putSession :: WDSession -> s ()

class SessionState wd => WebDriver wd where
  doCommand :: (ToJSON a, FromJSON b) => 
                RequestMethod -> Text -> a -> wd b 


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
