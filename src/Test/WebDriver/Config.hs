module Test.WebDriver.Config(
    -- * WDConfigReader class
      WDConfigReader(..)
    -- * WebDriver configuration
    , WDConfig(..), defaultConfig
    ) where

import Data.Word
import Data.Default

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

-- | WebDriver session configuration
data WDConfig = WDConfig {
     -- |Host name of the WebDriver server for this
     -- session (default 127.0.0.1)
     wdHost     :: String
     -- |Port number of the server (default 4444)
    , wdPort     :: Word16
     -- |Base path for all API requests (default "/wd/hub")
    , wdBasePath :: String
} deriving (Read, Show, Eq)

instance Default WDConfig where
    def = WDConfig {
      wdHost              = "127.0.0.1"
    , wdPort              = 4444
    , wdBasePath       = "/wd/hub"
    }
    
 
    
{- |A default session config connects to localhost on port 4444, and hasn't been
initialized server-side. This value is the same as 'def' but with a less
polymorphic type. -}
defaultConfig :: WDConfig
defaultConfig = def

class Monad m => WDConfigReader m where
    askConfig :: m WDConfig
    --localConfig :: (WDConfig -> WDConfig) -> m a -> m a

instance WDConfigReader m => WDConfigReader (LS.StateT s m) where
    askConfig = lift askConfig
  
instance WDConfigReader m => WDConfigReader (SS.StateT s m) where
    askConfig = lift askConfig
    
instance WDConfigReader m => WDConfigReader (MaybeT m) where
    askConfig = lift askConfig

instance WDConfigReader m => WDConfigReader (IdentityT m) where
    askConfig = lift askConfig
  
instance (Monoid w, WDConfigReader m) => WDConfigReader (LW.WriterT w m) where
    askConfig = lift askConfig
  
instance WDConfigReader m => WDConfigReader (ReaderT r m) where
    askConfig = lift askConfig
  
instance (Error e, WDConfigReader m) => WDConfigReader (ErrorT e m) where
    askConfig = lift askConfig
  
instance (Monoid w, WDConfigReader m) => WDConfigReader (SRWS.RWST r w s m) where
    askConfig = lift askConfig
  
instance (Monoid w, WDConfigReader wd) => WDConfigReader (LRWS.RWST r w s wd) where
    askConfig = lift askConfig
