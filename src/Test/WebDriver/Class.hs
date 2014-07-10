{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts,
             GeneralizedNewtypeDeriving, RecordWildCards #-}
module Test.WebDriver.Class
       ( -- * WebDriver class
         WebDriver(..), Method, methodDelete, methodGet, methodPost,
       ) where
import Test.WebDriver.Session
import Test.WebDriver.Config 

import Data.Aeson
import Data.Text (Text)

import Network.HTTP.Types.Method (methodDelete, methodGet, methodPost, Method)

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



  
  -- |A class for monads that can handle wire protocol requests. This is the
-- operation underlying all of the high-level commands exported in
-- "Test.WebDriver.Commands". For more information on the wire protocol see
-- <http://code.google.com/p/selenium/wiki/JsonWireProtocol>
class (WDSessionState wd, WDConfigReader wd) => WebDriver wd where
  doCommand :: (ToJSON a, FromJSON b) =>
                Method -- ^HTTP request method
                -> Text       -- ^URL of request
                -> a          -- ^JSON parameters passed in the body
                              -- of the request. Note that, as a special case,
                              -- anything that converts to Data.Aeson.Null will
                              -- result in an empty request body.
                -> wd b       -- ^The JSON result of the HTTP request.

instance WebDriver wd => WebDriver (SS.StateT s wd) where
  doCommand rm t a = lift (doCommand rm t a)
  
instance WebDriver wd => WebDriver (LS.StateT s wd) where
  doCommand rm t a = lift (doCommand rm t a)


instance WebDriver wd => WebDriver (MaybeT wd) where
  doCommand rm t a = lift (doCommand rm t a)


instance WebDriver wd => WebDriver (IdentityT wd) where
  doCommand rm t a = lift (doCommand rm t a)


instance (Monoid w, WebDriver wd) => WebDriver (LW.WriterT w wd) where
  doCommand rm t a = lift (doCommand rm t a)


instance WebDriver wd => WebDriver (ReaderT r wd) where
  doCommand rm t a = lift (doCommand rm t a)

instance (Error e, WebDriver wd) => WebDriver (ErrorT e wd) where
  doCommand rm t a = lift (doCommand rm t a)


instance (Monoid w, WebDriver wd) => WebDriver (SRWS.RWST r w s wd) where
  doCommand rm t a = lift (doCommand rm t a)

instance (Monoid w, WebDriver wd) => WebDriver (LRWS.RWST r w s wd) where
  doCommand rm t a = lift (doCommand rm t a)
