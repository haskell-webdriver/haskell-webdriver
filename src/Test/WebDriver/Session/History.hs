module Test.WebDriver.Session.History where

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (Request, Response)
import Control.Exception (SomeException)


data SessionHistory = SessionHistory 
    { histRequest :: Request
    , histResponse :: Either SomeException (Response ByteString)
    , histRetryCount :: Int
    }
    deriving (Show)
