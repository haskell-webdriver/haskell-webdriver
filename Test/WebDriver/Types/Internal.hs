{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Test.WebDriver.Types.Internal
       ( WDResponse(..) 
       ) where
import Test.WebDriver.Types
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Word


data WDResponse = WDResponse { rspSessId :: Maybe SessionId
                             , rspStatus :: Word8
                             , rspVal    :: Value
                             }
                  deriving (Eq, Show)
                           
instance FromJSON WDResponse where
  parseJSON (Object o) = WDResponse <$> req "sessionId"
                                    <*> req "status"
                                    <*> opt "value" Null
    where req = (o .:)
          opt k d = o .: k .!= d
  parseJSON v = typeMismatch "WDResponse" v