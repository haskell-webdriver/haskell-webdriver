{-# LANGUAGE DeriveGeneric #-}

module Test.WebDriver.Cookies where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Char as C
import Data.Text (Text)
import GHC.Generics
import Test.WebDriver.JSON

-- | Cookies are delicious delicacies. When sending cookies to the server, a value
-- of Nothing indicates that the server should use a default value. When receiving
-- cookies from the server, a value of Nothing indicates that the server is unable
-- to specify the value.
data Cookie = Cookie {
  cookName   :: Text
  , cookValue  :: Text          -- ^
  , cookPath   :: Maybe Text    -- ^path of this cookie.
                                -- if Nothing, defaults to /
  , cookDomain :: Maybe Text    -- ^domain of this cookie.
                                -- if Nothing, the current pages
                                -- domain is used
  , cookSecure :: Maybe Bool    -- ^Is this cookie secure?
  , cookExpiry :: Maybe Double  -- ^Expiry date expressed as
                                -- seconds since the Unix epoch
                                -- Nothing indicates that the
                                -- cookie never expires
  } deriving (Eq, Show, Generic)

aesonOptionsCookie :: Options
aesonOptionsCookie = defaultOptions {
  omitNothingFields = True
  , fieldLabelModifier = map C.toLower . drop 4
  }

-- |Creates a Cookie with only a name and value specified. All other
-- fields are set to Nothing, which tells the server to use default values.
mkCookie :: Text -> Text -> Cookie
mkCookie name value = Cookie { cookName = name, cookValue = value,
                               cookPath = Nothing, cookDomain = Nothing,
                               cookSecure = Nothing, cookExpiry = Nothing
                             }

instance ToJSON Cookie where
  toJSON = genericToJSON aesonOptionsCookie
  toEncoding = genericToEncoding aesonOptionsCookie
instance FromJSON Cookie where
  parseJSON (Object o) = Cookie <$> req "name"
                                <*> req "value"
                                <*> opt "path" Nothing
                                <*> opt "domain" Nothing
                                <*> opt "secure" Nothing
                                <*> opt "expiry" Nothing
    where
      req :: FromJSON a => Text -> Parser a
      req = (o .:) . fromText
      opt :: FromJSON a => Text -> a -> Parser a
      opt k d = o .:?? k .!= d
  parseJSON v = typeMismatch "Cookie" v
