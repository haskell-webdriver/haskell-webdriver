{-# LANGUAGE DeriveGeneric #-}

module Test.WebDriver.Commands.Cookies (
  cookies
  , cookie
  , setCookie
  , deleteCookie
  , deleteCookies

  -- * Types
  , mkCookie
  , Cookie(..)
  ) where

import Data.Aeson as A
import Data.Aeson.Types
import Data.CallStack
import qualified Data.Char as C
import Data.Text (Text)
import GHC.Generics
import Test.WebDriver.Monad
import Test.WebDriver.CommandUtil
import Test.WebDriver.JSON


-- | Retrieve all cookies.
cookies :: (HasCallStack, WebDriver wd) => wd [Cookie]
cookies = doSessCommand methodGet "/cookie" Null

-- | Retrieve a specific cookie by name.
cookie :: (HasCallStack, WebDriver wd) => Text -> wd Cookie
cookie n = doSessCommand methodGet ("/cookie/" <> n) Null

-- | Set a cookie. If the cookie path is not specified, it will default to \"/\".
-- Likewise, if the domain is omitted, it will default to the current page's domain.
setCookie :: (HasCallStack, WebDriver wd) => Cookie -> wd ()
setCookie = noReturn . doSessCommand methodPost "/cookie" . single "cookie"

-- | Delete a cookie by name.
deleteCookie :: (HasCallStack, WebDriver wd) => Text -> wd ()
deleteCookie n = noReturn $ doSessCommand methodDelete ("/cookie/" <> urlEncode n) Null

-- | Delete all visible cookies on the current page.
deleteCookies :: (HasCallStack, WebDriver wd) => wd ()
deleteCookies = noReturn $ doSessCommand methodDelete "/cookie" Null

-- * Types

-- | Cookies are delicious delicacies. When sending cookies to the server, a value
-- of Nothing indicates that the server should use a default value. When receiving
-- cookies from the server, a value of Nothing indicates that the server is unable
-- to specify the value.
data Cookie = Cookie {
  cookName   :: Text
  , cookValue  :: Text
  -- | Path of this cookie. If Nothing, defaults to /
  , cookPath   :: Maybe Text
  -- | Domain of this cookie. If Nothing, the current page's domain is used.
  , cookDomain :: Maybe Text
  -- | Is this cookie secure?
  , cookSecure :: Maybe Bool
  -- | Expiry date expressed as seconds since the Unix epoch.
  -- 'Nothing' indicates that the cookie never expires.
  , cookExpiry :: Maybe Double
  } deriving (Eq, Show, Generic)

aesonOptionsCookie :: Options
aesonOptionsCookie = defaultOptions {
  omitNothingFields = True
  , fieldLabelModifier = map C.toLower . drop 4
  }

-- | Creates a Cookie with only a name and value specified. All other
-- fields are set to Nothing, which tells the server to use default values.
mkCookie :: Text -> Text -> Cookie
mkCookie name value = Cookie {
  cookName = name
  , cookValue = value
  , cookPath = Nothing
  , cookDomain = Nothing
  , cookSecure = Nothing
  , cookExpiry = Nothing
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
