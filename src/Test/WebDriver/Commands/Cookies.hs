
module Test.WebDriver.Commands.Cookies (
  cookies
  , cookie
  , setCookie
  , deleteCookies
  , deleteCookie
  , deleteCookieByName

  -- * Types
  , mkCookie
  , Cookie(..)
  ) where

import Data.Aeson as A
import Data.CallStack
import Data.Text (Text)
import Prelude -- hides some "unused import" warnings
import Test.WebDriver.Class
import Test.WebDriver.Commands.Internal
import Test.WebDriver.Cookies
import Test.WebDriver.JSON
import Test.WebDriver.Utils (urlEncode)


-- | Retrieve all cookies.
cookies :: (HasCallStack, WebDriver wd) => wd [Cookie]
cookies = doSessCommand methodGet "/cookie" Null

-- | Retrieve a specific cookie by name.
cookie :: (HasCallStack, WebDriver wd) => Text -> wd [Cookie]
cookie n = doSessCommand methodGet ("/cookie/" <> n) Null

-- | Set a cookie. If the cookie path is not specified, it will default to \"/\".
-- Likewise, if the domain is omitted, it will default to the current page's domain.
setCookie :: (HasCallStack, WebDriver wd) => Cookie -> wd ()
setCookie = noReturn . doSessCommand methodPost "/cookie" . single "cookie"

-- | Delete a cookie.
deleteCookie :: (HasCallStack, WebDriver wd) => Cookie -> wd ()
deleteCookie c = noReturn $ doSessCommand methodDelete ("/cookie/" <> urlEncode (cookName c)) Null

deleteCookieByName :: (HasCallStack, WebDriver wd) => Text -> wd ()
deleteCookieByName n = noReturn $ doSessCommand methodDelete ("/cookie/" <> n) Null

-- | Delete all visible cookies on the current page.
deleteCookies :: (HasCallStack, WebDriver wd) => wd ()
deleteCookies = noReturn $ doSessCommand methodDelete "/cookie" Null
