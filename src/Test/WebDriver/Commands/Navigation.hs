
module Test.WebDriver.Commands.Navigation (
  openPage
  , getCurrentURL
  , back
  , forward
  , refresh
  , getTitle
  ) where

import Data.Aeson as A
import Data.CallStack
import Data.Text (Text)
import Network.URI hiding (path)  -- suppresses warnings
import Prelude -- hides some "unused import" warnings
import Test.WebDriver.Exceptions
import Test.WebDriver.JSON
import Test.WebDriver.Types
import Test.WebDriver.Util.Commands
import UnliftIO.Exception (throwIO)


-- | Opens a new page by the given URL.
openPage :: (HasCallStack, WebDriver wd) => String -> wd ()
openPage url
  | isURI url = noReturn . doSessCommand methodPost "/url" . single "url" $ url
  | otherwise = throwIO . InvalidURL $ url

-- | Gets the URL of the current page.
getCurrentURL :: (HasCallStack, WebDriver wd) => wd String
getCurrentURL = doSessCommand methodGet "/url" Null

-- | Navigate backward in the browser history.
back :: (HasCallStack, WebDriver wd) => wd ()
back = noReturn $ doSessCommand methodPost "/back" noObject

-- | Navigate forward in the browser history.
forward :: (HasCallStack, WebDriver wd) => wd ()
forward = noReturn $ doSessCommand methodPost "/forward" noObject

-- | Refresh the current page
refresh :: (HasCallStack, WebDriver wd) => wd ()
refresh = noReturn $ doSessCommand methodPost "/refresh" noObject

-- | Get the title of the current page.
getTitle :: (HasCallStack, WebDriver wd) => wd Text
getTitle = doSessCommand methodGet "/title" Null
