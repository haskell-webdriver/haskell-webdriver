
module Test.WebDriver.Commands.SeleniumSpecific.Misc (
  -- * Interacting with elements
  submit
  , isDisplayed

  -- * Element equality
  , (<==>)
  , (</=>)

  -- * Server information and logs
  , getLogs
  , getLogTypes
  , LogType
  , LogEntry(..)
  , LogLevel(..)
  ) where

import Control.Applicative
import Data.Aeson as A
import Data.CallStack
import Data.Text (append)
import Prelude -- hides some "unused import" warnings
import Test.WebDriver.Class
import Test.WebDriver.Commands.Internal
import Test.WebDriver.Commands.LoggingTypes
import Test.WebDriver.JSON
import Test.WebDriver.Utils (urlEncode)


-- | Submit a form element. This may be applied to descendents of a form element
-- as well.
submit :: (HasCallStack, WebDriver wd) => Element -> wd ()
submit e = noReturn $ doElemCommand methodPost e "/submit" Null

-- | Determine if the element is displayed.
-- This function isn't guaranteed to be implemented by the WebDriver spec,
-- but it is found in Selenium.
-- See https://www.w3.org/TR/webdriver1/#element-displayedness.
isDisplayed :: (HasCallStack, WebDriver wd) => Element -> wd Bool
isDisplayed e = doElemCommand methodGet e "/displayed" Null

infix 4 <==>
-- | Determines if two element identifiers refer to the same element.
(<==>) :: (HasCallStack, WebDriver wd) => Element -> Element -> wd Bool
e1 <==> (Element e2) = doElemCommand methodGet e1 ("/equals/" `append` urlEncode e2) Null

-- | Determines if two element identifiers refer to different elements.
infix 4 </=>
(</=>) :: (HasCallStack, WebDriver wd) => Element -> Element -> wd Bool
e1 </=> e2 = not <$> (e1 <==> e2)

-- | Retrieve the log buffer for a given log type. The server-side log buffer is reset after each request.
--
-- Which log types are available is server defined, but the wire protocol lists these as common log types:
-- client, driver, browser, server
getLogs :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getLogs t = doSessCommand methodPost "/log" . object $ ["type" .= t]

-- | Get a list of available log types.
getLogTypes :: (HasCallStack, WebDriver wd) => wd [LogType]
getLogTypes = doSessCommand methodGet "/log/types" Null
