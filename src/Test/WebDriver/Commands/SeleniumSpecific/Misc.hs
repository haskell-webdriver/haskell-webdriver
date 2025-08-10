
module Test.WebDriver.Commands.SeleniumSpecific.Misc (
  -- * Interacting with elements
  submit
  , isDisplayed

  -- * Element equality
  , (<==>)
  , (</=>)
  ) where

import Data.Aeson as A
import GHC.Stack
import Test.WebDriver.JSON
import Test.WebDriver.Types
import Test.WebDriver.Util.Commands


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
e1 <==> (Element e2) = doElemCommand methodGet e1 ("/equals/" <> urlEncode e2) Null

-- | Determines if two element identifiers refer to different elements.
infix 4 </=>
(</=>) :: (HasCallStack, WebDriver wd) => Element -> Element -> wd Bool
e1 </=> e2 = not <$> (e1 <==> e2)
