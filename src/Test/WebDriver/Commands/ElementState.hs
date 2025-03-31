
module Test.WebDriver.Commands.ElementState (
  isSelected
  , attr
  , prop
  , cssProp
  , getText
  , tagName
  , elemRect
  , isEnabled
  ) where

import Data.Aeson as A
import Data.CallStack
import Data.Text (Text, append)
import Test.WebDriver.Class
import Test.WebDriver.Commands.CommandContexts
import Test.WebDriver.Commands.ElementRetrieval
import Test.WebDriver.CommandUtil
import Test.WebDriver.Utils (urlEncode)


-- | Determine if the element is selected.
isSelected :: (HasCallStack, WebDriver wd) => Element -> wd Bool
isSelected e = doElemCommand methodGet e "/selected" Null

-- | Retrieve the value of an element's attribute
attr :: (HasCallStack, WebDriver wd) => Element -> Text -> wd (Maybe Text)
attr e t = doElemCommand methodGet e ("/attribute/" `append` urlEncode t) Null

-- | Retrieve the value of an element's property
prop :: (HasCallStack, WebDriver wd) => Element -> Text -> wd (Maybe Value)
prop e t = doElemCommand methodGet e ("/property/" `append` urlEncode t) Null

-- | Retrieve the value of an element's computed CSS property
cssProp :: (HasCallStack, WebDriver wd) => Element -> Text -> wd (Maybe Text)
cssProp e t = doElemCommand methodGet e ("/css/" `append` urlEncode t) Null

-- | Get all visible text within this element.
getText :: (HasCallStack, WebDriver wd) => Element -> wd Text
getText e = doElemCommand methodGet e "/text" Null

-- | Return the tag name of the given element.
tagName :: (HasCallStack, WebDriver wd) => Element -> wd Text
tagName e = doElemCommand methodGet e "/name" Null

-- | Retrieve an element's current position.
elemRect :: (HasCallStack, WebDriver wd) => Element -> wd Rect
elemRect e = doElemCommand methodGet e "/rect" Null

-- | Determine if the element is enabled.
isEnabled :: (HasCallStack, WebDriver wd) => Element -> wd Bool
isEnabled e = doElemCommand methodGet e "/enabled" Null
