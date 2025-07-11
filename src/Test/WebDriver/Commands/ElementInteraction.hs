
module Test.WebDriver.Commands.ElementInteraction (
  click
  , clearInput
  , sendKeys
  -- , sendRawKeys
  ) where

import Data.Text (Text)
import GHC.Stack
import Test.WebDriver.JSON (noObject, noReturn, single)
import Test.WebDriver.Types
import Test.WebDriver.Util.Commands


-- | Click on an element.
click :: (HasCallStack, WebDriver wd) => Element -> wd ()
click e = noReturn $ doElemCommand methodPost e "/click" noObject

-- | Clear a textarea or text input element's value.
clearInput :: (HasCallStack, WebDriver wd) => Element -> wd ()
clearInput e = noReturn $ doElemCommand methodPost e "/clear" noObject

-- | Send a sequence of keystrokes to an element. All modifier keys are released
-- at the end of the function. Named constants for special modifier keys can be found
-- in "Test.WebDriver.Keys"
sendKeys :: (HasCallStack, WebDriver wd) => Text -> Element -> wd ()
sendKeys t e = noReturn . doElemCommand methodPost e "/value" . single "text" $ t

-- -- | Similar to sendKeys, but doesn't implicitly release modifier keys
-- -- afterwards. This allows you to combine modifiers with mouse clicks.
-- sendRawKeys :: (HasCallStack, WebDriver wd) => Text -> wd ()
-- sendRawKeys t = noReturn . doSessCommand methodPost "/keys" . single "text" $ t
