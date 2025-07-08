
module Test.WebDriver.Commands.UserPrompts (
  dismissAlert
  , acceptAlert
  , getAlertText
  , replyToAlert
  ) where

import Data.Aeson as A
import Data.Text (Text)
import GHC.Stack
import Test.WebDriver.JSON
import Test.WebDriver.Types
import Test.WebDriver.Util.Commands


-- | Dismisses the currently displayed alert dialog.
dismissAlert :: (HasCallStack, WebDriver wd) => wd ()
dismissAlert = noReturn $ doSessCommand methodPost "/alert/dismiss" noObject

-- | Accepts the currently displayed alert dialog.
acceptAlert :: (HasCallStack, WebDriver wd) => wd ()
acceptAlert = noReturn $ doSessCommand methodPost "/alert/accept" noObject

-- | Get the text of an alert dialog.
getAlertText :: (HasCallStack, WebDriver wd) => wd Text
getAlertText = doSessCommand methodGet "/alert/text" Null

-- | Sends keystrokes to Javascript prompt() dialog.
replyToAlert :: (HasCallStack, WebDriver wd) => Text -> wd ()
replyToAlert = noReturn . doSessCommand methodPost "/alert/text" . single "text"
