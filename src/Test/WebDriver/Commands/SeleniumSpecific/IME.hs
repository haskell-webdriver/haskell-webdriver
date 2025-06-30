
module Test.WebDriver.Commands.SeleniumSpecific.IME (
  availableIMEEngines
  , activeIMEEngine
  , checkIMEActive
  , activateIME
  , deactivateIME
  ) where

import Data.Aeson as A
import Data.CallStack
import Data.Text (Text)
import Test.WebDriver.Class
import Test.WebDriver.CommandUtil
import Test.WebDriver.JSON


-- | Get a list of all available Input Method Editor (IME) engines installed on
-- the system. IME engines are used for typing in languages that require complex
-- input methods (like Chinese, Japanese, Korean). Returns a list of engine
-- identifiers.
availableIMEEngines :: (HasCallStack, WebDriver wd) => wd [Text]
availableIMEEngines = doSessCommand methodGet "/ime/available_engines" Null

-- | Get the identifier of the currently active IME engine. Returns an empty
-- string if no IME is active.
activeIMEEngine :: (HasCallStack, WebDriver wd) => wd Text
activeIMEEngine = doSessCommand methodGet "/ime/active_engine" Null

-- | Check whether an IME engine is currently activated.
checkIMEActive :: (HasCallStack, WebDriver wd) => wd Bool
checkIMEActive = doSessCommand methodGet "/ime/activated" Null

-- | Activate a specific IME engine by its identifier.
activateIME :: (HasCallStack, WebDriver wd) => Text -> wd ()
activateIME = noReturn . doSessCommand methodPost "/ime/activate" . single "engine"

-- | Deactivate the currently active IME engine.
deactivateIME :: (HasCallStack, WebDriver wd) => wd ()
deactivateIME = noReturn $ doSessCommand methodPost "/ime/deactivate" Null
