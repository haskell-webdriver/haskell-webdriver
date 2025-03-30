
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
import Prelude -- hides some "unused import" warnings
import Test.WebDriver.Class
import Test.WebDriver.Commands.Internal
import Test.WebDriver.JSON


availableIMEEngines :: (HasCallStack, WebDriver wd) => wd [Text]
availableIMEEngines = doSessCommand methodGet "/ime/available_engines" Null

activeIMEEngine :: (HasCallStack, WebDriver wd) => wd Text
activeIMEEngine = doSessCommand methodGet "/ime/active_engine" Null

checkIMEActive :: (HasCallStack, WebDriver wd) => wd Bool
checkIMEActive = doSessCommand methodGet "/ime/activated" Null

activateIME :: (HasCallStack, WebDriver wd) => Text -> wd ()
activateIME = noReturn . doSessCommand methodPost "/ime/activate" . single "engine"

deactivateIME :: (HasCallStack, WebDriver wd) => wd ()
deactivateIME = noReturn $ doSessCommand methodPost "/ime/deactivate" Null
