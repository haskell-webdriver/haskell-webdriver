
module Test.WebDriver.Exceptions (
  InvalidURL(..)
  , NoSessionId(..)
  , BadJSON(..)

  , HTTPStatusUnknown(..)

  , UnknownCommand(..)
  , ServerError(..)

  , FailedCommand(..)
  , FailedCommandType(..)

  , FailedCommandInfo(..)
  , StackFrame(..)

  , mkFailedCommandInfo
  , failedCommand
  ) where

import Test.WebDriver.Exceptions.Internal
import Test.WebDriver.JSON
import Test.WebDriver.Util.Commands
