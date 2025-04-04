
module Test.WebDriver.Exceptions (
  InvalidURL(..)
  , NoSessionId(..)
  , BadJSON(..)

  , HTTPStatusUnknown(..)
  , HTTPConnError(..)

  , UnknownCommand(..)
  , ServerError(..)

  , FailedCommand(..)
  , FailedCommandType(..)

  , FailedCommandInfo(..)
  , StackFrame(..)

  , mkFailedCommandInfo
  , failedCommand
  ) where

import Test.WebDriver.CommandUtil
import Test.WebDriver.Exceptions.Internal
import Test.WebDriver.JSON
