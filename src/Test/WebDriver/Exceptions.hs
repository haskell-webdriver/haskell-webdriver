module Test.WebDriver.Exceptions
       ( InvalidURL(..), NoSessionId(..), BadJSON(..)
       , HTTPStatusUnknown(..), HTTPConnError(..)
       , UnknownCommand(..), ServerError(..)
       , FailedCommand(..), FailedCommandType(..)
       , FailedCommandInfo(..), StackFrame(..)
       , mkFailedCommandInfo, failedCommand
       ) where
import Test.WebDriver.JSON
import Test.WebDriver.Commands.Internal
import Test.WebDriver.Exceptions.Internal