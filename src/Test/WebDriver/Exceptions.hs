module Test.WebDriver.Exceptions 
       ( InvalidURL(..), NoSessionId(..), BadJSON(..)
       , HTTPStatusUnknown(..), HTTPConnError(..)
       , UnknownCommand(..), ServerError(..)
       , FailedCommand(..), FailedCommandType(..)
       , FailedCommandInfo(..), StackFrame(..)
       , mkFailedCommandInfo, failedCommand
       )where
import Test.WebDriver.Internal
import Test.WebDriver.Classes
import Test.WebDriver.JSON