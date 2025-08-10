{-# LANGUAGE OverloadedStrings #-}

module Test.WebDriver.Commands.Logs.Selenium (
  getSeleniumLogs
  , getSeleniumLogTypes
  ) where

import Data.Aeson
import GHC.Stack
import Test.WebDriver.Commands.Logs.Common
import Test.WebDriver.Types
import Test.WebDriver.Util.Commands


-- | Get logs from Selenium using the legacy /log endpoint
getSeleniumLogs :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getSeleniumLogs logType = doSessCommand methodPost "/log" $ object ["type" .= logType]

-- | Get a list of available log types.
getSeleniumLogTypes :: (HasCallStack, WebDriver wd) => wd [LogType]
getSeleniumLogTypes = doSessCommand methodGet "/log/types" Null
