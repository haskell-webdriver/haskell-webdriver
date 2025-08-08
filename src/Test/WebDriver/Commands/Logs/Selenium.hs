{-# LANGUAGE OverloadedStrings #-}

module Test.WebDriver.Commands.Logs.Selenium (
  getSeleniumConsoleLogs
  , getSeleniumLogs
  ) where

import Data.Aeson
import GHC.Stack
import Test.WebDriver.Commands.Logs.Common
import Test.WebDriver.Types
import Test.WebDriver.Util.Commands

-- | Get console logs from Selenium using legacy log endpoint
getSeleniumConsoleLogs :: (HasCallStack, WebDriver wd) => wd [LogEntry]
getSeleniumConsoleLogs = getSeleniumLogs "browser"

-- | Get logs from Selenium using the legacy /log endpoint
-- This uses the same endpoint as SeleniumSpecific.Misc.getLogs
-- but is integrated into our modular logging system
getSeleniumLogs :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getSeleniumLogs logType = doSessCommand methodPost "/log" $ object ["type" .= logType]