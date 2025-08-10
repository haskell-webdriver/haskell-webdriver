module Test.WebDriver.Commands.Logs (
  -- * Main log retrieval functions
  getLogs
  , getConsoleLogs

  -- * Driver-specific implementations
  , getChromeLogs
  , getFirefoxLogs
  , getSeleniumLogs
  , getSeleniumLogTypes

  -- * Types
  , LogType
  , LogEntry(..)
  , LogLevel(..)
  ) where

import GHC.Stack
import Test.WebDriver.Commands.Logs.Chrome
import Test.WebDriver.Commands.Logs.Common
import Test.WebDriver.Commands.Logs.Firefox
import Test.WebDriver.Commands.Logs.Selenium
import Test.WebDriver.Types


-- | Retrieve console logs from the browser.
-- | This is a convenience function equivalent to @getLogs "browser"@
getConsoleLogs :: (HasCallStack, WebDriver wd) => wd [LogEntry]
getConsoleLogs = getLogs "browser"

-- | Retrieve logs of a specific type from the browser.
-- This function automatically detects the browser and uses the appropriate method:
-- - Chrome/Chromium: Uses Chrome DevTools Protocol (CDP)
-- - Firefox: Uses WebDriver BiDi when available, falls back gracefully
-- - Selenium: Uses legacy log endpoint (/log)
-- - Other browsers: Returns empty list
--
-- Common log types: "browser", "driver", "performance", "server"
getLogs :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getLogs logType = do
  Session { sessionDriver = Driver { _driverConfig = driverConfig } } <- getSession
  case detectBrowserFromDriver driverConfig of
    Just BrowserChrome -> getChromeLogs logType
    Just BrowserFirefox -> getFirefoxLogs logType
    Just BrowserSelenium -> getSeleniumLogs logType
    Nothing -> return [] -- Return empty for unsupported browsers
