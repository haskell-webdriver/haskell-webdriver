module Test.WebDriver.Commands.Logs (
  -- * Main log retrieval functions
  withRecordLogsViaBiDi
  , withRecordLogsViaBiDi'

  , getLogs

  -- * Driver-specific implementations
  -- , getChromeLogs
  -- , getFirefoxLogs
  -- , getSeleniumLogs
  -- , getSeleniumLogTypes

  -- * Types
  , LogType
  , LogEntry(..)
  , LogLevel(..)
  ) where

import GHC.Stack
import Test.WebDriver.Commands.Logs.BiDi
import Test.WebDriver.Commands.Logs.Chrome
import Test.WebDriver.Commands.Logs.Common
import Test.WebDriver.Commands.Logs.Firefox
import Test.WebDriver.Commands.Logs.Selenium
import Test.WebDriver.Types


-- | Retrieve logs of a specific type from the browser. The W3C spec doesn't
-- define how to do this natively, so we automatically detect the browser and
-- try to use an appropriate method:
--
-- * Chrome/Chromium: Chrome DevTools Protocol (CDP)
-- * Firefox: legacy log endpoint (/log)
-- * Selenium: legacy log endpoint (/log)
-- * Other browsers: returns empty list
--
-- Common log types: @browser@, @driver@, @performance@, @server@.
--
-- However, the modern way to do this is via 'withRecordLogsViaBiDi'.
getLogs :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getLogs logType = do
  Session { sessionDriver = Driver { _driverConfig = driverConfig } } <- getSession
  case detectBrowserFromDriver driverConfig of
    Just BrowserChrome -> getChromeLogs logType
    Just BrowserFirefox -> getFirefoxLogs logType
    Just BrowserSelenium -> getSeleniumLogs logType
    Nothing -> return [] -- Return empty for unsupported browsers
