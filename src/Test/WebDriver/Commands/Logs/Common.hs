module Test.WebDriver.Commands.Logs.Common (
  Browser(..)
  , detectBrowserFromDriver

  -- * Re-exports
  , LogType
  , LogEntry(..)
  , LogLevel(..)
  ) where

import Test.WebDriver.Commands.SeleniumSpecific.Misc
import Test.WebDriver.Types

-- | Supported browsers for log retrieval
data Browser =
  BrowserChrome
  | BrowserFirefox
  | BrowserSelenium
  deriving (Eq, Show)

detectBrowserFromDriver :: DriverConfig -> Maybe Browser
detectBrowserFromDriver driverConfig = case driverConfig of
  DriverConfigSeleniumJar { driverConfigSubDrivers = subDrivers } ->
    case subDrivers of
      (subDriver:_) -> detectBrowserFromDriver subDriver
      [] -> Just BrowserSelenium
  DriverConfigGeckodriver {} -> Just BrowserFirefox
  DriverConfigChromedriver {} -> Just BrowserChrome
