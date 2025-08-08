module Test.WebDriver.Commands.Logs.Common (
  -- * Browser types and detection
  Browser(..)
  , detectBrowserFromDriver

  -- * Re-exports from existing modules
  , module Test.WebDriver.Commands.SeleniumSpecific.Misc
  ) where

import Test.WebDriver.Commands.SeleniumSpecific.Misc (LogEntry(..), LogLevel(..), LogType)
import Test.WebDriver.Types

-- | Supported browsers for log retrieval
data Browser = 
    BrowserChrome 
  | BrowserFirefox
  | BrowserSelenium
  deriving (Eq, Show)

-- | Extract browser type from driver configuration
detectBrowserFromDriver :: DriverConfig -> Maybe Browser
detectBrowserFromDriver driverConfig = case driverConfig of
  DriverConfigSeleniumJar { driverConfigSubDrivers = subDrivers } ->
    -- For Selenium jar, check the sub-drivers
    case subDrivers of
      (subDriver:_) -> detectBrowserFromDriver subDriver -- Use first sub-driver
      [] -> Just BrowserSelenium -- No sub-drivers, use Selenium legacy endpoint
  DriverConfigGeckodriver {} -> Just BrowserFirefox
  DriverConfigChromedriver {} -> Just BrowserChrome