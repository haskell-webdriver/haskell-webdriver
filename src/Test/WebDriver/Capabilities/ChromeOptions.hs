{-# LANGUAGE TemplateHaskell #-}

module Test.WebDriver.Capabilities.ChromeOptions where

import Data.Aeson.TH
import Test.WebDriver.Capabilities.Aeson


-- https://developer.chrome.com/docs/chromedriver/capabilities
data ChromeOptions = ChromeOptions {
  -- List of command-line arguments to use when starting Chrome.
  -- Arguments with an associated value should be separated by a '=' sign
  -- (such as, ['start-maximized', 'user-data-dir=/tmp/temp_profile']).
  -- See a list of Chrome arguments: https://peter.sh/experiments/chromium-command-line-switches/
  chromeOptionsArgs :: Maybe [String]
  -- Path to the Chrome executable to use.
  -- On macOS X, this should be the actual binary, not just the app, such as,
  -- /Applications/Google Chrome.app/Contents/MacOS/Google Chrome.
  , chromeOptionsBinary :: Maybe String
  }
  deriving (Show, Eq)
deriveJSON baseOptions ''ChromeOptions


defaultChromeOptions :: ChromeOptions
defaultChromeOptions = ChromeOptions {
  chromeOptionsArgs = Nothing
  , chromeOptionsBinary = Nothing
  }
