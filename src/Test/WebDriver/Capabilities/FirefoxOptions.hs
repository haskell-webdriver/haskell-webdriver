{-# LANGUAGE TemplateHaskell #-}

module Test.WebDriver.Capabilities.FirefoxOptions where

import Data.Aeson
import Data.Aeson.TH
import Data.String (fromString)
import Data.Text (Text, toLower, toUpper)
import Test.WebDriver.Capabilities.Aeson

-- https://developer.mozilla.org/en-US/docs/Web/WebDriver/Capabilities/firefoxOptions
data FirefoxOptions = FirefoxOptions {
  -- | Absolute path to the custom Firefox binary to use.
  -- On macOS you may either give the path to the application bundle, i.e. /Applications/Firefox.app, or the absolute path to the executable binary inside this bundle, for example /Applications/Firefox.app/Contents/MacOS/firefox-bin.
  -- geckodriver will attempt to deduce the default location of Firefox on the current system if left undefined. The default locations of Firefox are:
  firefoxOptionsBinary :: Maybe String
  -- | Command line arguments to pass to the Firefox binary. These must include the leading dash (-) where required, e.g. ["-headless"].
  -- To have geckodriver pick up an existing profile on the local filesystem, you may pass ["-profile", "/path/to/profile"]. But if a profile has to be transferred to a target machine it is recommended to use the profile entry.
  , firefoxOptionsArgs :: Maybe [String]
  }
  deriving (Show, Eq)
deriveJSON baseOptions ''FirefoxOptions


defaultFirefoxOptions :: FirefoxOptions
defaultFirefoxOptions = FirefoxOptions {
  firefoxOptionsBinary = Nothing
  , firefoxOptionsArgs = Nothing
  }
