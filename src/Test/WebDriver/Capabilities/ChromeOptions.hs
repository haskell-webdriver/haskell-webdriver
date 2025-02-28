{-# LANGUAGE TemplateHaskell #-}

module Test.WebDriver.Capabilities.ChromeOptions where

import Data.Aeson as A
import Data.Aeson.TH
import Data.Text
import Lens.Micro.TH
import Test.WebDriver.Capabilities.Aeson


-- https://developer.chrome.com/docs/chromedriver/capabilities
data ChromeOptions = ChromeOptions {
  -- | List of command-line arguments to use when starting Chrome. Arguments with an associated value should be separated
  -- by a '=' sign (such as, ['start-maximized', 'user-data-dir=/tmp/temp_profile']). See a list of Chrome arguments:
  -- https://peter.sh/experiments/chromium-command-line-switches/
  _chromeOptionsArgs :: Maybe [String]
  -- | Path to the Chrome executable to use. On macOS X, this should be the actual binary, not just the app, such as,
  -- /Applications/Google Chrome.app/Contents/MacOS/Google Chrome.
  , _chromeOptionsBinary :: Maybe FilePath
  -- | A list of Chrome extensions to install on startup. Each item in the list should be a base-64 encoded packed Chrome
  -- extension (.crx)
  , _chromeOptionsExtensions :: Maybe [Text]
  -- | A dictionary with each entry consisting of the name of the preference and its value. These preferences are applied
  -- to the Local State file in the user data folder.
  , _chromeOptionsLocalState :: Maybe A.Object
  -- | A dictionary with each entry consisting of the name of the preference and its value. These preferences are only
  -- applied to the user profile in use. See the 'Preferences' file in Chrome's user data directory for examples.
  , _chromeOptionsPrefs :: Maybe A.Object
  -- | If false, Chrome is quit when ChromeDriver is killed, regardless of whether the session is quit.
  -- If true, Chrome only quits if the session is quit or closed. If true and the session isn't quit, ChromeDriver
  -- cannot clean up the temporary user data directory that the running Chrome instance is using.
  , _chromeOptionsDetach :: Maybe Bool
  -- | An address of a Chrome debugger server to connect to, in the form of @<hostname/ip:port>@, such as '127.0.0.1:38947'
  , _chromeOptionsDebuggerAddress :: Maybe String
  -- | List of Chrome command line switches to exclude that ChromeDriver by default passes when starting Chrome. Don't
  -- prefix switches with --.
  , _chromeOptionsExcludeSwitches :: Maybe [String]
  -- | Directory to store Chrome minidumps. (Supported only on Linux.)
  , _chromeOptionsMinidumpPath :: Maybe FilePath
  -- | A dictionary with either a value for "deviceName," or values for "deviceMetrics", and "userAgent." Refer to Mobile
  -- Emulation for more information.
  , _chromeOptionsMobileEmulation :: Maybe A.Object
  -- | An optional dictionary that specifies performance logging preferences. See below for more information.
  , _chromeOptionsPerfLoggingPrefs :: Maybe A.Object
  -- | A list of window types that appear in the list of window handles. For access to webview elements, include "webview"
  -- in this list.
  , _chromeOptionsWindowTypes :: Maybe [String]
  }
  deriving (Show, Eq)
deriveJSON toCamel2 ''ChromeOptions
makeLenses ''ChromeOptions

defaultChromeOptions :: ChromeOptions
defaultChromeOptions = ChromeOptions {
  _chromeOptionsArgs = Nothing
  , _chromeOptionsBinary = Nothing
  , _chromeOptionsExtensions = Nothing
  , _chromeOptionsLocalState = Nothing
  , _chromeOptionsPrefs = Nothing
  , _chromeOptionsDetach = Nothing
  , _chromeOptionsDebuggerAddress = Nothing
  , _chromeOptionsExcludeSwitches = Nothing
  , _chromeOptionsMinidumpPath = Nothing
  , _chromeOptionsMobileEmulation = Nothing
  , _chromeOptionsPerfLoggingPrefs = Nothing
  , _chromeOptionsWindowTypes = Nothing
  }
