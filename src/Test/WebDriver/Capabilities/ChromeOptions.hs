{-# LANGUAGE TemplateHaskell #-}

module Test.WebDriver.Capabilities.ChromeOptions where

import Data.Aeson as A
import Data.Aeson.TH
import Data.Text
import Lens.Micro.TH
import Test.WebDriver.Capabilities.Aeson


-- | See https://developer.chrome.com/docs/chromedriver/mobile-emulation
data ChromeDeviceMetrics = ChromeDeviceMetrics {
  -- | The width in pixels of the device's screen.
  _chromeDeviceMetricsWidth :: Maybe Int
  -- | The height in pixels of the device's screen.
  , _chromeDeviceMetricsHeight :: Maybe Int
  -- | The device's pixel ratio.
  , _chromeDeviceMetricsPixelRatio :: Maybe Double
  -- | Whether to emulate touch events. The value defaults to true and usually
  -- can be omitted.
  , _chromeDeviceMetricsTouch :: Maybe Bool
  -- | Whether the browser must behave as a mobile user agent (overlay
  -- scrollbars, emit orientation events, shrink the content to fit the
  -- viewport, etc.). The value defaults to true and usually can be omitted.
  , _chromeDeviceMetricsMobile :: Maybe Bool
  } deriving (Show, Eq)
deriveJSON toCamel3 ''ChromeDeviceMetrics
makeLenses ''ChromeDeviceMetrics

data BrandAndVersion = BrandAndVersion {
  brandAndVersionBrand :: String
  , brandAndVersionVersion :: String
  } deriving (Show, Eq)
deriveJSON toCamel3 ''BrandAndVersion
makeLenses ''BrandAndVersion

-- | See https://developer.chrome.com/docs/chromedriver/mobile-emulation
data ChromeClientHints = ChromeClientHints {
  -- | The operating system. It can be either a known value ("Android", "Chrome
  -- OS", "Chromium OS", "Fuchsia", "Linux", "macOS", "Windows"), that exactly
  -- matches the value returned by Chrome running on the given platform, or it
  -- can be a user defined value. This value is mandatory.
  _chromeClientHintsPlatform :: String
  -- | Whether the browser should request a mobile or desktop resource version.
  -- Usually Chrome running on a mobile phone with Android sets this value to
  -- true. Chrome on a tablet Android device sets this value to false. Chrome on
  -- a desktop device also sets this value to false. You can use this
  -- information to specify a realistic emulation. This value is mandatory.
  , _chromeClientHintsMobile :: Bool
  -- | List of brand / major version pairs. If omitted the browser uses its own
  -- values.
  , _chromeClientHintsBrands :: Maybe [BrandAndVersion]
  -- | List of brand / version pairs. It omitted the browser uses its own
  -- values.
  , _chromeClientHintsFullVersionList :: Maybe [BrandAndVersion]
  -- | OS version. Defaults to empty string.
  , _chromeClientHintsPlatformVersion :: Maybe String
  -- | Device model. Defaults to empty string.
  , _chromeClientHintsModel :: Maybe String
  -- | CPU architecture. Known values are "x86" and "arm". The user is free to
  -- provide any string value. Defaults to empty string.
  , _chromeClientHintsArchitecture :: Maybe String
  -- | Platform bitness. Known values are "32" and "64". The user is free to
  -- provide any string value. Defaults to empty string.
  , _chromeClientHintsBitness :: Maybe String
  -- | Emulation of windows 32 on windows 64. A boolean value that defaults to
  -- false.
  , _chromeClientHintsWow64 :: Maybe Bool
  } deriving (Show, Eq)
deriveJSON toCamel3 ''ChromeClientHints
makeLenses ''ChromeClientHints
mkChromeClientHints :: String -> Bool -> ChromeClientHints
mkChromeClientHints platform mobile = ChromeClientHints {
  _chromeClientHintsPlatform = platform
  , _chromeClientHintsMobile = mobile
  , _chromeClientHintsBrands = Nothing
  , _chromeClientHintsFullVersionList = Nothing
  , _chromeClientHintsPlatformVersion = Nothing
  , _chromeClientHintsModel = Nothing
  , _chromeClientHintsArchitecture = Nothing
  , _chromeClientHintsBitness = Nothing
  , _chromeClientHintsWow64 = Nothing
  }

-- | See https://developer.chrome.com/docs/chromedriver/mobile-emulation
data ChromeMobileEmulation =
  -- | Specify a known device. To enable device emulation with a specific
  -- device, the "mobileEmulation" dictionary must contain a "deviceName." Use a
  -- valid device name from the DevTools Emulated Devices settings as the value
  -- for "deviceName."
  ChromeMobileEmulationSpecificDevice {
    _chromeMobileEmulationDeviceName :: String
  }
  -- | Specify individual device attributes.
  | ChromeMobileEmulationIndividualAttributes {
      _chromeMobileEmulationDeviceMetrics :: Maybe ChromeDeviceMetrics
      , _chromeMobileEmulationClientHints :: Maybe ChromeClientHints
      -- | ChromeDriver is capable to infer "userAgent" value from "clientHints" on
      -- the following platforms: "Android", "Chrome OS", "Chromium OS", "Fuchsia",
      -- "Linux", "macOS", "Windows". Therefore this value can be omitted.
      --
      -- If "clientHints" dictionary is omitted (legacy mode) ChromeDriver does its
      -- best to infer the "clientHints" from "userAgent". This feature is not
      -- reliable, due to internal ambiguities of "userAgent" value format.
      , _chromeMobileEmulationUserAgent :: Maybe String
      }
  deriving (Show, Eq)
deriveJSON toCamel3 ''ChromeMobileEmulation
makeLenses ''ChromeMobileEmulation

-- | See https://developer.chrome.com/docs/chromedriver/capabilities#chromeoptions_object
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
  , _chromeOptionsMobileEmulation :: Maybe ChromeMobileEmulation
  -- | An optional dictionary that specifies performance logging preferences.
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
