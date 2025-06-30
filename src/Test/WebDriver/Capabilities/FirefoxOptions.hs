{-# LANGUAGE TemplateHaskell #-}

module Test.WebDriver.Capabilities.FirefoxOptions where

import Data.Aeson as A
import Data.Aeson.TH
import Lens.Micro.TH
import Test.WebDriver.Capabilities.Aeson
import Test.WebDriver.Profile


data FirefoxLogLevelType =
  FirefoxLogLevelTypeTrace
  | FirefoxLogLevelTypeDebug
  | FirefoxLogLevelTypeConfig
  | FirefoxLogLevelTypeInfo
  | FirefoxLogLevelTypeWarn
  | FirefoxLogLevelTypeError
  | FirefoxLogLevelTypeFatal
  deriving (Show, Eq)
deriveJSON toCamelC4 ''FirefoxLogLevelType
makeLenses ''FirefoxLogLevelType

data FirefoxLogLevel = FirefoxLogLevel {
  _firefoxLogLevelLevel :: FirefoxLogLevelType
  }
  deriving (Show, Eq)
deriveJSON toCamel3 ''FirefoxLogLevel
makeLenses ''FirefoxLogLevel

-- | See https://developer.mozilla.org/en-US/docs/Web/WebDriver/Capabilities/firefoxOptions
data FirefoxOptions = FirefoxOptions {
  -- | Absolute path to the custom Firefox binary to use.
  -- On macOS you may either give the path to the application bundle, i.e. @\/Applications\/Firefox.app@, or the absolute
  -- path to the executable binary inside this bundle, for example
  -- @\/Applications\/Firefox.app\/Contents\/MacOS\/firefox-bin@. geckodriver will attempt to deduce the default location of
  -- Firefox on the current system if left undefined.
  _firefoxOptionsBinary :: Maybe String
  -- | Command line arguments to pass to the Firefox binary.
  -- These must include the leading dash (-) where required, e.g. ["-headless"].
  -- To have geckodriver pick up an existing profile on the local filesystem, you may pass @["-profile", -- "\/path\/to\/profile"]@.
  -- But if a profile has to be transferred to a target machine it is recommended to use the profile entry.
  , _firefoxOptionsArgs :: Maybe [String]
  -- | Base64-encoded ZIP of a profile directory to use for the Firefox instance. This may be used to e.g. install
  -- extensions or custom certificates, but for setting custom preferences we recommend using the prefs entry instead.
  , _firefoxOptionsProfile :: Maybe (PreparedProfile Firefox)
  -- | To increase the logging verbosity of geckodriver and Firefox, you may pass a log object that may look like
  -- {"log": {"level": "trace"}} to include all trace-level logs and above.
  , _firefoxOptionsLog :: Maybe FirefoxLogLevel
  -- | Map of preference name to preference value, which can be a string, a boolean or an integer.
  , _firefoxOptionsPrefs :: Maybe A.Object

  -- TODO: Android options
  }
  deriving (Show, Eq)
deriveJSON toCamel2 ''FirefoxOptions
makeLenses ''FirefoxOptions

emptyFirefoxOptions :: FirefoxOptions
emptyFirefoxOptions = FirefoxOptions {
  _firefoxOptionsBinary = Nothing
  , _firefoxOptionsArgs = Nothing
  , _firefoxOptionsProfile = Nothing
  , _firefoxOptionsLog = Nothing
  , _firefoxOptionsPrefs = Nothing
  }

defaultFirefoxOptions :: FirefoxOptions
defaultFirefoxOptions = emptyFirefoxOptions
