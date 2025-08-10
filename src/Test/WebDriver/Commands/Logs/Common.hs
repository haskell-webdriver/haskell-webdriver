module Test.WebDriver.Commands.Logs.Common (
  Browser(..)
  , detectBrowserFromDriver

  -- * Re-exports
  , LogType
  , LogEntry(..)
  , LogLevel(..)
  ) where

import Data.Aeson as A
import Data.Aeson.Types (typeMismatch)
import Data.Maybe
import Data.Text (Text)
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


-- | A record that represents a single log entry.
data LogEntry = LogEntry {
  -- | Timestamp for the log entry. The standard does not specify the epoch or
  -- the unit of time.
  logTime  :: Integer
  -- | Log verbosity level.
  , logLevel :: LogLevel
  , logMsg   :: Text
  }
  deriving (Eq, Ord, Show, Read)

instance FromJSON LogEntry where
  parseJSON (Object o) =
    LogEntry <$> o .: "timestamp"
             <*> o .: "level"
             <*> (fromMaybe "" <$> o .: "message")
  parseJSON v = typeMismatch "LogEntry" v

instance ToJSON LogEntry where
  toJSON (LogEntry {..}) = A.object [
    ("timestamp", A.Number (fromIntegral logTime))
    , ("level", toJSON logLevel)
    , ("message", A.String logMsg)
    ]

type LogType = String

-- | Indicates a log verbosity level. Used in 'Firefox' and 'Opera' configuration.
data LogLevel =
  LogOff
  | LogSevere
  | LogWarning
  | LogInfo
  | LogConfig
  | LogFine
  | LogFiner
  | LogFinest
  | LogDebug
  | LogAll
  deriving (Eq, Show, Read, Ord, Bounded, Enum)

instance ToJSON LogLevel where
  toJSON p= String $ case p of
    LogOff -> "OFF"
    LogSevere -> "SEVERE"
    LogWarning -> "WARNING"
    LogInfo -> "INFO"
    LogConfig -> "CONFIG"
    LogFine -> "FINE"
    LogFiner -> "FINER"
    LogFinest -> "FINEST"
    LogDebug -> "DEBUG"
    LogAll -> "ALL"

instance FromJSON LogLevel where
  parseJSON (String s) = case s of
    "OFF" -> return LogOff
    "SEVERE" -> return LogSevere
    "WARNING" -> return LogWarning
    "INFO" -> return LogInfo
    "CONFIG" -> return LogConfig
    "FINE" -> return LogFine
    "FINER" -> return LogFiner
    "FINEST" -> return LogFinest
    "DEBUG" -> return LogDebug
    "ALL" -> return LogAll
    _ -> fail ("Invalid logging preference: " ++ show s)

  parseJSON other = typeMismatch "LogLevel" other
