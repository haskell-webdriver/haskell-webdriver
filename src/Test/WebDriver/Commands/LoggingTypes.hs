
module Test.WebDriver.Commands.LoggingTypes where

import Data.Aeson as Aeson
import Data.Aeson.Types (Parser, typeMismatch, Pair)
import Data.Maybe
import Data.Text
import Test.WebDriver.Capabilities


-- | A record that represents a single log entry.
data LogEntry =
  LogEntry { logTime  :: Integer  -- ^ timestamp for the log entry. The standard
                                  -- does not specify the epoch or the unit of
                                  -- time.
           , logLevel :: LogLevel -- ^ log verbosity level
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
  toJSON (LogEntry {..}) = Aeson.object [
    ("timestamp", Aeson.Number (fromIntegral logTime))
    , ("level", toJSON logLevel)
    , ("message", Aeson.String logMsg)
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
