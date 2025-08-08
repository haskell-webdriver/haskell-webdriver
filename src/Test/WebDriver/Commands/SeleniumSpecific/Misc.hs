
module Test.WebDriver.Commands.SeleniumSpecific.Misc (
  -- * Interacting with elements
  submit
  , isDisplayed

  -- * Element equality
  , (<==>)
  , (</=>)

  -- * Server information and logs
  , getLogTypes
  , LogType
  , LogEntry(..)
  , LogLevel(..)
  ) where

import Data.Aeson as A
import Data.Aeson.Types (typeMismatch)
import Data.Maybe
import Data.Text (Text)
import GHC.Stack
import Test.WebDriver.JSON
import Test.WebDriver.Types
import Test.WebDriver.Util.Commands


-- | Submit a form element. This may be applied to descendents of a form element
-- as well.
submit :: (HasCallStack, WebDriver wd) => Element -> wd ()
submit e = noReturn $ doElemCommand methodPost e "/submit" Null

-- | Determine if the element is displayed.
-- This function isn't guaranteed to be implemented by the WebDriver spec,
-- but it is found in Selenium.
-- See https://www.w3.org/TR/webdriver1/#element-displayedness.
isDisplayed :: (HasCallStack, WebDriver wd) => Element -> wd Bool
isDisplayed e = doElemCommand methodGet e "/displayed" Null

infix 4 <==>
-- | Determines if two element identifiers refer to the same element.
(<==>) :: (HasCallStack, WebDriver wd) => Element -> Element -> wd Bool
e1 <==> (Element e2) = doElemCommand methodGet e1 ("/equals/" <> urlEncode e2) Null

-- | Determines if two element identifiers refer to different elements.
infix 4 </=>
(</=>) :: (HasCallStack, WebDriver wd) => Element -> Element -> wd Bool
e1 </=> e2 = not <$> (e1 <==> e2)

-- | Get a list of available log types.
getLogTypes :: (HasCallStack, WebDriver wd) => wd [LogType]
getLogTypes = doSessCommand methodGet "/log/types" Null

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
