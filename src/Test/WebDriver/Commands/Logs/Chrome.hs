{-# LANGUAGE OverloadedStrings #-}

module Test.WebDriver.Commands.Logs.Chrome (
  getChromeConsoleLogs
  , getChromeLogs
  ) where

import Data.Aeson
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack
import Test.WebDriver.Commands.Logs.Common
import Test.WebDriver.JSON
import Test.WebDriver.Types
import Test.WebDriver.Util.Commands
import UnliftIO.Exception (try, SomeException)

-- | Get console logs from Chrome using Chrome DevTools Protocol (CDP)
getChromeConsoleLogs :: (HasCallStack, WebDriver wd) => wd [LogEntry]
getChromeConsoleLogs = getChromeLogs "browser"

-- | Get logs from Chrome using Chrome DevTools Protocol for specified log type
getChromeLogs :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getChromeLogs logType = do
  result <- try $ getChromeLogsViaCDP logType
  case result of
    Right entries -> return entries
    Left (_ :: SomeException) -> 
      -- Fallback: Try legacy Selenium log endpoint if CDP fails
      getLegacyChromeLogs logType

-- | Get logs from Chrome using Chrome DevTools Protocol  
getChromeLogsViaCDP :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getChromeLogsViaCDP _logType = do
  -- Use CDP Runtime.consoleAPICalled to get console logs
  result <- doSessCommand methodPost "/goog/cdp/execute" $ object
    [ "cmd" .= ("Runtime.getConsoleEntries" :: Text)
    , "params" .= object []
    ]
  parseChromeCDPLogs result

-- | Parse Chrome CDP console entries into LogEntry format
parseChromeCDPLogs :: (HasCallStack, WebDriver wd) => Value -> wd [LogEntry]
parseChromeCDPLogs (Object obj) = do
  valueObj <- obj !: "value"
  case valueObj of
    Object valueObj' -> do
      resultObj <- valueObj' !: "result"
      case resultObj of
        Object resultObj' -> do
          entriesValue <- resultObj' !: "entries"
          case entriesValue of
            Array entries -> mapM parseChromeLogEntry (F.toList entries)
            _ -> return []
        _ -> return []
    _ -> return []
parseChromeCDPLogs _ = return []

-- | Parse individual Chrome log entry from CDP format
parseChromeLogEntry :: (HasCallStack, WebDriver wd) => Value -> wd LogEntry
parseChromeLogEntry (Object obj) = do
  timestamp :: Double <- obj !: "timestamp"
  level :: Text <- obj !: "level" 
  message :: Text <- obj !: "text"
  return $ LogEntry 
    { logTime = round timestamp
    , logLevel = parseChromeLogLevel level
    , logMsg = message
    }
parseChromeLogEntry v = return $ LogEntry 
  { logTime = 0
  , logLevel = LogInfo
  , logMsg = "Failed to parse log entry: " <> T.pack (show v)
  }

-- | Convert Chrome CDP log levels to our LogLevel type
parseChromeLogLevel :: Text -> LogLevel
parseChromeLogLevel level = case level of
  "verbose" -> LogDebug
  "debug"   -> LogDebug
  "log"     -> LogInfo
  "info"    -> LogInfo
  "warning" -> LogWarning
  "error"   -> LogSevere
  _         -> LogInfo

-- | Fallback to legacy Selenium log endpoint
getLegacyChromeLogs :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getLegacyChromeLogs logType = do
  result <- try $ doSessCommand methodPost "/log" $ object ["type" .= logType]
  case result of  
    Right (Array logs) -> mapM fromJSON' (F.toList logs)
    Right _ -> return []
    Left (_ :: SomeException) -> return []