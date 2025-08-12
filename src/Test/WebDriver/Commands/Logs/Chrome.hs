{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Test.WebDriver.Commands.Logs.Chrome (
  getChromeLogs
  ) where

import Data.Aeson
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack
import Test.WebDriver.Commands.Logs.Common
import Test.WebDriver.JSON
import Test.WebDriver.Types
import Test.WebDriver.Util.Aeson
import Test.WebDriver.Util.Commands
import UnliftIO.Exception


getChromeLogs :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getChromeLogs logType =
  tryAny (getChromeLogsViaCDP logType) >>= \case
    Right entries -> return entries
    Left (_ :: SomeException) -> getLegacyChromeLogs logType

getChromeLogsViaCDP :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getChromeLogsViaCDP _logType = do
  result <- doSessCommand methodPost "/goog/cdp/execute" $ object [
    "cmd" .= ("Runtime.getConsoleEntries" :: Text)
    , "params" .= object []
    ]
  case result of
    (Object (aesonLookup "value" -> Just (Object (aesonLookup "result" -> Just (Object (aesonLookup "entries" -> Just (Array entries))))))) ->
      mapM parseChromeLogEntry (F.toList entries)
    _ -> return []

  where
    parseChromeLogEntry :: (WebDriver wd) => Value -> wd LogEntry
    parseChromeLogEntry (Object obj) = do
      timestamp :: Double <- obj !: "timestamp"
      level :: Text <- obj !: "level"
      message :: Text <- obj !: "text"
      return $ LogEntry {
        logTime = round timestamp
        , logLevel = parseChromeLogLevel level
        , logMsg = message
        }
    parseChromeLogEntry v = return $ LogEntry {
      logTime = 0
      , logLevel = LogInfo
      , logMsg = "Failed to parse log entry: " <> T.pack (show v)
      }

    parseChromeLogLevel :: Text -> LogLevel
    parseChromeLogLevel level = case level of
      "verbose" -> LogDebug
      "debug"   -> LogDebug
      "log"     -> LogInfo
      "info"    -> LogInfo
      "warning" -> LogWarning
      "error"   -> LogSevere
      _         -> LogInfo

getLegacyChromeLogs :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getLegacyChromeLogs logType = do
  tryAny (doSessCommand methodPost "/log" $ object ["type" .= logType]) >>= \case
    Right (Array logs) -> mapM fromJSON' (F.toList logs)
    Right _ -> return []
    Left (_ :: SomeException) -> return []
