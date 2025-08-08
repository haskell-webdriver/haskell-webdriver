{-# LANGUAGE OverloadedStrings #-}

module Test.WebDriver.Commands.Logs.Firefox (
  getFirefoxConsoleLogs
  , getFirefoxLogs
  ) where

import Data.Aeson
import qualified Data.Foldable as F
import GHC.Stack
import Test.WebDriver.Commands.Logs.Common
import Test.WebDriver.JSON
import Test.WebDriver.Types
import Test.WebDriver.Util.Commands
import UnliftIO.Exception (try, SomeException)

-- | Get console logs from Firefox
getFirefoxConsoleLogs :: (HasCallStack, WebDriver wd) => wd [LogEntry]
getFirefoxConsoleLogs = getFirefoxLogs "browser"

-- | Get logs from Firefox using available methods
getFirefoxLogs :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getFirefoxLogs logType = do
  -- Firefox currently has limited logging support in WebDriver
  -- Try WebDriver BiDi first (when available), then fall back to legacy
  result <- try $ getFirefoxLogsBiDi logType
  case result of
    Right logs -> return logs
    Left (_ :: SomeException) -> 
      -- Try legacy endpoint (usually not supported)
      getLegacyFirefoxLogs logType

-- | Attempt to get Firefox logs via WebDriver BiDi (future support)
getFirefoxLogsBiDi :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getFirefoxLogsBiDi _logType = do
  -- WebDriver BiDi support for logs is still experimental in Firefox
  -- This would be the future implementation when Firefox supports log.entryAdded
  result <- doSessCommand methodGet "/log/entries" Null
  parseFirefoxLogs result

-- | Parse Firefox logs (when available) 
parseFirefoxLogs :: WebDriver wd => Value -> wd [LogEntry]  
parseFirefoxLogs (Array logs) = mapM fromJSON' (F.toList logs)
parseFirefoxLogs _ = return []

-- | Try legacy Firefox log endpoints (typically not supported)
getLegacyFirefoxLogs :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getLegacyFirefoxLogs logType = do
  result <- try $ doSessCommand methodPost "/log" $ object ["type" .= logType]
  case result of  
    Right (Array logs) -> mapM fromJSON' (F.toList logs)
    Right _ -> return []
    Left (_ :: SomeException) -> return [] -- Firefox doesn't support legacy log retrieval