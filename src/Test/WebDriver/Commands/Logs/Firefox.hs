{-# LANGUAGE OverloadedStrings #-}

module Test.WebDriver.Commands.Logs.Firefox (
  getFirefoxLogs
  ) where

import Data.Aeson
import qualified Data.Foldable as F
import GHC.Stack
import Test.WebDriver.Commands.Logs.Common
import Test.WebDriver.JSON
import Test.WebDriver.Types
import Test.WebDriver.Util.Commands
import UnliftIO.Exception


getFirefoxLogs :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getFirefoxLogs logType =
  tryAny (getFirefoxLogsBiDi logType) >>= \case
    Right logs -> return logs
    Left (_ :: SomeException) -> getLegacyFirefoxLogs logType

-- | Attempt to get Firefox logs via WebDriver BiDi (future support)
getFirefoxLogsBiDi :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getFirefoxLogsBiDi _logType =
  -- WebDriver BiDi support for logs is still experimental in Firefox
  -- This would be the future implementation when Firefox supports log.entryAdded
  doSessCommand methodGet "/log/entries" Null >>= parseFirefoxLogs

parseFirefoxLogs :: WebDriver wd => Value -> wd [LogEntry]
parseFirefoxLogs (Array logs) = mapM fromJSON' (F.toList logs)
parseFirefoxLogs _ = return []

-- | Try legacy Firefox log endpoints (typically not supported)
getLegacyFirefoxLogs :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getLegacyFirefoxLogs logType = do
  tryAny (doSessCommand methodPost "/log" $ object ["type" .= logType]) >>= \case
    Right (Array logs) -> mapM fromJSON' (F.toList logs)
    _ -> return []
