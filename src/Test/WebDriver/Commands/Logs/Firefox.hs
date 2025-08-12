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
getFirefoxLogs logType = getLegacyFirefoxLogs logType

-- | Try legacy Firefox log endpoints (typically not supported)
getLegacyFirefoxLogs :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getLegacyFirefoxLogs logType = do
  tryAny (doSessCommand methodPost "/log" $ object ["type" .= logType]) >>= \case
    Right (Array logs) -> mapM fromJSON' (F.toList logs)
    _ -> return []
