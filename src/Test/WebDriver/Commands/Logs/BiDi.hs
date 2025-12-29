{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Test.WebDriver.Commands.Logs.BiDi (
  withRecordLogsViaBiDi
  , withRecordLogsViaBiDi'
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger (MonadLogger, logDebugN, logWarnN)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Network.URI as URI
import Test.WebDriver.Commands.BiDi.Session
import Test.WebDriver.Commands.Logs.Common
import Test.WebDriver.Types


logEvents :: [Text]
logEvents = ["log.entryAdded"]

-- | Wrapper around 'withRecordLogsViaBiDi'' which uses the WebSocket URL from
-- the current 'Session'. You must make sure to pass '_capabilitiesWebSocketUrl'
-- = @Just True@ to enable this. This will not work with Selenium 3.
withRecordLogsViaBiDi :: (WebDriver m, MonadLogger m) => (LogEntry -> m ()) -> m a -> m a
withRecordLogsViaBiDi cb action = do
  withBiDiSession logEvents (mkLogCallback cb) action

-- | Connect to WebSocket URL and subscribe to log events using the W3C BiDi protocol; see
-- <https://w3c.github.io/webdriver-bidi/>.
withRecordLogsViaBiDi' :: forall m a. (MonadUnliftIO m, MonadLogger m) => Int -> URI.URI -> (LogEntry -> m ()) -> m a -> m a
withRecordLogsViaBiDi' bidiSessionId uri cb action =
  withBiDiSession' bidiSessionId uri logEvents (mkLogCallback cb) action

mkLogCallback :: (MonadLogger m) => (LogEntry -> m ()) -> BiDiEvent -> m ()
mkLogCallback cb (BiDiEvent "event" "log.entryAdded" params) = case parseBiDiLogEntry params of
  Just logEntry -> cb logEntry
  Nothing -> logWarnN "BiDi: Failed to parse log entry"
mkLogCallback _cb x =
  logDebugN [i|BiDi: Ignoring non-log event message: #{x}|]

parseBiDiLogEntry :: Value -> Maybe LogEntry
parseBiDiLogEntry (Object o) = case parseEither parseLogEntry o of
  Right entry -> Just entry
  Left _ -> Nothing
  where
    parseLogEntry o' = do
      timestamp <- o' .: "timestamp"
      levelText <- o' .: "level"
      message <- o' .: "text"
      level <- case parseLogLevel levelText of
        Just l -> pure l
        Nothing -> fail "Invalid log level"
      pure $ LogEntry (round (timestamp :: Double)) level message

    parseLogLevel :: Text -> Maybe LogLevel
    parseLogLevel "debug" = Just LogDebug
    parseLogLevel "info" = Just LogInfo
    parseLogLevel "warn" = Just LogWarning
    parseLogLevel "error" = Just LogSevere
    parseLogLevel _ = Nothing
parseBiDiLogEntry _ = Nothing
