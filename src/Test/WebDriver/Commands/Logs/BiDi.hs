{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Test.WebDriver.Commands.Logs.BiDi (
  withRecordBiDiLogs
  , withRecordBiDiLogs'
  ) where

import Control.Monad (forever)
import Control.Monad.IO.Unlift
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (parseEither)
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text (Text)
import qualified Network.URI as URI
import qualified Network.WebSockets as WS
import Test.WebDriver.Capabilities.Aeson
import Test.WebDriver.Commands.Logs.Common
import Test.WebDriver.Types
import Text.Read (readMaybe)
import UnliftIO.Async
import UnliftIO.Exception


data BiDiLogEvent = BiDiLogEvent {
  biDiType :: Text
  , biDiMethod :: Text
  , biDiParams :: Value
  } deriving Show
deriveFromJSON toCamel2 ''BiDiLogEvent

withRecordBiDiLogs :: WebDriver m => (LogEntry -> m ()) -> m a -> m a
withRecordBiDiLogs cb action = do
  Session {..} <- getSession
  webSocketUrl <- case sessionWebSocketUrl of
    Nothing -> throwIO $ userError [i|Session wasn't configured with a BiDi WebSocket URL when trying to record logs. Make sure to enable _capabilitiesWebSocketUrl.|]
    Just x -> pure x

  withRecordBiDiLogs' webSocketUrl cb action

-- | Convert BiDi log parameters to LogEntry
parseBiDiLogEntry :: Value -> Maybe LogEntry
parseBiDiLogEntry = \case
  Object o -> case parseEither parseLogEntry o of
    Right entry -> Just entry
    Left _ -> Nothing
  _ -> Nothing
  where
    parseLogEntry o = do
      timestamp <- o .: "timestamp"
      levelText <- o .: "level"
      message <- o .: "text"
      level <- case parseLogLevel levelText of
        Just l -> pure l
        Nothing -> fail "Invalid log level"
      pure $ LogEntry (round (timestamp :: Double)) level message

    parseLogLevel :: Text -> Maybe LogLevel
    parseLogLevel = \case
      "debug" -> Just LogDebug
      "info" -> Just LogInfo
      "warn" -> Just LogWarning
      "error" -> Just LogSevere
      _ -> Nothing

withRecordBiDiLogs' :: MonadUnliftIO m => String -> (LogEntry -> m ()) -> m a -> m a
withRecordBiDiLogs' webSocketUrl cb action = withAsync backgroundAction $ \_ -> action
  where
    backgroundAction = withRunInIO $ \runInIO -> do
      (host, port, path) <- parseWebSocketUrl webSocketUrl

      liftIO $ WS.runClient host port path $ \conn -> do
        WS.sendTextData conn $ encode $ object [
          "id" .= (1 :: Int)
          , "method" .= ("log.entryAdded" :: Text)
          , "params" .= object []
          ]

        forever $ do
          (decode <$> WS.receiveData conn) >>= \case
            Just (BiDiLogEvent "event" "log.entryAdded" params) ->
              case parseBiDiLogEntry params of
                Just logEntry -> runInIO (cb logEntry)
                Nothing -> pure ()
            _ -> pure ()

parseWebSocketUrl :: MonadIO m => String -> m (String, Int, String)
parseWebSocketUrl url = case URI.parseURI url of
  Just (URI.URI { uriAuthority=(Just (URI.URIAuth {uriPort=(readMaybe . L.drop 1 -> Just port), ..})), .. }) ->
    pure (uriRegName, port, uriPath)
  _ -> liftIO $ throwIO $ userError [i|Invalid WebSocket URL: #{url}|]
