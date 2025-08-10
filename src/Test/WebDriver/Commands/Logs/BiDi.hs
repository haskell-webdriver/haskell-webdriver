{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Test.WebDriver.Commands.Logs.BiDi (
  withRecordBiDiLogs
  , withRecordBiDiLogs'
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (retry)
import Control.Monad (forever)
import Control.Monad.IO.Unlift
import Control.Monad.Logger (MonadLogger, logDebugN, logInfoN, logWarnN)
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
import UnliftIO.Async (race, withAsync)
import UnliftIO.Exception
import UnliftIO.STM (atomically, newTVarIO, readTVar, writeTVar)


data BiDiLogEvent = BiDiLogEvent {
  biDiType :: Text
  , biDiMethod :: Text
  , biDiParams :: Value
  } deriving Show
deriveFromJSON toCamel2 ''BiDiLogEvent

data BiDiResponse = BiDiResponse {
  biDiResponseType :: Text
  , biDiResponseId :: Int
  , biDiResponseResult :: Maybe Value
  , biDiResponseError :: Maybe Value
  } deriving Show

instance FromJSON BiDiResponse where
  parseJSON = withObject "BiDiResponse" $ \o -> BiDiResponse
    <$> o .: "type"
    <*> o .: "id"
    <*> o .:? "result"
    <*> o .:? "error"

withRecordBiDiLogs :: (WebDriver m, MonadLogger m) => (LogEntry -> m ()) -> m a -> m a
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

withRecordBiDiLogs' :: (MonadUnliftIO m, MonadLogger m) => String -> (LogEntry -> m ()) -> m a -> m a
withRecordBiDiLogs' webSocketUrl cb action = do
  subscriptionStatus <- newTVarIO Nothing  -- Nothing = pending, Just (Left err) = error, Just (Right ()) = success

  withAsync (backgroundAction subscriptionStatus) $ \_ -> do
    -- Wait for subscription to be confirmed or errored before proceeding
    result <- atomically $ do
      status <- readTVar subscriptionStatus
      case status of
        Nothing -> retry  -- Still pending
        Just (Left err) -> pure (Left err)
        Just (Right ()) -> pure (Right ())

    case result of
      Left err -> throwIO err
      Right () -> action
  where
    backgroundAction subscriptionStatus = withRunInIO $ \runInIO -> do
      -- Set a timeout for the entire background action
      result <- race (liftIO $ threadDelay 10_000_000) $ tryAny $ do
        runInIO $ logInfoN [i|BiDi: Parsing WebSocket URL: #{webSocketUrl}|]
        (host, port, path) <- parseWebSocketUrl webSocketUrl
        runInIO $ logInfoN [i|BiDi: Connecting to #{host}:#{port}#{path}|]

        liftIO $ WS.runClient host port path $ \conn -> do
          runInIO $ logInfoN "BiDi: Connected successfully"
          -- Send subscription request for console logs
          runInIO $ logInfoN "BiDi: Sending subscription request"
          WS.sendTextData conn $ encode $ object [
            "id" .= (1 :: Int)
            , "method" .= ("session.subscribe" :: Text)
            , "params" .= object [
                "events" .= (["log.entryAdded"] :: [Text])
              ]
            ]
          runInIO $ logInfoN "BiDi: Sent subscription request, waiting for response..."

          -- Listen for messages
          forever $ do
            msg <- WS.receiveData conn
            runInIO $ logInfoN [i|BiDi: Received message: #{msg}|]
            -- Try to parse as response first
            case decode msg of
              Just response@(BiDiResponse responseType responseId _ _) -> do
                runInIO $ logInfoN [i|BiDi: Parsed response: #{response}|]
                if responseType == "success" && responseId == 1
                  then do
                    runInIO $ logInfoN "BiDi: Subscription successful!"
                    atomically $ writeTVar subscriptionStatus (Just (Right ()))
                  else if responseType == "error" && responseId == 1
                    then do
                      let errMsg = "BiDi subscription failed: " ++ show response
                      runInIO $ logWarnN [i|BiDi: #{errMsg}|]
                      atomically $ writeTVar subscriptionStatus (Just (Left (toException (userError errMsg))))
                    else do
                      runInIO $ logDebugN [i|BiDi: Ignoring response with type #{responseType}, ID #{responseId}|]
              Nothing -> do
                runInIO $ logWarnN "BiDi: Failed to parse as BiDiResponse"
                -- Try to parse as log event
                case decode msg of
                  Just (BiDiLogEvent "event" "log.entryAdded" params) ->
                    case parseBiDiLogEntry params of
                      Just logEntry -> runInIO (cb logEntry)
                      Nothing -> pure ()
                  _ -> pure ()

      case result of
        Left () -> do -- Timeout occurred
          runInIO $ logWarnN "BiDi: Connection timed out"
          atomically $ writeTVar subscriptionStatus (Just (Left (toException (userError "BiDi WebSocket connection timed out"))))
        Right (Left ex) -> do -- Exception occurred
          runInIO $ logWarnN [i|BiDi: Exception occurred: #{ex}|]
          atomically $ writeTVar subscriptionStatus (Just (Left ex))
        Right (Right ()) -> pure () -- Success (shouldn't reach here normally)

parseWebSocketUrl :: MonadIO m => String -> m (String, Int, String)
parseWebSocketUrl url = case URI.parseURI url of
  Just (URI.URI { uriAuthority=(Just (URI.URIAuth {uriPort=(readMaybe . L.drop 1 -> Just port), ..})), .. }) ->
    pure (uriRegName, port, uriPath)
  _ -> liftIO $ throwIO $ userError [i|Invalid WebSocket URL: #{url}|]
