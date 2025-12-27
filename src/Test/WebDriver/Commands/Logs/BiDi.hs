{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Test.WebDriver.Commands.Logs.BiDi (
  withRecordLogsViaBiDi
  , withRecordLogsViaBiDi'
  ) where

import Control.Concurrent.STM (retry)
import Control.Monad (forever)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Unlift
import Control.Monad.Logger (MonadLogger, logDebugN, logErrorN, logInfoN, logWarnN)
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
import UnliftIO.Async (cancel, withAsync)
import UnliftIO.Exception
import UnliftIO.STM (atomically, newTVarIO, readTVar, writeTVar)
import UnliftIO.Timeout (timeout)


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
deriveFromJSON toCamel3 ''BiDiResponse

-- | Wrapper around 'withRecordLogsViaBiDi'' which uses the WebSocket URL from
-- the current 'Session'. You must make sure to pass '_capabilitiesWebSocketUrl'
-- = @Just True@ to enable this. This will not work with Selenium 3.
withRecordLogsViaBiDi :: (WebDriver m, MonadLogger m) => (LogEntry -> m ()) -> m a -> m a
withRecordLogsViaBiDi cb action = do
  Session {..} <- getSession
  webSocketUrl <- case sessionWebSocketUrl of
    Nothing -> throwIO $ userError [i|Session wasn't configured with a BiDi WebSocket URL when trying to record logs. Make sure to enable _capabilitiesWebSocketUrl.|]
    Just x -> pure x

  uri <- case URI.parseURI webSocketUrl of
    Just x -> pure x
    Nothing -> throwIO $ userError [i|Couldn't parse WebSocket URL: #{webSocketUrl}|]

  withRecordLogsViaBiDi' uri cb action

-- | Connect to WebSocket URL and subscribe to log events using the W3C BiDi protocol; see
-- <https://w3c.github.io/webdriver-bidi/>.
withRecordLogsViaBiDi' :: (MonadUnliftIO m, MonadLogger m) => URI.URI -> (LogEntry -> m ()) -> m a -> m a
withRecordLogsViaBiDi' uri@(URI.URI { uriAuthority=(Just (URI.URIAuth {uriPort=(readMaybe . L.drop 1 -> Just (port :: Int)), ..})), .. }) cb action = do
  subscriptionStatus <- newTVarIO Nothing

  withAsync (backgroundAction subscriptionStatus) $ \backgroundActionAsy -> do
    -- Wait for subscription to be confirmed or errored before proceeding
    result <- atomically $ do
      status <- readTVar subscriptionStatus
      case status of
        Nothing -> retry
        Just (Left err) -> pure (Left err)
        Just (Right ()) -> pure (Right ())

    case result of
      Left err -> throwIO err
      Right () -> do
        ret <- flip finally (logInfoN [i|BiDi: finished wrapped action|]) action

        logInfoN [i|BiDi: Going to try cancelling the background action|]
        flip withException (\(e :: SomeException) -> (logInfoN [i|BiDi: finished cancelling background action: #{e}|])) $ cancel backgroundActionAsy

        return ret

  where
    backgroundAction subscriptionStatus = withRunInIO $ \runInIO -> do
      result <- tryAny $ do
        runInIO $ logDebugN [i|BiDi: Connecting to #{uriRegName}:#{port}#{uriPath}|]

        liftIO $ WS.runClient uriRegName port uriPath $ \conn -> do
          runInIO $ logInfoN "BiDi: Connected successfully"

          -- Send subscription request for console logs
          runInIO $ logDebugN "BiDi: Sending subscription request"
          WS.sendTextData conn $ encode $ object [
            "id" .= (1 :: Int)
            , "method" .= ("session.subscribe" :: Text)
            , "params" .= object [
                "events" .= (["log.entryAdded"] :: [Text])
              ]
            ]
          runInIO $ logDebugN "BiDi: Sent subscription request, waiting for response..."

          subscriptionResult <- timeout 15_000_000 $ fix $ \loop -> do
            msg <- WS.receiveData conn
            runInIO $ logDebugN [i|BiDi: Waiting for subscription response: #{msg}|]
            case decode msg of
              Just response@(BiDiResponse responseType responseId _ _)
                | responseType == "success" && responseId == 1 -> do
                    runInIO $ logInfoN "BiDi: Subscription successful!"
                    atomically $ writeTVar subscriptionStatus (Just (Right ()))
                | responseType == "error" && responseId == 1 -> do
                    let errMsg = "BiDi subscription failed: " ++ show response
                    runInIO $ logErrorN [i|BiDi: #{errMsg}|]
                    atomically $ writeTVar subscriptionStatus (Just (Left (toException (userError errMsg))))
                | otherwise -> do
                    runInIO $ logDebugN [i|BiDi: Ignoring response with type #{responseType}, ID #{responseId}|]
                    loop
              Nothing -> do
                runInIO $ logDebugN [i|BiDi: Not a BiDiResponse, continuing to wait for subscription response (#{msg})|]
                loop

          case subscriptionResult of
            Nothing -> do
              runInIO $ logErrorN "BiDi: Subscription response timed out"
              atomically $ writeTVar subscriptionStatus (Just (Left (toException (userError "BiDi subscription response timed out"))))
            Just () -> do
              runInIO $ logDebugN "BiDi: Starting log event listener"
              forever $ do
                msg <- WS.receiveData conn
                runInIO $ logDebugN [i|BiDi: Received log event: #{msg}|]
                case decode msg of
                  Just (BiDiLogEvent "event" "log.entryAdded" params) ->
                    case parseBiDiLogEntry params of
                      Just logEntry -> runInIO (cb logEntry)
                      Nothing -> runInIO $ logWarnN "BiDi: Failed to parse log entry"
                  _ -> runInIO $ logDebugN [i|BiDi: Ignoring non-log event message: #{msg}|]

      case result of
        Left ex -> do
          runInIO $ logErrorN [i|BiDi: got exception (URI #{uri}): #{ex}|]
          atomically $ writeTVar subscriptionStatus (Just (Left ex))
        Right () -> pure ()
withRecordLogsViaBiDi' uri _cb _action =
  throwIO $ userError [i|WebSocket URL didn't contain an authority: #{uri}|]

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
