{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Test.WebDriver.Commands.Logs.BiDi (
  withRecordLogsViaBiDi
  , withRecordLogsViaBiDi'
  ) where

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
import UnliftIO.STM (atomically, stateTVar)
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

  bidiSessionId <- atomically $ stateTVar sessionIdCounter (\x -> (x, x + 1))

  withRecordLogsViaBiDi' bidiSessionId uri cb action

-- | Connect to WebSocket URL and subscribe to log events using the W3C BiDi protocol; see
-- <https://w3c.github.io/webdriver-bidi/>.
withRecordLogsViaBiDi' :: (MonadUnliftIO m, MonadLogger m) => Int -> URI.URI -> (LogEntry -> m ()) -> m a -> m a
withRecordLogsViaBiDi' bidiSessionId uri@(URI.URI { uriAuthority=(Just (URI.URIAuth {uriPort=(readMaybe . L.drop 1 -> Just (port :: Int)), ..})), .. }) cb action = do
  logDebugN [i|BiDi: Connecting to #{uriRegName}:#{port}#{uriPath}|]

  withRunInIO $ \runInIO -> liftIO $ WS.runClient uriRegName port uriPath $ \conn -> runInIO $ do
    logDebugN [i|BiDi: Connected successfully, sending subscription request with ID #{bidiSessionId}|]
    liftIO $ WS.sendTextData conn $ encode $ object [
      "id" .= bidiSessionId
      , "method" .= ("session.subscribe" :: Text)
      , "params" .= object [
          "events" .= (["log.entryAdded"] :: [Text])
        ]
      ]
    logDebugN "BiDi: Sent subscription request, waiting for response..."

    timeout 15_000_000 (waitForSubscriptionResult bidiSessionId conn) >>= \case
      Nothing -> throwIO $ userError "BiDi: Subscription response timed out"
      Just (Left err) ->
        throwIO $ userError [i|BiDi: got exception (URI #{uri}): #{err}|]
      Just (Right ()) -> do
        logDebugN "BiDi: Starting log event listener"
        withAsync (messageListener conn) $ \messageListenerAsy -> do
          ret <- flip finally (logInfoN [i|BiDi: finished wrapped action|]) action

          logInfoN [i|BiDi: Going to try cancelling the log event listener|]
          flip finally (logInfoN [i|BiDi: finished cancelling log event listener|]) $
            flip withException (\(e :: SomeException) -> (logInfoN [i|BiDi: cancelling log event listener threw exception: #{e}|])) $
            cancel messageListenerAsy

          return ret

  where
    messageListener conn =
      forever $
        (decode <$>) (liftIO $ WS.receiveData conn) >>= \case
          Just (BiDiLogEvent "event" "log.entryAdded" params) ->
            case parseBiDiLogEntry params of
              Just logEntry -> cb logEntry
              Nothing -> logWarnN "BiDi: Failed to parse log entry"
          x -> logDebugN [i|BiDi: Ignoring non-log event message: #{x}|]
withRecordLogsViaBiDi' _ uri _cb _action =
  throwIO $ userError [i|WebSocket URL didn't contain an authority: #{uri}|]


waitForSubscriptionResult :: (MonadIO m, MonadLogger m) => Int -> WS.Connection -> m (Either SomeException ())
waitForSubscriptionResult bidiSessionId conn = fix $ \loop -> do
  msg <- liftIO $ WS.receiveData conn
  logDebugN [i|BiDi: Waiting for subscription response: #{msg}|]
  case decode msg of
    Just response@(BiDiResponse responseType responseId _ _)
      | responseType == "success" && responseId == bidiSessionId -> do
          logInfoN "BiDi: Subscription successful!"
          return $ Right ()
      | responseType == "error" && responseId == bidiSessionId -> do
          let errMsg = "BiDi subscription failed: " ++ show response
          logErrorN [i|BiDi: #{errMsg}|]
          return $ Left (toException (userError errMsg))
      | otherwise -> do
          logDebugN [i|BiDi: Ignoring response with type #{responseType}, ID #{responseId}|]
          loop
    Nothing -> do
      logDebugN [i|BiDi: Not a BiDiResponse, continuing to wait for subscription response (#{msg})|]
      loop

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
parseBiDiLogEntry _ = Nothing

parseLogLevel :: Text -> Maybe LogLevel
parseLogLevel "debug" = Just LogDebug
parseLogLevel "info" = Just LogInfo
parseLogLevel "warn" = Just LogWarning
parseLogLevel "error" = Just LogSevere
parseLogLevel _ = Nothing
