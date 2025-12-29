
module Test.WebDriver.Commands.BiDi.NetworkActivity (
    NetworkActivityVar
  , RequestInfo(..)
  , RequestId
  , withRecordNetworkActivityViaBiDi
  , withRecordNetworkActivityViaBiDi'
  , newNetworkActivityVar
  , readNetworkActivity
  , waitForNetworkIdle
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (retry)
import Control.Monad.IO.Unlift
import Control.Monad.Logger (MonadLogger, logDebugN, logWarnN)
import Data.Aeson
import Data.Aeson.Types (parseEither, Parser)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String.Interpolate
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Network.URI as URI
import Test.WebDriver.Commands.BiDi.Session
import Test.WebDriver.Types
import UnliftIO.STM


networkEvents :: [Text]
networkEvents = [
  "network.beforeRequestSent"
  , "network.responseCompleted"
  , "network.responseStarted"
  , "network.fetchError"
  ]

type RequestId = Text

data RequestInfo = RequestInfo {
  requestInfoRequestId :: RequestId
  , requestInfoMethod :: Text
  , requestInfoUrl :: Text
  , requestInfoTimestamp :: UTCTime
  , requestInfoRequestHeaders :: Maybe (Map Text Text)
  , requestInfoResponseHeaders :: Maybe (Map Text Text)
  , requestInfoResponseStatus :: Maybe Int
  , requestInfoResponseText :: Maybe Text
  , requestInfoErrorText :: Maybe Text
  , requestInfoCompleted :: Bool
  } deriving (Show, Eq)

type NetworkActivityVar = TVar (Map RequestId RequestInfo)

-- | Wrapper around 'withRecordLogsViaBiDi'' which uses the WebSocket URL from
-- the current 'Session'. You must make sure to pass '_capabilitiesWebSocketUrl'
-- = @Just True@ to enable this. This will not work with Selenium 3.
withRecordNetworkActivityViaBiDi :: (WebDriver m, MonadLogger m) => NetworkActivityVar -> m a -> m a
withRecordNetworkActivityViaBiDi networkActivityVar action = do
  withBiDiSession networkEvents (mkCallback networkActivityVar) action

-- | Connect to WebSocket URL and subscribe to log events using the W3C BiDi protocol; see
-- <https://w3c.github.io/webdriver-bidi/>.
withRecordNetworkActivityViaBiDi' :: forall m a. (MonadUnliftIO m, MonadLogger m) => Int -> URI.URI -> NetworkActivityVar -> m a -> m a
withRecordNetworkActivityViaBiDi' bidiSessionId uri networkActivityVar action =
  withBiDiSession' bidiSessionId uri networkEvents (mkCallback networkActivityVar) action

mkCallback :: (MonadIO m, MonadLogger m) => NetworkActivityVar -> BiDiEvent -> m ()
mkCallback nav (BiDiEvent "event" "network.beforeRequestSent" params) =
  case parseBeforeRequestSent params of
    Just (requestId, method, url, timestamp, headers) -> do
      atomically $ modifyTVar nav $ Map.insert requestId $ RequestInfo {
        requestInfoRequestId = requestId
        , requestInfoMethod = method
        , requestInfoUrl = url
        , requestInfoTimestamp = timestamp
        , requestInfoRequestHeaders = headers
        , requestInfoResponseHeaders = Nothing
        , requestInfoResponseStatus = Nothing
        , requestInfoResponseText = Nothing
        , requestInfoErrorText = Nothing
        , requestInfoCompleted = False
        }
      logDebugN [i|BiDi: Network request started: #{requestId} #{method} #{url}|]
    Nothing -> logWarnN "BiDi: Failed to parse network.beforeRequestSent event"

mkCallback nav (BiDiEvent "event" "network.responseStarted" params) =
  case parseResponseStarted params of
    Just (requestId, status, headers) -> do
      atomically $ modifyTVar nav $ flip Map.adjust requestId $ \ri -> ri {
        requestInfoResponseStatus = Just status
        , requestInfoResponseHeaders = headers
        }
      logDebugN [i|BiDi: Network response started: #{requestId} status #{status}|]
    Nothing -> logWarnN "BiDi: Failed to parse network.responseStarted event"

mkCallback nav (BiDiEvent "event" "network.responseCompleted" params) =
  case parseResponseCompleted params of
    Just (requestId, responseText) -> do
      atomically $ modifyTVar nav $ flip Map.adjust requestId $ \ri -> ri {
        requestInfoResponseText = responseText
        , requestInfoCompleted = True
        }
      logDebugN [i|BiDi: Network response completed: #{requestId}|]
    Nothing -> logWarnN "BiDi: Failed to parse network.responseCompleted event"

mkCallback nav (BiDiEvent "event" "network.fetchError" params) =
  case parseFetchError params of
    Just (requestId, errorText) -> do
      atomically $ modifyTVar nav $ flip Map.adjust requestId $ \ri -> ri {
        requestInfoErrorText = Just errorText
        , requestInfoCompleted = True
        }
      logDebugN [i|BiDi: Network fetch error: #{requestId} - #{errorText}|]
    Nothing -> logWarnN "BiDi: Failed to parse network.fetchError event"

mkCallback _nav x = logDebugN [i|BiDi: Ignoring event: #{x}|]

-- | Parse network.beforeRequestSent event parameters
parseBeforeRequestSent :: Value -> Maybe (RequestId, Text, Text, UTCTime, Maybe (Map Text Text))
parseBeforeRequestSent (Object o) = case parseEither parseRequest o of
  Right result -> Just result
  Left _ -> Nothing
  where
    parseRequest o' = do
      requestId <- o' .: "requestId"
      request <- o' .: "request"
      method <- request .: "method"
      url <- request .: "url"
      timestamp <- o' .: "timestamp"
      headers <- optional (request .: "headers") >>= \case
        Just headerList -> Just <$> parseHeaders headerList
        Nothing -> pure Nothing
      let utcTime = posixSecondsToUTCTime (timestamp / 1000)
      pure (requestId, method, url, utcTime, headers)
parseBeforeRequestSent _ = Nothing

parseResponseStarted :: Value -> Maybe (RequestId, Int, Maybe (Map Text Text))
parseResponseStarted (Object o) = case parseEither parseResponse o of
  Right result -> Just result
  Left _ -> Nothing
  where
    parseResponse o' = do
      requestId <- o' .: "requestId"
      response <- o' .: "response"
      status <- response .: "status"
      headers <- optional (response .: "headers") >>= \case
        Just headerList -> Just <$> parseHeaders headerList
        Nothing -> pure Nothing
      pure (requestId, status, headers)
parseResponseStarted _ = Nothing

parseResponseCompleted :: Value -> Maybe (RequestId, Maybe Text)
parseResponseCompleted (Object o) = case parseEither parseResponse o of
  Right result -> Just result
  Left _ -> Nothing
  where
    parseResponse o' = do
      requestId <- o' .: "requestId"
      responseText <- optional (o' .: "response" >>= (.: "body") >>= (.: "value"))
      pure (requestId, responseText)
parseResponseCompleted _ = Nothing

parseFetchError :: Value -> Maybe (RequestId, Text)
parseFetchError (Object o) = case parseEither parseError o of
  Right result -> Just result
  Left _ -> Nothing
  where
    parseError o' = do
      requestId <- o' .: "requestId"
      errorText <- o' .: "errorText"
      pure (requestId, errorText)
parseFetchError _ = Nothing

parseHeaders :: Value -> Parser (Map Text Text)
parseHeaders (Array headers) = do
  headerPairs <- mapM parseHeader (toList headers)
  pure $ Map.fromList headerPairs
  where
    parseHeader (Object h) = do
      name <- h .: "name"
      value <- h .: "value"
      pure (name, value)
    parseHeader _ = fail "Invalid header format"
parseHeaders _ = fail "Headers should be an array"

optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- | Create a new empty NetworkActivityVar
newNetworkActivityVar :: MonadIO m => m NetworkActivityVar
newNetworkActivityVar = newTVarIO Map.empty

-- | Read the current network activity map
readNetworkActivity :: MonadIO m => NetworkActivityVar -> m (Map RequestId RequestInfo)
readNetworkActivity = readTVarIO

-- | Wait for network to be idle (no pending requests)
-- This is useful for waiting until all network activity has finished
waitForNetworkIdle :: MonadIO m => NetworkActivityVar -> m ()
waitForNetworkIdle nav = atomically $ do
  requests <- readTVar nav
  let pending = filter (not . requestInfoCompleted) (Map.elems requests)
  if null pending
    then pure ()
    else retry
