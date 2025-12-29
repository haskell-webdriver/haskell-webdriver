{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}

module Test.WebDriver.Commands.BiDi.NetworkActivity (
  withRecordNetworkActivityViaBiDi
  , withRecordNetworkActivityViaBiDi'

  , newNetworkActivityVar

  , readNetworkActivity
  , waitForNetworkIdle
  , waitForNetworkIdleForPeriod

  -- * Types
  , NetworkActivityVar
  , RequestInfo(..)
  , RequestId
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (retry)
import Control.Monad (unless)
import Control.Monad.IO.Unlift
import Control.Monad.Logger (MonadLogger, logDebugN, logWarnN)
import Data.Aeson
import Data.Aeson.Types (parseEither, Parser)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text (Text)
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Network.URI as URI
import Test.WebDriver.Commands.BiDi.Session
import Test.WebDriver.Types
import UnliftIO.Concurrent
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

data NetworkActivity = NetworkActivity {
  networkActivityRequests :: Map RequestId RequestInfo
  , networkActivityLastActivityTime :: UTCTime
  } deriving (Eq)

type NetworkActivityVar = TVar NetworkActivity

-- | Wrapper around 'withRecordLogsViaBiDi'' which uses the WebSocket URL from
-- the current 'Session'. You must make sure to pass '_capabilitiesWebSocketUrl'
-- = @Just True@ to enable this. This will not work with Selenium 3.
withRecordNetworkActivityViaBiDi :: (WebDriver m, MonadLogger m) => (NetworkActivityVar -> m a) -> m a
withRecordNetworkActivityViaBiDi action = do
  networkActivityVar <- newNetworkActivityVar
  withBiDiSession networkEvents (mkCallback networkActivityVar) (action networkActivityVar)

-- | Connect to WebSocket URL and subscribe to network events using the W3C BiDi protocol; see
-- <https://w3c.github.io/webdriver-bidi/>.
withRecordNetworkActivityViaBiDi' :: forall m a. (MonadUnliftIO m, MonadLogger m) => Int -> URI.URI -> (NetworkActivityVar -> m a) -> m a
withRecordNetworkActivityViaBiDi' bidiSessionId uri action = do
  networkActivityVar <- newNetworkActivityVar
  withBiDiSession' bidiSessionId uri networkEvents (mkCallback networkActivityVar) (action networkActivityVar)

mkCallback :: (MonadIO m, MonadLogger m) => NetworkActivityVar -> BiDiEvent -> m ()
mkCallback nav (BiDiEvent "event" "network.beforeRequestSent" params) = do
  case parseBeforeRequestSent params of
    Just (requestId, method, url, timestamp, headers) -> do
      now <- liftIO getCurrentTime
      atomically $ modifyTVar nav $ \na -> na {
        networkActivityRequests = M.insert requestId (RequestInfo {
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
          }) (networkActivityRequests na)
        , networkActivityLastActivityTime = now
        }
      logDebugN [i|BiDi: Network request started: #{requestId} #{method} #{url}|]
    Nothing -> logWarnN "BiDi: Failed to parse network.beforeRequestSent event"

mkCallback nav (BiDiEvent "event" "network.responseStarted" params) = do
  case parseResponseStarted params of
    Just (requestId, status, headers) -> do
      now <- liftIO getCurrentTime
      maybeRequestInfo <- atomically $ do
        na <- readTVar nav

        let (ret, requests') = adjustAndReturnNew requestId (networkActivityRequests na) $ \ri -> ri {
              requestInfoResponseStatus = Just status
              , requestInfoResponseHeaders = headers
              }

        modifyTVar nav $ \na' -> na' {
          networkActivityRequests = requests'
          , networkActivityLastActivityTime = now
          }

        return ret
      logDebugN [i|BiDi: Network response started: #{requestId} status #{status} (#{maybe "<unknown>" requestInfoUrl maybeRequestInfo})|]
    Nothing -> logWarnN "BiDi: Failed to parse network.responseStarted event"

mkCallback nav (BiDiEvent "event" "network.responseCompleted" params) = do
  case parseResponseCompleted params of
    Just (requestId, responseText) -> do
      now <- liftIO getCurrentTime
      maybeRequestInfo <- atomically $ do
        na <- readTVar nav

        let (ret, requests') = adjustAndReturnNew requestId (networkActivityRequests na) $ \ri -> ri {
              requestInfoResponseText = responseText
              , requestInfoCompleted = True
              }

        modifyTVar nav $ \na' -> na' {
          networkActivityRequests = requests'
          , networkActivityLastActivityTime = now
          }

        return ret

      logDebugN [i|BiDi: Network response completed: #{requestId} (#{maybe "<unknown>" requestInfoUrl maybeRequestInfo})|]
    Nothing -> logWarnN "BiDi: Failed to parse network.responseCompleted event"

mkCallback nav (BiDiEvent "event" "network.fetchError" params) = do
  case parseFetchError params of
    Just (requestId, errorText) -> do
      now <- liftIO getCurrentTime
      maybeRequestInfo <- atomically $ do
        na <- readTVar nav

        let (ret, requests') = adjustAndReturnNew requestId (networkActivityRequests na) $ \ri -> ri {
              requestInfoErrorText = Just errorText
              , requestInfoCompleted = True
              }

        modifyTVar nav $ \na' -> na' {
          networkActivityRequests = requests'
          , networkActivityLastActivityTime = now
          }

        return ret

      logDebugN [i|BiDi: Network fetch error: #{requestId} - #{errorText} (#{maybe "<unknown>" requestInfoUrl maybeRequestInfo})|]
    Nothing -> logWarnN "BiDi: Failed to parse network.fetchError event"

mkCallback _nav x = logDebugN [i|BiDi: Ignoring event: #{x}|]

parseBeforeRequestSent :: Value -> Maybe (RequestId, Text, Text, UTCTime, Maybe (Map Text Text))
parseBeforeRequestSent (Object o) = case parseEither parseRequest o of
  Right result -> Just result
  Left _ -> Nothing
  where
    parseRequest o' = do
      request <- o' .: "request"
      requestId <- request .: "request"  -- The requestId is in request.request
      method <- request .: "method"
      url <- request .: "url"
      timestamp <- o' .: "timestamp" :: Parser Integer
      headers <- optional (request .: "headers") >>= \case
        Just headerList -> Just <$> parseHeaders headerList
        Nothing -> pure Nothing
      let utcTime = posixSecondsToUTCTime (realToFrac timestamp / 1000)
      pure (requestId, method, url, utcTime, headers)
parseBeforeRequestSent _ = Nothing

parseResponseStarted :: Value -> Maybe (RequestId, Int, Maybe (Map Text Text))
parseResponseStarted (Object o) = case parseEither parseResponse o of
  Right result -> Just result
  Left _ -> Nothing
  where
    parseResponse o' = do
      request <- o' .: "request"
      requestId <- request .: "request"  -- The requestId is in request.request
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
      request <- o' .: "request"
      requestId <- request .: "request"  -- The requestId is in request.request
      responseText <- optional (o' .: "response" >>= (.: "body") >>= (.: "value"))
      pure (requestId, responseText)
parseResponseCompleted _ = Nothing

parseFetchError :: Value -> Maybe (RequestId, Text)
parseFetchError (Object o) = case parseEither parseError o of
  Right result -> Just result
  Left _ -> Nothing
  where
    parseError o' = do
      request <- o' .: "request"
      requestId <- request .: "request"  -- The requestId is in request.request
      errorText <- o' .: "errorText"
      pure (requestId, errorText)
parseFetchError _ = Nothing

parseHeaders :: Value -> Parser (Map Text Text)
parseHeaders (Array headers) = do
  headerPairs <- mapM parseHeader (toList headers)
  pure $ M.fromList headerPairs
  where
    parseHeader (Object h) = do
      name <- h .: "name"
      valueObj <- h .: "value"
      value <- case valueObj of
        Object vo -> vo .: "value"  -- Extract value from {type: "string", value: "..."}
        String s -> pure s          -- Fallback for direct string values
        _ -> fail "Invalid header value format"
      pure (name, value)
    parseHeader _ = fail "Invalid header format"
parseHeaders _ = fail "Headers should be an array"

optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

newNetworkActivityVar :: MonadIO m => m NetworkActivityVar
newNetworkActivityVar = do
  now <- liftIO getCurrentTime
  newTVarIO $ NetworkActivity M.empty now

-- | Read the current network activity map
readNetworkActivity :: MonadIO m => NetworkActivityVar -> m (Map RequestId RequestInfo)
readNetworkActivity nav = networkActivityRequests <$> readTVarIO nav

-- | Wait for network to be idle (no pending requests).
waitForNetworkIdle :: MonadIO m => NetworkActivityVar -> m ()
waitForNetworkIdle nav = atomically $ do
  na <- readTVar nav
  let pending = filter (not . requestInfoCompleted) (M.elems (networkActivityRequests na))
  unless (null pending) retry

-- | Wait for network to be idle with a delay after the last activity
-- This waits until:
-- 1. There are no outstanding requests AND
-- 2. No request has started or finished in the last N microseconds
waitForNetworkIdleForPeriod :: MonadIO m => NetworkActivityVar -> NominalDiffTime -> m ()
waitForNetworkIdleForPeriod nav idleTime = do
  lastActivityTime <- atomically $ do
    na <- readTVar nav
    let pending = filter (not . requestInfoCompleted) (M.elems (networkActivityRequests na))
    unless (null pending) retry

    return (networkActivityLastActivityTime na)

  now <- liftIO getCurrentTime
  let timeSinceLastActivity = diffUTCTime now lastActivityTime
  if | timeSinceLastActivity >= idleTime ->
         return ()
     | otherwise -> do
         threadDelay $ nominalDiffTimeToMicroseconds (idleTime - timeSinceLastActivity)
         waitForNetworkIdleForPeriod nav idleTime
  where
    nominalDiffTimeToMicroseconds :: NominalDiffTime -> Int
    nominalDiffTimeToMicroseconds t = round (t * 1000000)

adjustAndReturnNew :: Ord k => k -> M.Map k a -> (a -> a) -> (Maybe a, M.Map k a)
adjustAndReturnNew k m f = M.alterF alter k m
  where
    alter Nothing = (Nothing, Nothing)
    alter (Just v) = let v' = f v in (Just v', Just v')
