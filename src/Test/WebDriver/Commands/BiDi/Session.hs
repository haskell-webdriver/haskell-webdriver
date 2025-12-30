{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Test.WebDriver.Commands.BiDi.Session (
  withBiDiSession
  , withBiDiSession'

  , BiDiEvent(..)
  , BiDiResponse(..)
  ) where

import Control.Monad (forever)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Unlift
import Control.Monad.Logger (MonadLogger, logDebugN, logErrorN)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text (Text)
import qualified Network.URI as URI
import qualified Network.WebSockets as WS
import Test.WebDriver.Capabilities.Aeson
import Test.WebDriver.Types
import Text.Read (readMaybe)
import UnliftIO.Async (withAsync)
import UnliftIO.Exception
import UnliftIO.STM (atomically, stateTVar)
import UnliftIO.Timeout (timeout)


data BiDiEvent = BiDiEvent {
  biDiType :: Text
  , biDiMethod :: Text
  , biDiParams :: Value
  } deriving Show
deriveFromJSON toCamel2 ''BiDiEvent

data BiDiResponse = BiDiResponse {
  biDiResponseType :: Text
  , biDiResponseId :: Int
  , biDiResponseResult :: Maybe Value
  , biDiResponseError :: Maybe Value
  } deriving Show
deriveFromJSON toCamel3 ''BiDiResponse

-- | Wrapper around 'withBiDiSession'' which uses the WebSocket URL from
-- the current 'Session'. You must make sure to pass '_capabilitiesWebSocketUrl'
-- = @Just True@ to enable this. This will not work with Selenium 3.
withBiDiSession :: (WebDriver m, MonadLogger m) => [Text] -> (BiDiEvent -> m ()) -> m a -> m a
withBiDiSession events cb action = do
  Session {..} <- getSession
  webSocketUrl <- case sessionWebSocketUrl of
    Nothing -> throwIO $ userError [i|Session wasn't configured with a BiDi WebSocket URL when trying to record logs. Make sure to enable _capabilitiesWebSocketUrl.|]
    Just x -> pure x

  uri <- case URI.parseURI webSocketUrl of
    Just x -> pure x
    Nothing -> throwIO $ userError [i|Couldn't parse WebSocket URL: #{webSocketUrl}|]

  bidiSessionId <- atomically $ stateTVar sessionIdCounter (\x -> (x, x + 1))

  withBiDiSession' bidiSessionId uri events cb action

-- | Connect to WebSocket URL and subscribe to the given events using the W3C BiDi protocol; see
-- <https://w3c.github.io/webdriver-bidi/>.
withBiDiSession' :: (MonadUnliftIO m, MonadLogger m) => Int -> URI.URI -> [Text] -> (BiDiEvent -> m ()) -> m a -> m a
withBiDiSession' bidiSessionId uri@(URI.URI { uriAuthority=(Just (URI.URIAuth {uriPort=(readMaybe . L.drop 1 -> Just (port :: Int)), ..})), .. }) events cb action = do
  logDebugN [i|BiDi: Connecting to #{uriRegName}:#{port}#{uriPath}|]

  withRunInIO $ \runInIO -> liftIO $ WS.runClient uriRegName port uriPath $ \conn -> runInIO $ do
    logDebugN [i|BiDi: Connected successfully, sending subscription request with ID #{bidiSessionId}|]
    liftIO $ WS.sendTextData conn $ encode $ object [
      "id" .= bidiSessionId
      , "method" .= ("session.subscribe" :: Text)
      , "params" .= object [
          "events" .= (events :: [Text])
        ]
      ]

    logDebugN "BiDi: Sent subscription request, waiting for response..."
    timeout 15_000_000 (waitForSubscriptionResult bidiSessionId conn) >>= \case
      Nothing -> throwIO $ userError "BiDi: Subscription response timed out"
      Just (Left err) ->
        throwIO $ userError [i|BiDi: got exception (URI #{uri}): #{err}|]
      Just (Right ()) -> do
        logDebugN "BiDi: Starting log event listener"
        withAsync (messageListener conn) $ \_messageListenerAsy -> do
          finally action $ do
            logDebugN [i|BiDi: finished wrapped action|]
            liftIO $ WS.sendClose conn ([i|Finishing session #{bidiSessionId}|] :: Text)
  where
    messageListener conn =
      forever $
        (decode <$>) (liftIO $ WS.receiveData conn) >>= \case
          Just (x :: BiDiEvent) -> cb x
          x -> logDebugN [i|BiDi: Ignoring non-log event message: #{x}|]
withBiDiSession' _ uri _events _cb _action =
  throwIO $ userError [i|WebSocket URL didn't contain an authority: #{uri}|]


waitForSubscriptionResult :: (
  MonadIO m, MonadLogger m
  ) => Int -> WS.Connection -> m (Either SomeException ())
waitForSubscriptionResult bidiSessionId conn = fix $ \loop -> do
  msg <- liftIO $ WS.receiveData conn
  logDebugN [i|BiDi: Waiting for subscription response: #{msg}|]
  case decode msg of
    Just response@(BiDiResponse responseType responseId _ _)
      | responseType == "success" && responseId == bidiSessionId -> do
          logDebugN "BiDi: Subscription successful!"
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

-- * Better WebSocket ping/pong
-- | I've run into problems with the ping/pong support in the websockets
-- library, so I came up with this alternative version that I use in my
-- websockets projects.
--
-- But, according to some searching, it seems that the BiDi client doesn't need
-- to do ping/pong, as we can count on the browser driver to do it. So leaving
-- this unused for now. It might still be useful to use at some point to catch
-- dead drivers etc.

-- data BetterPongTimeout =
--   BetterPongTimeoutUnexpectedResponse BL.ByteString
--   | BetterPongTimeoutNoResponse
--   deriving Show
-- instance Exception BetterPongTimeout

-- data PingPongOptions = PingPongOptions {
--   pingInterval :: Int -- ^ Interval in seconds
--   , pongTimeout :: Int -- ^ Timeout in seconds
--   , pingAction :: Int -> IO () -- ^ Action to perform after sending a ping
--   , pingMessage :: Text -> IO () -- ^ Message to log
-- }
-- defaultPingPongOptions :: PingPongOptions
-- defaultPingPongOptions = PingPongOptions {
--   pingInterval = 15
--   , pongTimeout = 30
--   , pingAction = const $ return ()
--   , pingMessage = const $ return ()
-- }

-- withBetterPingPong :: MonadUnliftIO m => PingPongOptions -> WS.Connection -> (WS.Connection -> m ()) -> m ()
-- withBetterPingPong (PingPongOptions {..}) connection app = void $
--   withAsync (app connection) $ \appAsync -> do
--     withAsync (liftIO pingPongThread) $ \pingAsync -> do
--       waitAnyCancel [appAsync, pingAsync]
--     where
--       pingPongThread = do
--         -- Make sure the heartbeat MVar is empty
--         _ <- tryTakeMVar (WS.connectionHeartbeat connection)

--         flip withException reportPingPongException $ flip fix (0 :: Int) $ \loop n -> do
--           let bytes :: BL.ByteString = Builder.toLazyByteString $ Builder.int64BE $ fromIntegral n

--           WS.sendPing connection bytes

--           timeout (pongTimeout * 1000 * 1000) (takeMVar (WS.connectionHeartbeat connection)) >>= \case
--             Just _ -> return ()
--             Nothing -> throwIO $ BetterPongTimeoutNoResponse

--           threadDelay (pongTimeout * 1000 * 1000)
--           loop (n + 1)

--       reportPingPongException :: SomeException -> IO ()
--       reportPingPongException (fromException -> Just (AsyncCancelled {})) = return ()
--       reportPingPongException e = pingMessage [i|Ping pong thread had exception: #{e}|]
