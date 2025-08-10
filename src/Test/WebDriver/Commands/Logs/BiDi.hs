{-# LANGUAGE OverloadedStrings #-}

module Test.WebDriver.Commands.Logs.BiDi (
  withRecordBiDiLogs
  , withRecordBiDiLogs'
  ) where

import Control.Monad.IO.Unlift
import Data.String.Interpolate
import GHC.Stack
import Test.WebDriver.Commands.Logs.Common
import Test.WebDriver.Types
import UnliftIO.Exception


withRecordBiDiLogs :: (HasCallStack, WebDriver m) => (LogEntry -> m ()) -> m a -> m a
withRecordBiDiLogs cb action = do
  Session {..} <- getSession
  webSocketUrl <- case sessionWebSocketUrl of
    Nothing -> throwIO $ userError [i|Session wasn't configured with a BiDi WebSocket URL when trying to record logs. Make sure to enable _capabilitiesWebSocketUrl.|]
    Just x -> pure x

  withRecordBiDiLogs' webSocketUrl cb action

withRecordBiDiLogs' :: (HasCallStack, MonadUnliftIO m) => String -> (LogEntry -> m ()) -> m a -> m a
withRecordBiDiLogs' webSocketUrl cb action =
  -- TODO: use the Network.WebSockets library to connect to the given URL, inside a bracket.
  -- Use the BiDi protocol to subscribe to log entry events.
  -- use withAsync to start a background thread to listen for these, parse them, and call the callback with each one.
  undefined
