{-# LANGUAGE NumericUnderscores #-}

module Spec.SafariSpecific.StuckSessions where

import Data.Aeson (Value)
import Test.Sandwich
import Test.WebDriver
import TestLib.Types
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.Timeout


tests :: SessionSpec
tests = describe "Stuck sessions" $
  it "starts a session after one that couldn't be closed" $ getContext driverConfig >>= \case
    dc@(DriverConfigSafaridriver {}) -> do
      caps <- getContext browserDependencies >>= getCapabilities False
      bracket mkEmptyWebDriverContext teardownWebDriverContext $ \wdc -> do
        stuck <- startSession wdc dc caps "session1"
        -- safaridriver holds back every later command for the session, DELETE included, until a
        -- script that never calls back times out.
        pushContext session stuck $ setScriptTimeout (Just 60_000)
        withAsync (pushContext session stuck $ asyncJS [] "" >>= \(_ :: Maybe Value) -> return ()) $ \_ -> do
          threadDelay 1_000_000
          timeout 2_000_000 (closeSession wdc stuck) >>= (`shouldBe` Nothing)

        next <- startSession wdc dc caps "session1"
        pushContext session next $ openPage "about:blank"
        closeSession wdc next
    _ -> pending
