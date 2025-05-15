{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.SeleniumSpecific.Mobile where

import Test.Sandwich
-- import Test.WebDriver
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types
import UnliftIO.Concurrent


tests :: SessionSpec
tests = introduceMobileSession $ describe "Mobile" $ before "Open test page" openSimpleTestPage $ do
  it "Pauses" $ do
    -- threadDelay 999_999_000_000
    pending
