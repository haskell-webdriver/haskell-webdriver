
module Spec.Sessions where

import Data.String.Interpolate
import Test.Sandwich
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Sessions" $ before "Open test page" openSimpleTestPage $ do
  -- createSession is already tested in introduceSession

  it "status" $ do
    status <- serverStatus
    info [i|Got status: #{status}|]

  -- TODO: test closeSession without breaking the context
  it "closeSession" $ do
    pending

  describe "Timeouts" $ do
    describe "Bulk timeout functions" $ do
      it "getTimeouts" $ do
        timeouts <- getTimeouts
        info [i|Got timeouts: #{timeouts}|]

      it "setTimeouts" $ do
        let timeouts = Timeouts {
              timeoutsScript = Just 1
              , timeoutsPageLoad = Just 2
              , timeoutsImplicit = Just 3
              }
        setTimeouts timeouts

        getTimeouts >>= (`shouldBe` timeouts)

    describe "Individual timeout functions" $ do
      it "setScriptTimeout" $ do
        setScriptTimeout (Just 11)
        getTimeouts >>= (`shouldBe` (Timeouts (Just 11) (Just 2) (Just 3)))

        setScriptTimeout Nothing
        getTimeouts >>= (`shouldBe` (Timeouts Nothing (Just 2) (Just 3)))

      it "setPageLoadTimeout" $ do
        setPageLoadTimeout 12
        getTimeouts >>= (`shouldBe` (Timeouts Nothing (Just 12) (Just 3)))

      it "setImplicitWait" $ do
        setImplicitWait 13
        getTimeouts >>= (`shouldBe` (Timeouts Nothing (Just 12) (Just 13)))

  -- it "sessions" $ do
  --   xs <- sessions
  --   info [i|Got sessions: #{xs}|]

  -- it "getActualCaps" $ do
  --   caps <- getActualCaps
  --   info [i|Got actual caps: #{caps}|]
