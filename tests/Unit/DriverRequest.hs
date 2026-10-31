{-# LANGUAGE NumericUnderscores #-}

module Unit.DriverRequest (
  tests
  ) where

import qualified Data.Aeson as A
import Network.HTTP.Client (responseTimeout, responseTimeoutDefault, responseTimeoutMicro)
import Network.HTTP.Types (methodGet, methodPost)
import Test.Sandwich
import Test.WebDriver


tests :: SpecFree context IO ()
tests = describe "Driver requests" $ do
  it "gives a request to start a Safari session 2 minutes" $ do
    driver <- mkManualDriver "localhost" 4444 "" []
    responseTimeout (newSession driver Safari) `shouldBe` responseTimeoutMicro 120_000_000

  it "gives other requests http-client's default" $ do
    driver <- mkManualDriver "localhost" 4444 "" []
    responseTimeout (newSession driver Chrome) `shouldBe` responseTimeoutDefault
    responseTimeout (mkDriverRequest driver methodGet "/status" A.Null) `shouldBe` responseTimeoutDefault

  where
    -- The request startSession sends.
    newSession driver browser = mkDriverRequest driver methodPost "/session" $
      A.object [("capabilities", A.object [("alwaysMatch", A.toJSON defaultCaps { _capabilitiesBrowserName = Just browser })])]
