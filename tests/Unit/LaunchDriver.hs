{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Unit.LaunchDriver (
  tests
  ) where

import Control.Monad.IO.Class
import Data.List (isPrefixOf)
import System.FilePath ((</>))
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.WebDriver
import TestLib.Contexts.MockDriver
import TestLib.Types
import UnliftIO.Exception


tests :: NixSpec
tests = describe "Driver startup" $ introduceBinaryViaNixDerivation @"mock-driver" mockDriverDerivation $ do
  it "retries safaridriver on a new port when its port is taken" $
    portClashIsRetried $ \mockDriver dir env -> DriverConfigSafaridriver {
      driverConfigSafaridriver = mockDriver
      , driverConfigSafaridriverFlags = []
      , driverConfigSafaridriverExtraEnv = Just env
      , driverConfigLogDir = Just dir
      }

  it "retries chromedriver on a new port when its port is taken" $
    portClashIsRetried $ \mockDriver dir env -> DriverConfigChromedriver {
      driverConfigChromedriver = mockDriver
      , driverConfigChromedriverFlags = []
      , driverConfigChromedriverExtraEnv = Just env
      , driverConfigChrome = "unused"
      , driverConfigLogDir = Just dir
      }

-- | Start a session on the mock driver. Its first start hits a taken port, so launchDriver should
-- retry once, then report the second start's different failure.
portClashIsRetried :: (
  HasBaseContext context, HasFile context "mock-driver"
  ) => (FilePath -> FilePath -> [(String, String)] -> DriverConfig) -> ExampleT context IO ()
portClashIsRetried mkDriverConfig = do
  Just dir <- getCurrentFolder
  mockDriver <- askFile @"mock-driver"
  let startsFile = dir </> "starts"
  wdc <- mkEmptyWebDriverContext
  Left (e :: SomeException) <- try $
    startSession wdc (mkDriverConfig mockDriver dir [("MOCK_DRIVER_STARTS", startsFile)]) defaultCaps "session1"
  show e `shouldBe` "DriverOutputEndedBeforeReady (Just (ExitFailure 2)) [\"mock-driver: failed for another reason\"]"
  starts <- lines <$> liftIO (readFile startsFile)
  length starts `shouldBe` 2
  all ("--port" `isPrefixOf`) starts `shouldBe` True
