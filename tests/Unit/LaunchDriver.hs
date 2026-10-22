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
  -- launchDriver gives up after 10 retries.
  it "retries safaridriver whenever it exits before it's ready" $
    startsBeforeGivingUp 11 $ \mockDriver dir env -> DriverConfigSafaridriver {
      driverConfigSafaridriver = mockDriver
      , driverConfigSafaridriverFlags = []
      , driverConfigSafaridriverExtraEnv = Just env
      , driverConfigLogDir = Just dir
      }

  it "retries chromedriver on a new port when its port is taken" $
    startsBeforeGivingUp 2 $ \mockDriver dir env -> DriverConfigChromedriver {
      driverConfigChromedriver = mockDriver
      , driverConfigChromedriverFlags = []
      , driverConfigChromedriverExtraEnv = Just env
      , driverConfigChrome = "unused"
      , driverConfigLogDir = Just dir
      }

-- | Start a session on the mock driver, which hits a taken port on its first start and fails for
-- another reason after that. Check that launchDriver started it the given number of times and then
-- reported the last start's failure.
startsBeforeGivingUp :: (
  HasBaseContext context, HasFile context "mock-driver"
  ) => Int -> (FilePath -> FilePath -> [(String, String)] -> DriverConfig) -> ExampleT context IO ()
startsBeforeGivingUp expectedStarts mkDriverConfig = do
  Just dir <- getCurrentFolder
  mockDriver <- askFile @"mock-driver"
  let startsFile = dir </> "starts"
  wdc <- mkEmptyWebDriverContext
  Left (e :: SomeException) <- try $
    startSession wdc (mkDriverConfig mockDriver dir [("MOCK_DRIVER_STARTS", startsFile)]) defaultCaps "session1"
  show e `shouldBe` "DriverOutputEndedBeforeReady (Just (ExitFailure 2)) [\"mock-driver: failed for another reason\"]"
  starts <- lines <$> liftIO (readFile startsFile)
  length starts `shouldBe` expectedStarts
  all ("--port" `isPrefixOf`) starts `shouldBe` True
