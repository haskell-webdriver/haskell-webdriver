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
tests = do
  driverStartup
  safariSessionStartup

driverStartup :: NixSpec
driverStartup = describe "Driver startup" $ introduceBinaryViaNixDerivation @"mock-driver" mockDriverDerivation $ do
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

safariSessionStartup :: NixSpec
safariSessionStartup = describe "Safari session startup" $
  introduceBinaryViaNixDerivation @"mock-safaridriver" mockSafaridriverDerivation $ do
    it "launches a new safaridriver when one exits while starting a session" $ do
      (starts, sessions) <- startSessionAfterFailure
      length starts `shouldBe` 2
      length sessions `shouldBe` 2

-- | Start and close a session on the mock safaridriver, which exits on its first new-session
-- request, and return the lines it wrote for each start and each new-session request.
startSessionAfterFailure :: (
  HasBaseContext context, HasFile context "mock-safaridriver"
  ) => ExampleT context IO ([String], [String])
startSessionAfterFailure = do
  Just dir <- getCurrentFolder
  mockSafaridriver <- askFile @"mock-safaridriver"
  let startsFile = dir </> "starts"
      sessionsFile = dir </> "sessions"
      dc = DriverConfigSafaridriver {
        driverConfigSafaridriver = mockSafaridriver
        , driverConfigSafaridriverFlags = []
        , driverConfigSafaridriverExtraEnv = Just [
            ("MOCK_DRIVER_STARTS", startsFile)
            , ("MOCK_DRIVER_SESSIONS", sessionsFile)
            ]
        , driverConfigLogDir = Just dir
        }
  bracket mkEmptyWebDriverContext teardownWebDriverContext $ \wdc ->
    startSession wdc dc defaultCaps "session1" >>= closeSession wdc
  (,) <$> (lines <$> liftIO (readFile startsFile)) <*> (lines <$> liftIO (readFile sessionsFile))
