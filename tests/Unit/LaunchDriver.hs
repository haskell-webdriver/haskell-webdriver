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
      (starts, sessions) <- startSessionAfterFailure safaridriver "exit"
      length starts `shouldBe` 2
      length sessions `shouldBe` 2

    it "retries a Selenium session when its safaridriver exits before it's ready" $ do
      (starts, sessions) <- startSessionAfterFailure selenium
        "Could not start a new session. Error while creating session with the driver service. Stopping driver service: Driver server process died prematurely, exit value: 1"
      length starts `shouldBe` 1
      length sessions `shouldBe` 2

    it "retries a Selenium session when its safaridriver exits while starting it" $ do
      (starts, sessions) <- startSessionAfterFailure selenium
        "Could not start a new session. Error while creating session with the driver service. Stopping driver service: java.io.IOException: HTTP/1.1 header parser received no bytes "
      length starts `shouldBe` 1
      length sessions `shouldBe` 2
  where
    safaridriver mock dir env = DriverConfigSafaridriver {
      driverConfigSafaridriver = mock
      , driverConfigSafaridriverFlags = []
      , driverConfigSafaridriverExtraEnv = Just env
      , driverConfigLogDir = Just dir
      }

    selenium mock dir env = DriverConfigSeleniumJar {
      driverConfigJava = mock
      , driverConfigSeleniumJar = "selenium-server.jar"
      , driverConfigSubDrivers = []
      , driverConfigLogDir = Just dir
      , driverConfigJavaFlags = []
      , driverConfigJavaExtraEnv = Just env
      , driverConfigSeleniumVersion = Just Selenium4
      }

-- | Start and close a Safari session on the mock safaridriver, whose first new-session request fails
-- in the given way, and return the lines it wrote for each start and each new-session request.
startSessionAfterFailure :: (
  HasBaseContext context, HasFile context "mock-safaridriver"
  ) => (FilePath -> FilePath -> [(String, String)] -> DriverConfig) -> String -> ExampleT context IO ([String], [String])
startSessionAfterFailure mkDriverConfig failure = do
  Just dir <- getCurrentFolder
  mockSafaridriver <- askFile @"mock-safaridriver"
  let startsFile = dir </> "starts"
      sessionsFile = dir </> "sessions"
      dc = mkDriverConfig mockSafaridriver dir [
        ("MOCK_DRIVER_STARTS", startsFile)
        , ("MOCK_DRIVER_SESSIONS", sessionsFile)
        , ("MOCK_SAFARIDRIVER_FAILURE", failure)
        ]
  bracket mkEmptyWebDriverContext teardownWebDriverContext $ \wdc ->
    startSession wdc dc (defaultCaps { _capabilitiesBrowserName = Just Safari }) "session1" >>= closeSession wdc
  (,) <$> (lines <$> liftIO (readFile startsFile)) <*> (lines <$> liftIO (readFile sessionsFile))
