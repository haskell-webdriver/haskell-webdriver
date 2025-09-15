{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Test.WebDriver
import Test.WebDriver.Capabilities
import Test.WebDriver.WD
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory
import UnliftIO.Exception


main :: IO ()
main = do
  -- You must have a compatible chrome/chromedriver on your PATH for this to work
  Just chromedriverBin <- findExecutable "chromedriver"
  Just chromeBin <- findExecutable "google-chrome-stable"

  let caps = defaultCaps {
        _capabilitiesGoogChromeOptions = Just $ defaultChromeOptions {
            _chromeOptionsBinary = Just chromeBin
            }
        }

  let driverConfig = DriverConfigChromedriver {
        driverConfigChromedriver = chromedriverBin
        , driverConfigChromedriverFlags = []
        , driverConfigChrome = chromeBin
        , driverConfigLogDir = Nothing
        }

  runStdoutLoggingT $ bracket mkEmptyWebDriverContext teardownWebDriverContext $ \wdc -> do
    sess <- startSession wdc driverConfig caps "session1"
    runWD sess $ do
      openPage "http://www.xkcd.com"
      liftIO $ threadDelay 5_000_000

    sess2 <- startSession wdc driverConfig caps "session2"
    runWD sess2 $ do
      openPage "http://www.smbc-comics.com"
      liftIO $ threadDelay 5_000_000
