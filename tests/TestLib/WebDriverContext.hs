
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TestLib.WebDriverContext where

import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import System.FilePath
import System.IO.Temp
import Test.Sandwich hiding (BrowserToUse(..))
import TestLib.Types
import UnliftIO.Process


introduceWebDriver :: forall context m. (
  BaseMonadContext m context, HasCommandLineOptions context UserOptions, MonadThrow m
  )
  => () -> SpecFree (LabelValue "webdriver" WebDriver :> context) m () -> SpecFree context m ()
introduceWebDriver _wdOptions = introduceWith "Introduce WebDriver" webdriver withAlloc
  where
    withAlloc action = do
      UserOptions {..} <- getUserCommandLineOptions
      Just dir <- getCurrentFolder
      webdriverDir <- liftIO $ createTempDirectory dir "webdriver"

      javaArgs :: [String] <- case optBrowserToUse of
        UseChrome -> do
          let chromedriverLog = webdriverDir </> "chromedriver.log"
          return ([
            "-Dwebdriver.chrome.logfile=" <> chromedriverLog
            , "-Dwebdriver.chrome.verboseLogging=true"
            ]
            <> (maybe [] (\x -> ["-Dwebdriver.chrome.driver=" <> x]) optChromeDriver)
            <> maybe [] (\x -> ["-Dwebdriver.chrome.driver=" <> x]) optChromeBinary
            )
        UseFirefox -> return []

      let cp = (proc "java" (javaArgs <> ["-jar", optSeleniumJar])) { create_group = True }

      withCreateProcess cp $ \_ hout herr p -> do
        let hostname = "localhost"
        port <- undefined
        void $ action $ WebDriver hostname port
