{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module TestLib.WebDriverContext where

import Control.Lens
import Control.Lens.Regex.Text
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Data.Function
import qualified Data.Text as T
import Safe
import System.FilePath
import System.IO (hGetLine)
import System.IO.Temp
import Test.Sandwich hiding (BrowserToUse(..))
import Test.Sandwich.Contexts.Files
import TestLib.Types
import UnliftIO.Async
import UnliftIO.Process


introduceWebDriver :: forall context m. (
  BaseMonadContext m context, HasCommandLineOptions context UserOptions, MonadThrow m
  , HasFile context "selenium.jar"
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

      seleniumJar <- askFile @"selenium.jar"

      (hRead, hWrite) <- createPipe

      let cp = (proc "java" (javaArgs <> ["-jar", seleniumJar])) {
            create_group = True
            , std_out = UseHandle hWrite
            , std_err = UseHandle hWrite
            }

      withCreateProcess cp $ \_ _ _ _p -> do
        let hostname = "localhost"

        -- Read from the (combined) output stream until we see the up and running message
        port <- fix $ \loop -> do
          line <- fmap T.pack $ liftIO $ hGetLine hRead
          debug line

          case line ^.. [regex|Selenium Server is up and running on port (\d+)|] . group 0 of
            [(readMay . T.unpack) -> Just port] -> pure port
            _ -> loop

        withAsync (forever $ liftIO (hGetLine hRead) >>= (debug . T.pack)) $ \_ ->
          void $ action $ WebDriver hostname port



seleniumOutFileName, seleniumErrFileName :: FilePath
seleniumOutFileName = "stdout.txt"
seleniumErrFileName = "stderr.txt"
