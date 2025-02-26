{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module TestLib.Contexts.WebDriver where

import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Retry
import Data.Function
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import GHC.Stack
import Network.Socket
import System.FilePath
import System.IO (hGetLine)
import System.IO.Temp
import Test.Sandwich hiding (BrowserToUse(..))
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Util.Ports
import Test.Sandwich.Waits
import Test.WebDriver.Commands
import Test.WebDriver.Config
import TestLib.Types
import TestLib.Types.Cli
import TestLib.Waits
import Text.Read (readMaybe)
import UnliftIO.Async
import UnliftIO.IORef
import UnliftIO.Process


type BaseMonad m = (HasCallStack, MonadUnliftIO m)
type BaseMonadContext m context = (BaseMonad m, HasBaseContext context)

data SeleniumVersion =
  Selenium3
  | Selenium4
  | SeleniumUnknown
  deriving (Show, Eq)

introduceWebDriver :: forall context m. (
  BaseMonadContext m context, MonadMask m
  , HasFile context "selenium.jar"
  , HasFile context "java"
  , HasBrowserDependencies context
  , HasCommandLineOptions context UserOptions
  )
  => SpecFree (LabelValue "webdriver" WebDriverContext :> context) m () -> SpecFree context m ()
introduceWebDriver = introduceWith "Introduce WebDriver" webdriver withAlloc
  where
    withAlloc action = do
      Just dir <- getCurrentFolder
      webdriverDir <- liftIO $ createTempDirectory dir "webdriver"

      javaArgs :: [String] <- getContext browserDependencies >>= \case
        BrowserDependenciesChrome {..} -> do
          let chromedriverLog = webdriverDir </> "chromedriver.log"
          return ([
            "-Dwebdriver.chrome.logfile=" <> chromedriverLog
            , "-Dwebdriver.chrome.verboseLogging=true"
            , "-Dwebdriver.chrome.driver=" <> browserDependenciesChromeChromedriver
            ])
        BrowserDependenciesFirefox {..} -> do
          return ([
            "-Dwebdriver.gecko.driver=" <> browserDependenciesFirefoxGeckodriver
            ])

      java <- askFile @"java"
      seleniumJar <- askFile @"selenium.jar"

      port <- findFreePortOrException

      let seleniumVersion = autodetectSeleniumVersionByFileName seleniumJar
      info [i|Detected Selenium version: #{seleniumVersion}|]
      let extraArgs = case seleniumVersion of
            Selenium3 -> ["-port", show port]
            Selenium4 -> ["standalone", "--port", show port]
            SeleniumUnknown -> ["-port", show port]

      let fullArgs = javaArgs
                   <> ["-jar", seleniumJar]
                   <> extraArgs
      debug [i|#{java} #{T.unwords $ fmap T.pack fullArgs}|]

      (hRead, hWrite) <- createPipe

      let cp = (proc java fullArgs) {
            create_group = True
            , std_out = UseHandle hWrite
            , std_err = UseHandle hWrite
            }

      withCreateProcess cp $ \_ _ _ _p -> do
        let hostname = "localhost"

        -- Read from the (combined) output stream until we see the up and running message
        fix $ \loop -> do
          line <- fmap T.pack $ liftIO $ hGetLine hRead
          debug line

          case "Selenium Server is up and running" `T.isInfixOf` line || "Started Selenium Standalone" `T.isInfixOf` line of
            True -> return ()
            False -> loop

        let webDriverContext = WebDriverContext hostname port

        withAsync (forever $ liftIO (hGetLine hRead) >>= (debug . T.pack)) $ \_ -> do
          -- Wait for a successful connectino to the server socket
          addr:_ <- liftIO $ getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just (show port))
          let policy = limitRetriesByCumulativeDelay (120 * 1_000_000) $ capDelay 5_000_000 $ exponentialBackoff 1000
          waitForSocket policy addr

          -- Wait for a successful call to /status
          waitUntil 120 $ do
            browserDeps <- getContext browserDependencies

            wdConfig <- getWDConfig' webDriverContext browserDeps
            baseSessionVar <- mkSession wdConfig >>= newIORef

            pushContext wdSession baseSessionVar $ do
              status <- serverStatus
              info [i|WebDriver server status: #{status}|]

          void $ action webDriverContext


autodetectSeleniumVersionByFileName :: FilePath -> SeleniumVersion
autodetectSeleniumVersionByFileName (takeFileName -> seleniumJar) = case autodetectSeleniumMajorVersionByFileName of
  Just 3 -> Selenium3
  Just 4 -> Selenium4
  _ -> SeleniumUnknown
  where
    autodetectSeleniumMajorVersionByFileName :: Maybe Int
    autodetectSeleniumMajorVersionByFileName
      | not ("selenium-server-" `L.isPrefixOf` seleniumJar) = Nothing
      | not (".jar" `L.isSuffixOf` seleniumJar) = Nothing
      | otherwise = do
          let parts = seleniumJar
                    & drop (length ("selenium-server-" :: String))
                    & T.dropEnd (length (".jar" :: String)) . T.pack
                    & T.splitOn "."
                    & fmap T.unpack
                    & fmap readMaybe

          case any (== Nothing) parts of
            True -> Nothing
            False -> case catMaybes parts of
              [x, _, _, _] -> Just x
              [x, _, _] -> Just x
              [x, _] -> Just x
              [x] -> Just x
              _ -> Nothing

autodetectSeleniumVersion :: FilePath -> FilePath -> m SeleniumVersion
autodetectSeleniumVersion _java _seleniumJar = undefined

seleniumOutFileName, seleniumErrFileName :: FilePath
seleniumOutFileName = "stdout.txt"
seleniumErrFileName = "stderr.txt"
