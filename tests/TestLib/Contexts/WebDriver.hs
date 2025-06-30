{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module TestLib.Contexts.WebDriver (
  introduceWebDriver
  ) where

import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
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
import Test.Sandwich.Contexts.Util.Ports
import Test.Sandwich.Waits
import Test.WebDriver.Commands
import Test.WebDriver.Types
import TestLib.Types
import TestLib.Types.Cli
import TestLib.Waits
import Text.Read (readMaybe)
import UnliftIO.Async
import UnliftIO.IORef
import UnliftIO.Process
import UnliftIO.Timeout


type BaseMonad m = (HasCallStack, MonadUnliftIO m)
type BaseMonadContext m context = (BaseMonad m, HasBaseContext context)

introduceWebDriver :: forall context m. (
  BaseMonadContext m context, MonadMask m
  , HasBrowserDependencies context
  , HasDriverType context
  , HasCommandLineOptions context UserOptions
  )
  => SpecFree (LabelValue "webdriver" WebDriverContext :> context) m () -> SpecFree context m ()
introduceWebDriver = introduceWith "Introduce WebDriver" webdriver withAlloc
  where
    withAlloc action = do
      Just dir <- getCurrentFolder
      webdriverDir <- liftIO $ createTempDirectory dir "webdriver"

      port <- findFreePortOrException

      (programName, args) <- getContext driverType >>= getArguments port webdriverDir
      debug [i|#{programName} #{T.unwords $ fmap T.pack args}|]

      (hRead, hWrite) <- createPipe

      let cp = (proc programName args) {
            create_group = True
            , std_out = UseHandle hWrite
            , std_err = UseHandle hWrite
            }

      withCreateProcess cp $ \_ _ _ _p -> do
        let hostname = "localhost"

        -- Read from the (combined) output stream until we see the up and running message
        dt <- getContext driverType
        maybeReady <- timeout 30_000_00 $ fix $ \loop -> do
          line <- fmap T.pack $ liftIO $ hGetLine hRead
          debug line
          case any (\x -> x `T.isInfixOf` line) (needles dt) of
            True -> return ()
            False -> loop
        when (maybeReady == Nothing) $
          expectationFailure [i|Didn't see ready message within 30s|]

        let webDriverContext = WebDriverContext dt hostname port

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


autodetectSeleniumVersionByFileName :: FilePath -> Maybe SeleniumVersion
autodetectSeleniumVersionByFileName (takeFileName -> seleniumJar) = case autodetectSeleniumMajorVersionByFileName of
  Just 3 -> Just Selenium3
  Just 4 -> Just Selenium4
  _ -> Nothing
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

-- autodetectSeleniumVersion :: FilePath -> FilePath -> m SeleniumVersion
-- autodetectSeleniumVersion _java _seleniumJar = undefined

getArguments :: (
  HasBrowserDependencies context, MonadReader context m, MonadLogger m
  ) => PortNumber -> FilePath -> DriverType -> m (FilePath, [String])
getArguments port webdriverDir (DriverTypeSeleniumJar java seleniumJar) = do
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

  let maybeSeleniumVersion = autodetectSeleniumVersionByFileName seleniumJar
  info [i|Detected Selenium version: #{maybeSeleniumVersion}|]
  let extraArgs = case maybeSeleniumVersion of
        Just Selenium3 -> ["-port", show port]
        Just Selenium4 -> ["standalone", "--port", show port, "--host", "localhost"]
        Nothing -> ["-port", show port]

  let fullArgs = javaArgs
               <> ["-jar", seleniumJar]
               <> extraArgs
  return (java, fullArgs)
getArguments port _webdriverDir (DriverTypeChromedriver chromedriver) = do
  return (chromedriver, ["--port=" <> show port])
getArguments port _webdriverDir (DriverTypeGeckodriver geckodriver) = do
  return (geckodriver, ["--port", show port])

needles :: DriverType -> [T.Text]
needles (DriverTypeSeleniumJar {}) = ["Selenium Server is up and running", "Started Selenium Standalone"]
needles (DriverTypeChromedriver {}) = ["ChromeDriver was started successfully"]
needles (DriverTypeGeckodriver {}) = ["Listening on"]
