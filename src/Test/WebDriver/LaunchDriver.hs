{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Test.WebDriver.LaunchDriver (
  launchDriver
  , teardownDriver
  , mkDriverRequest
  ) where

import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Retry
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Function
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Data.Time
import Network.HTTP.Client
import Network.HTTP.Types (hAccept, hContentType, statusCode)
import Network.Socket
import System.FilePath
import System.Exit (ExitCode)
import System.IO
import System.IO.Error (isEOFError)
import Test.WebDriver.Types
import Test.WebDriver.Util.Ports
import Test.WebDriver.Util.Sockets
import Text.Read (readMaybe)
import UnliftIO.Async
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Environment
import UnliftIO.Exception
import UnliftIO.IORef
import UnliftIO.Process
import UnliftIO.Timeout


launchDriver :: (MonadUnliftIO m, MonadMask m, MonadLogger m) => DriverConfig -> m Driver
launchDriver driverConfig = recovering policy [const handler] $ \_ -> launchDriver' driverConfig
  where
    policy = limitRetries 10 <> capDelay 1_000_000 (exponentialBackoff 50_000)

    handler = Handler $ \case
      DriverOutputEndedBeforeReady _ output | Prelude.any isPortTaken output -> do
        logWarnN [i|Driver couldn't bind port; retrying on a fresh one|]
        return True
      _ -> return False

    isPortTaken line = "address already in use" `T.isInfixOf` T.toLower line

launchDriver' :: (MonadUnliftIO m, MonadMask m, MonadLogger m) => DriverConfig -> m Driver
launchDriver' driverConfig = do
  manager <- liftIO $ newManager defaultManagerSettings
  let requestHeaders = mempty

  port <- findFreePortOrException

  (programName, args, maybeEnv) <- getArguments port driverConfig
  logDebugN [i|#{programName} #{T.unwords $ fmap T.pack args}|]

  (hRead, hWrite) <- createPipe

  envToUse <- case maybeEnv of
    Nothing -> pure Nothing
    Just extraEnv -> do
      env <- getEnvironment
      return $ Just (extraEnv <> env)

  let cp = (proc programName args) {
        create_group = True
        , std_out = UseHandle hWrite
        , std_err = UseHandle hWrite
        , env = envToUse
        }

  (_, _, _, p) <- createProcess cp

  let hostname = "localhost"

  maybeLogFileHandle <- case driverConfigLogDir driverConfig of
    Nothing -> return Nothing
    Just logDir -> do
      (logFilePath, logFileHandle) <- liftIO $ openTempFile logDir ((driverBaseName driverConfig) <> ".log")
      logDebugN [i|Logging driver output to #{logFilePath}|]
      return $ Just logFileHandle

  let handler (e :: SomeException) = do
        terminateProcess p
        now <- liftIO getCurrentTime
        whenJust maybeLogFileHandle $ \logFileHandle -> do
          liftIO (T.hPutStrLn logFileHandle [i|(#{now}) haskell-webdriver: process ending with exception: #{e}|])
          liftIO (hClose logFileHandle)

  startupOutput <- newIORef []
  -- Keep recording output until the driver is up, so a failure can report it.
  recording <- newIORef True

  let outputEnded = do
        -- EOF usually means the driver exited, but not necessarily, so don't wait forever.
        maybeExitCode <- timeout 5_000_000 (waitForProcess p)
        output <- Prelude.reverse <$> readIORef startupOutput
        return $ DriverOutputEndedBeforeReady maybeExitCode output

  flip withException handler $ do
    -- Read from the (combined) output stream until we see the up and running message
    -- safaridriver prints nothing, so for it readiness comes from the socket and /status checks alone.
    whenJust (needles driverConfig) $ \ns -> do
      maybeReady <- timeout 30_000_000 $ fix $ \loop -> do
        line <- liftIO (try (T.pack <$> hGetLine hRead)) >>= \case
          Right line -> return line
          Left e | isEOFError e -> outputEnded >>= throwIO
                 | otherwise -> throwIO e
        modifyIORef' startupOutput (line :)
        whenJust maybeLogFileHandle $ \logFileHandle ->
          liftIO $ T.hPutStrLn logFileHandle line
        unless (Prelude.any (`T.isInfixOf` line) ns) loop
      when (isNothing maybeReady) $
        throwIO DriverNoReadyMessage

    logAsync <- async $ flip finally (liftIO $ whenJust maybeLogFileHandle hClose) $ forever $ do
      line <- liftIO (T.hGetLine hRead)
      readIORef recording >>= flip when (modifyIORef' startupOutput (line :))
      whenJust maybeLogFileHandle $ \logFileHandle ->
        liftIO $ T.hPutStrLn logFileHandle line

    flip onException (cancel logAsync) $ do
      addr <- liftIO (getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just "127.0.0.1") (Just (show port))) >>= \case
        addr:_ -> return addr
        _ -> throwIO DriverGetAddrInfoFailed
      let policy = limitRetriesByCumulativeDelay (120 * 1_000_000) $ capDelay 1_000_000 $ exponentialBackoff 1000

      let basePath = case driverConfig of
            DriverConfigSeleniumJar {} -> "/wd/hub"
            _ -> ""

      let driver = Driver {
            _driverHostname = hostname
            , _driverPort = fromIntegral port
            , _driverBasePath = basePath
            , _driverRequestHeaders = requestHeaders
            , _driverManager = manager
            , _driverProcess = Just p
            , _driverLogAsync = Just logAsync
            , _driverConfig = driverConfig
            }

      -- A driver that fails to start, for example because its port was taken after all, exits without
      -- ever listening. Watch for that rather than waiting the whole policy out. Without a ready
      -- message we can't tell our listener from another process's on the same port, though, so a clash
      -- that safaridriver notices only after /status has answered goes undetected.
      let watchProcess = fix $ \loop -> getProcessExitCode p >>= \case
            -- Poll: waitForProcess would block every thread in a program built without -threaded.
            Nothing -> threadDelay 100_000 >> loop
            Just _ -> do
              -- Let the log thread finish copying the output; it ends when the pipe closes.
              _ <- timeout 5_000_000 (waitCatch logAsync)
              outputEnded

      let waitForDriver = do
            -- Wait for a successful connection to the server socket
            waitForSocket policy addr

            logDebugN [i|Finished wait for driver socket|]

            -- Wait for a successful call to /status
            recoverAll policy $ \retryStatus -> do
              let req = mkDriverRequest driver methodGet "/status" Null
              resp <- liftIO $ httpLbs req manager
              let code = statusCode (responseStatus resp)
              if | code >= 200 && code < 300 -> return ()
                 | otherwise -> do
                     logWarnN [i|(#{retryStatus}) Invalid response from /status: #{resp}|]
                     throwIO DriverStatusEndpointNotReady

      race watchProcess waitForDriver >>= either throwIO return
      writeIORef recording False

      logInfoN [i|Finished wait for driver /status endpoint. Driver is running on #{hostname}:#{port}|]

      return driver

getArguments :: (MonadIO m, MonadLogger m) => PortNumber -> DriverConfig -> m (FilePath, [String], Maybe [(String, String)])
getArguments port (DriverConfigSeleniumJar {..}) = do
  javaArgs :: [String] <- mconcat <$> mapM getSubDriverArgs driverConfigSubDrivers

  let maybeSeleniumVersion = case driverConfigSeleniumVersion of
        Just x -> Just x
        Nothing -> autodetectSeleniumVersionByFileName driverConfigSeleniumJar
  logInfoN [i|Detected Selenium version: #{maybeSeleniumVersion}|]
  let extraArgs = case maybeSeleniumVersion of
        Just Selenium3 -> ["-port", show port]
        Just Selenium4 -> ["standalone", "--port", show port, "--host", "localhost"]
        Nothing -> ["-port", show port]

  let fullArgs = javaArgs
               <> ["-jar", driverConfigSeleniumJar]
               <> extraArgs
  return (driverConfigJava, fullArgs <> driverConfigJavaFlags, driverConfigJavaExtraEnv)
getArguments port (DriverConfigChromedriver {..}) = do
  return (driverConfigChromedriver, ["--port=" <> show port] <> driverConfigChromedriverFlags, driverConfigChromedriverExtraEnv)
getArguments port (DriverConfigGeckodriver {..}) = do
  return (driverConfigGeckodriver, ["--port", show port] <> driverConfigGeckodriverFlags, driverConfigGeckodriverExtraEnv)
getArguments port (DriverConfigSafaridriver {..}) = do
  return (driverConfigSafaridriver, ["--port", show port] <> driverConfigSafaridriverFlags, driverConfigSafaridriverExtraEnv)

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

          case any isNothing parts of
            True -> Nothing
            False -> case catMaybes parts of
              [x, _, _, _] -> Just x
              [x, _, _] -> Just x
              [x, _] -> Just x
              [x] -> Just x
              _ -> Nothing

getSubDriverArgs :: Monad m => DriverConfig -> m [FilePath]
getSubDriverArgs (DriverConfigChromedriver {..}) = do
  let logArgs = case driverConfigLogDir of
        Just d -> ["-Dwebdriver.chrome.logfile=" <> (d </> "chromedriver.log")
                  , "-Dwebdriver.chrome.verboseLogging=true"
                  ]
        Nothing -> []
  return (["-Dwebdriver.chrome.driver=" <> driverConfigChromedriver] <> logArgs)
getSubDriverArgs (DriverConfigGeckodriver {..}) = do
  return [
    "-Dwebdriver.gecko.driver=" <> driverConfigGeckodriver
    ]
-- Selenium 4 honours this property; Selenium 3 ignores it and uses /usr/bin/safaridriver.
getSubDriverArgs (DriverConfigSafaridriver {..}) = do
  return [
    "-Dwebdriver.safari.driver=" <> driverConfigSafaridriver
    ]
getSubDriverArgs _ = return []

-- | Output that shows the driver is listening, for drivers that print any.
needles :: DriverConfig -> Maybe [T.Text]
needles (DriverConfigSeleniumJar {}) = Just ["Selenium Server is up and running", "Started Selenium Standalone"]
needles (DriverConfigChromedriver {}) = Just ["ChromeDriver was started successfully"]
needles (DriverConfigGeckodriver {}) = Just ["Listening on"]
needles (DriverConfigSafaridriver {}) = Nothing

driverBaseName :: DriverConfig -> String
driverBaseName (DriverConfigSeleniumJar {}) = "selenium"
driverBaseName (DriverConfigChromedriver {}) = "chromedriver"
driverBaseName (DriverConfigGeckodriver {}) = "geckodriver"
driverBaseName (DriverConfigSafaridriver {}) = "safaridriver"

data DriverException =
  DriverGetAddrInfoFailed
  | DriverNoReadyMessage
  | DriverStatusEndpointNotReady
  | DriverOutputEndedBeforeReady (Maybe ExitCode) [T.Text]
  deriving (Show, Eq)

instance Exception DriverException

mkDriverRequest :: (ToJSON a) => Driver -> Method -> T.Text -> a -> Request
mkDriverRequest (Driver {..}) meth wdPath args =
  defaultRequest {
    host = BS.pack _driverHostname
    , port = _driverPort
    , path = BS.pack _driverBasePath `BS.append`  TE.encodeUtf8 wdPath
    , requestBody = RequestBodyLBS body
    , requestHeaders = _driverRequestHeaders ++ extraHeaders
    , method = meth
#if !MIN_VERSION_http_client(0,5,0)
    , checkStatus = \_ _ _ -> Nothing
#endif
    }
  where
    body = case toJSON args of
      Null -> "" -- Passing Null as the argument indicates no request body
      other -> encode other

    extraHeaders = [
      (hAccept, "application/json;charset=UTF-8")
      , (hContentType, "application/json;charset=UTF-8")
      ]


-- | Grace period to wait at each escalation step when stopping a driver process.
driverStopGracePeriodUs :: Int
driverStopGracePeriodUs = 15_000_000

gracefullyStopDriverProcess :: (MonadUnliftIO m, MonadLogger m) => ProcessHandle -> m ()
gracefullyStopDriverProcess p = do
  let waitForExit = do
        let policy = limitRetriesByCumulativeDelay driverStopGracePeriodUs $ capDelay 200_000 $ exponentialBackoff 1_000
        retrying policy (\_ x -> return (isNothing x)) $ \_ -> getProcessExitCode p

  -- Interrupting can throw if the group is already gone (a benign race); anything else is worth surfacing.
  let interruptGroup = tryAny (interruptProcessGroupOf p) >>= \case
        Right () -> return ()
        Left e -> logWarnN [i|Failed to interrupt driver process group: #{e}|]

  interruptGroup
  waitForExit >>= \case
    Just _ -> return ()
    Nothing -> do
      logWarnN [i|Driver process didn't stop after interrupting its group; interrupting again|]
      interruptGroup
      waitForExit >>= \case
        Just _ -> return ()
        Nothing -> do
          logWarnN [i|Driver process still didn't stop; terminating the leader|]
          terminateProcess p
          void waitForExit

teardownDriver :: (MonadUnliftIO m, MonadLogger m) => Driver -> m ()
teardownDriver (Driver {..}) = do
  case _driverProcess of
    Just p -> gracefullyStopDriverProcess p
    Nothing -> return ()
  case _driverLogAsync of
    Just x -> cancel x
    Nothing -> return ()

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg
