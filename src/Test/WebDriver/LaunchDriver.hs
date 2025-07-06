{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Test.WebDriver.LaunchDriver (
  launchDriver

  , DriverConfig(..)
  , Driver(..)

  , Session(..)
  , SessionId(..)

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
import Data.String
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Network.HTTP.Client
import Network.HTTP.Types (RequestHeaders, Method, hAccept, hContentType, methodGet, statusCode)
import Network.Socket
import System.FilePath
import System.IO
import Test.WebDriver.Types
import Test.WebDriver.Util.Ports
import Test.WebDriver.Util.Sockets
import Text.Read (readMaybe)
import UnliftIO.Async
import UnliftIO.Exception
import UnliftIO.Process
import UnliftIO.Timeout


launchDriver :: (MonadUnliftIO m, MonadMask m, MonadLogger m) => DriverConfig -> m Driver
launchDriver driverConfig = do
  manager <- liftIO $ newManager defaultManagerSettings
  let requestHeaders = mempty
  let authHeaders = mempty

  -- webdriverDir <- liftIO $ createTempDirectory (driverConfigLogDir driverConfig) "webdriver"

  port <- findFreePortOrException

  (programName, args) <- getArguments port driverConfig
  logDebugN [i|#{programName} #{T.unwords $ fmap T.pack args}|]

  (hRead, hWrite) <- createPipe

  let cp = (proc programName args) {
        create_group = True
        , std_out = UseHandle hWrite
        , std_err = UseHandle hWrite
        }

  (_, _, _, p) <- createProcess cp

  let hostname = "localhost"

  -- Read from the (combined) output stream until we see the up and running message
  maybeReady <- timeout 30_000_00 $ fix $ \loop -> do
    line <- fmap T.pack $ liftIO $ hGetLine hRead
    -- debug line
    unless (Prelude.any (\x -> x `T.isInfixOf` line) (needles driverConfig)) loop
  when (isNothing maybeReady) $
    throwIO DriverNoReadyMessage

  logAsync <- async $ forever $ do
    line <- liftIO (T.hGetLine hRead)
    logDebugN line

  -- Wait for a successful connection to the server socket
  addr <- liftIO (getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just (show port))) >>= \case
    addr:_ -> return addr
    _ -> undefined -- exception
  let policy = limitRetriesByCumulativeDelay (120 * 1_000_000) $ capDelay 1_000_000 $ exponentialBackoff 1000

  waitForSocket policy addr

  let basePath = case driverConfig of
        DriverConfigSeleniumJar {} -> "/wd/hub"
        _ -> ""

  let driver = Driver {
        _driverHostname = hostname
        , _driverPort = fromIntegral port
        , _driverBasePath = basePath
        , _driverRequestHeaders = requestHeaders
        , _driverAuthHeaders = authHeaders
        , _driverManager = manager
        , _driverProcess = p
        , _driverConfig = driverConfig
        , _driverLogAsync = logAsync
        }

  -- Wait for a successful call to /status
  recoverAll policy $ \retryStatus -> do
    let req = mkDriverRequest driver methodGet "/status" Null
    resp <- liftIO $ httpLbs req manager
    let code = statusCode (responseStatus resp)
    if | code >= 200 && code < 300 -> return ()
       | otherwise -> do
           logWarnN [i|(#{retryStatus}) Invalid response from /status: #{resp}|]
           throwIO DriverStatusEndpointNotReady

  return driver

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

getArguments :: (MonadIO m, MonadLogger m) => PortNumber -> DriverConfig -> m (FilePath, [String])
getArguments port (DriverConfigSeleniumJar {..}) = do
  javaArgs :: [String] <- fmap mconcat <$> mapM getSubDriverArgs driverConfigSubDrivers

  let maybeSeleniumVersion = autodetectSeleniumVersionByFileName driverConfigSeleniumJar
  logInfoN [i|Detected Selenium version: #{maybeSeleniumVersion}|]
  let extraArgs = case maybeSeleniumVersion of
        Just Selenium3 -> ["-port", show port]
        Just Selenium4 -> ["standalone", "--port", show port, "--host", "localhost"]
        Nothing -> ["-port", show port]

  let fullArgs = javaArgs
               <> ["-jar", driverConfigSeleniumJar]
               <> extraArgs
  return (driverConfigJava, fullArgs)
getArguments port (DriverConfigChromedriver {..}) = do
  return (driverConfigChromedriver, ["--port=" <> show port])
getArguments port (DriverConfigGeckodriver {..}) = do
  return (driverConfigGeckodriver, ["--port", show port])

getSubDriverArgs :: Monad m => DriverConfig -> m [FilePath]
getSubDriverArgs (DriverConfigChromedriver {..}) = do
  let chromedriverLog = driverConfigLogDir </> "chromedriver.log"
  return [
    "-Dwebdriver.chrome.logfile=" <> chromedriverLog
    , "-Dwebdriver.chrome.verboseLogging=true"
    , "-Dwebdriver.chrome.driver=" <> driverConfigChromedriver
    ]
getSubDriverArgs (DriverConfigGeckodriver {..}) = do
  return [
    "-Dwebdriver.gecko.driver=" <> driverConfigGeckodriver
    ]
getSubDriverArgs _ = return []

needles :: DriverConfig -> [T.Text]
needles (DriverConfigSeleniumJar {}) = ["Selenium Server is up and running", "Started Selenium Standalone"]
needles (DriverConfigChromedriver {}) = ["ChromeDriver was started successfully"]
needles (DriverConfigGeckodriver {}) = ["Listening on"]

data SeleniumVersion =
  Selenium3
  | Selenium4
  deriving (Show, Eq)

data DriverException =
  DriverNoReadyMessage
  | DriverStatusEndpointNotReady
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
