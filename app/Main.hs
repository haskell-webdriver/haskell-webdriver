{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status as N
import System.IO.Temp
import Test.WebDriver
import Test.WebDriver.Capabilities
import Test.WebDriver.Types
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory
import UnliftIO.Exception


newtype WD a = WD (ReaderT Session (LoggingT IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadUnliftIO, MonadReader Session, MonadLogger)

doCommandBaseWithLogging driver method path args = do
  let req = mkDriverRequest driver method path args
  logDebugN [i|--> #{HC.method req} #{HC.path req}#{HC.queryString req} (#{showRequestBody (HC.requestBody req)})|]
  response <- tryAny (liftIO $ HC.httpLbs req (_driverManager driver)) >>= either throwIO return
  let (N.Status code _) = HC.responseStatus response
  logDebugN [i|<-- #{code} #{response}|]
  return response

  where
    showRequestBody :: HC.RequestBody -> B.ByteString
    showRequestBody (HC.RequestBodyLBS bytes) = BL.toStrict bytes
    showRequestBody (HC.RequestBodyBS bytes) = bytes
    showRequestBody _ = "<request body>"

instance WebDriverBase WD where
  doCommandBase = doCommandBaseWithLogging

instance WebDriverBase (LoggingT IO) where
  doCommandBase = doCommandBaseWithLogging

instance SessionState WD where
  getSession = ask

runWD :: Session -> WD a -> LoggingT IO a
runWD sess (WD wd) = runReaderT wd sess

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

  tmpDir <- getCanonicalTemporaryDirectory >>= flip createTempDirectory "chromedriver-logs"

  let driverConfig = DriverConfigChromedriver {
        driverConfigChromedriver = chromedriverBin
        , driverConfigChromedriverFlags = []
        , driverConfigChrome = chromeBin
        , driverConfigLogDir = tmpDir
        }

  runStdoutLoggingT $ bracket mkEmptyWebDriverContext teardownWebDriverContext $ \wdc -> do
    sess <- startSession wdc driverConfig caps "session1"
    runWD sess $ do
      openPage "http://www.xkcd.com"
      liftIO $ threadDelay 5_000_000
