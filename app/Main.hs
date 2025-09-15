{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status as N
import Test.WebDriver
import Test.WebDriver.Capabilities
import Test.WebDriver.Types
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory
import UnliftIO.Exception

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as KM

aesonLookup :: T.Text -> KM.KeyMap v -> Maybe v
aesonLookup = KM.lookup . A.fromText
#else
import qualified Data.HashMap.Strict as HM

aesonLookup :: (Eq k, Hashable k) => k -> HM.HashMap k v -> Maybe v
aesonLookup = HM.lookup
#endif

newtype WD a = WD (ReaderT Session (LoggingT IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadUnliftIO, MonadReader Session, MonadLogger)

doCommandBaseWithLogging driver method path args = do
  let req = mkDriverRequest driver method path args
  logDebugN [i|--> #{HC.method req} #{HC.path req}#{HC.queryString req} (#{showRequestBody (HC.requestBody req)})|]
  response <- tryAny (liftIO $ HC.httpLbs req (_driverManager driver)) >>= either throwIO return
  let (N.Status code _) = HC.responseStatus response

  if | code >= 200 && code < 300 -> case A.eitherDecode (HC.responseBody response) of
         -- For successful responses, try to pull out the "value" and show it
         Right (A.Object (aesonLookup "value" -> Just value)) -> logDebugN [i|<-- #{code} #{A.encode value}|]
         _ -> logDebugN [i|<-- #{code} #{HC.responseBody response}|]
     -- For non-successful responses, log the entire response.
     -- Reading the WebDriver spec, it would probably be sufficient to just show the "value" as above,
     -- plus the HTTP status message.
     | otherwise -> logDebugN [i|<-- #{code} #{response}|]
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

-- The following is a more general SessionState for MonadReader monads, but it
-- requires UndecidableInstances
-- class HasSession a where
--   extractSession :: a -> Session
-- instance HasSession Session where
--   extractSession = id
-- instance (HasSession a, MonadReader a m) => SessionState m where
--   getSession = asks extractSession

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
