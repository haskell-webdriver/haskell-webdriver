{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

{-|

Once upon a time, the main browser automation tool was Selenium. Users of this
library had to start a Selenium session themselves, making sure to configure it
with a browser-specific driver program like @chromedriver@ or @geckodriver@, and
pass a hostname\/port to this library. Then, this library would connect to
Selenium and use its wire protocol to control browsers.

Nowadays, there is an official W3C spec (<https://www.w3.org/TR/webdriver1>)
specifying the protocol, and a number of implementations. Selenium still exists,
but Chromedriver and Geckodriver can both serve as standalone WebDriver servers.
This library now helps you start up a driver in one of these supported
configurations:

1. Selenium.jar with one or more supported sub-drivers (@chromedriver@,
@geckodriver@). This is similar to the traditional picture.
2. Chromedriver standalone.
3. Geckodriver standalone.

You can pick the configuration you want by passing a 'DriverConfig' to the
'startSession' function. The WebDriver implementations have a few differences
between them, which this library tries to smooth over. For example, a single
Geckodriver instance can't start multiple Firefox sessions (see
<https://github.com/mozilla/geckodriver/issues/1946>). So, this library will spin
up a separate @geckodriver@ process for every session.

-}

module Test.WebDriver (
  -- * WebDriverContext
  WebDriverContext
  , mkEmptyWebDriverContext
  , teardownWebDriverContext

  -- * Managed sessions
  , startSession
  , closeSession
  , DriverConfig(..)

  -- * Lower-level session management
  , startSession'
  , closeSession'
  , mkManualDriver

  -- * Capabilities
  , defaultCaps
  , defaultChromeOptions
  , defaultFirefoxOptions
  , Capabilities(..)
  , Platform(..)
  , ProxyType(..)

  -- * Commands
  , module Test.WebDriver.Commands

  , mkDriverRequest
  , _driverManager

  -- * WebDriver monad
  , WebDriver
  , WebDriverBase

  , Session

  -- * Exceptions
  , module Test.WebDriver.Exceptions
  ) where

import Data.Aeson as A
import Test.WebDriver.Capabilities
import Test.WebDriver.Capabilities.Proxy
import Test.WebDriver.Commands
import Test.WebDriver.Exceptions
import Test.WebDriver.JSON
import Test.WebDriver.LaunchDriver
import Test.WebDriver.Types

import Data.Map as M
import Control.Monad.Catch (MonadMask)
import Control.Monad
import Control.Monad.IO.Class
import Data.String.Interpolate
import Test.WebDriver.Util.Aeson (aesonLookup)
import Control.Monad.Logger
import Network.HTTP.Client
import Network.HTTP.Types (RequestHeaders, statusCode)
import UnliftIO.Concurrent
import UnliftIO.Exception


-- | Start a WebDriver session, with a given 'WebDriverContext' and
-- 'DriverConfig'.
--
-- You need to provide the WebDriver 'Capabilities' for the session. You should
-- make sure the browser-specific fields of your 'Capabilities' are filled in
-- correctly to match the given 'DriverConfig'. For example, if you're using
-- 'DriverConfigChromedriver', you should make sure to fill in
-- '_capabilitiesGoogChromeOptions' and in particular the '_chromeOptionsBinary'
-- field.

startSession :: (WebDriverBase m, MonadMask m, MonadLogger m) => WebDriverContext -> DriverConfig -> Capabilities -> String -> m Session
startSession wdc dc@(DriverConfigSeleniumJar {}) caps sessionName = do
  driver <- modifyMVar (_webDriverSelenium wdc) $ \maybeSelenium -> do
    driver <- maybe (launchDriver dc) return maybeSelenium
    return (Just driver, driver)

  launchSessionInDriver wdc driver caps sessionName
startSession wdc dc@(DriverConfigChromedriver {}) caps sessionName = do
  driver <- modifyMVar (_webDriverChromedriver wdc) $ \maybeChromedriver -> do
    driver <- maybe (launchDriver dc) return maybeChromedriver
    return (Just driver, driver)

  launchSessionInDriver wdc driver caps sessionName
startSession wdc dc@(DriverConfigGeckodriver {}) caps sessionName = do
  driver <- launchDriver dc
  onException (launchSessionInDriver wdc driver caps sessionName)
              (teardownDriver driver)

launchSessionInDriver :: (WebDriverBase m, MonadLogger m) => WebDriverContext -> Driver -> Capabilities -> String -> m Session
launchSessionInDriver wdc driver caps sessionName = do
  sess <- startSession' driver caps sessionName

  modifyMVar (_webDriverSessions wdc) $ \sessionMap ->
    case M.lookup sessionName sessionMap of
      Just _ -> throwIO SessionNameAlreadyExists
      Nothing -> return (M.insert sessionName sess sessionMap, sess)

-- | Lower-level version of 'startSession'. This one allows you to construct a
-- driver instance manually and pass it in. Does not manage process lifecycles.
startSession' :: (WebDriverBase m, MonadLogger m) => Driver -> Capabilities -> String -> m Session
startSession' driver caps sessionName = do
  response <- doCommandBase driver methodPost "/session" $ single "capabilities" $ single "alwaysMatch" caps

  if | statusCode (responseStatus response) == 200 -> do
         case A.eitherDecode (responseBody response) of
           Right x@(A.Object (aesonLookup "value" -> Just (A.Object (aesonLookup "sessionId" -> Just (A.String sessId))))) -> do
             logInfoN [i|Got capabilities from driver: #{x}|]
             return $ Session { sessionDriver = driver, sessionId = SessionId sessId, sessionName = sessionName }
           _ -> throwIO $ SessionCreationResponseHadNoSessionId response
     | otherwise -> throwIO SessionNameAlreadyExists

-- | Close the given WebDriver session. This sends the @DELETE
-- \/session\/:sessionId@ command to the WebDriver API, and then shuts down the
-- process if necessary.
closeSession :: (WebDriverBase m, MonadLogger m) => WebDriverContext -> Session -> m ()
closeSession wdc sess@(Session {..}) = do
  closeSession' sess

  modifyMVar_ (_webDriverSessions wdc) (return . M.delete sessionName)

  case _driverConfig sessionDriver of
    DriverConfigGeckodriver {} -> teardownDriver sessionDriver
    _ -> return ()

-- | Close the given WebDriver session. This is a lower-level version of
-- 'closeSession', which manages the driver lifecycle for you. This version will
-- only issue the @DELETE \/session\/:sessionId@ command to the driver, but will
-- not shut driver processes.
closeSession' :: (WebDriverBase m, MonadLogger m) => Session -> m ()
closeSession' (Session { sessionId=(SessionId sessId), .. }) = do
  response <- doCommandBase sessionDriver methodDelete ("/session/" <> sessId) Null
  logInfoN [i|Close result: #{response}|]

-- | Create a manual 'Driver' to use with 'startSession''/'closeSession''.
mkManualDriver :: MonadIO m =>
  -- | Host name
  String
  -- | Port
  -> Int
  -- | Base HTTP path (use "\/wd\/hub" for Selenium)
  -> String
  -- | Headers to send with every request
  -> RequestHeaders
  -> m Driver
mkManualDriver hostname port basePath requestHeaders = do
  manager <- liftIO $ newManager defaultManagerSettings

  return $ Driver {
    _driverHostname = hostname
    , _driverPort = port
    , _driverBasePath = basePath
    , _driverRequestHeaders = requestHeaders
    , _driverManager = manager
    , _driverProcess = Nothing
    , _driverLogAsync = Nothing
    , _driverConfig = DriverConfigChromedriver "" [] "" "" -- Not used
    }

-- | Tear down all sessions and processes associated with a 'WebDriverContext'.
teardownWebDriverContext :: (WebDriverBase m, MonadLogger m) => WebDriverContext -> m ()
teardownWebDriverContext wdc@(WebDriverContext {..}) = do
  sessions <- readMVar _webDriverSessions
  forM_ [sess | (_, sess) <- M.toList sessions] $ \sess ->
    closeSession wdc sess

  modifyMVar_ _webDriverSelenium $ \case
    Nothing -> return Nothing
    Just driver -> teardownDriver driver >> return Nothing

  modifyMVar_ _webDriverChromedriver $ \case
    Nothing -> return Nothing
    Just driver -> teardownDriver driver >> return Nothing

-- -- | Set a temporary list of custom 'RequestHeaders' to use within the given action.
-- -- All previous custom headers are temporarily removed, and then restored at the end.
-- withRequestHeaders :: (SessionStatePut m) => RequestHeaders -> m a -> m a
-- withRequestHeaders _h _action = undefined -- do
--   -- withModifiedSession (\s -> s { wdSessRequestHeaders = h }) action

-- -- | Makes all webdriver HTTP requests in the given action use the session\'s auth headers, typically
-- -- configured by setting the 'wdAuthHeaders' config. This is useful if you want to temporarily use
-- -- the same auth headers you used for session creation with other HTTP requests.
-- withAuthHeaders :: (Monad m, SessionStatePut m) => m a -> m a
-- withAuthHeaders _wd = undefined -- do
--   -- authHeaders <- fmap wdSessAuthHeaders getSession
--   -- withRequestHeaders authHeaders wd

-- -- | A finalizer ensuring that the session is always closed at the end of
-- -- the given 'WD' action, regardless of any exceptions.
-- finallyClose :: (HasCallStack, WebDriver wd, MonadMask wd) => wd a -> wd a
-- finallyClose wd = closeOnException wd <* closeSession

-- -- | Exception handler that closes the session when an
-- -- asynchronous exception is thrown, but otherwise leaves the session open
-- -- if the action was successful.
-- closeOnException :: (HasCallStack, MonadMask wd) => WebDriver wd => wd a -> wd a
-- closeOnException wd = wd `onException` closeSession
