{-|
This module serves as the top-level interface to the Haskell WebDriver bindings,
providing most of the functionality you're likely to want.
-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module Test.WebDriver (
  WebDriverContext(..)
  , mkEmptyWebDriverContext
  , teardownWebDriverContext

  , mkDriverRequest
  , _driverManager

  -- , startSession
  , startSession'
  , closeSession'

  -- * WebDriver monad
  , WebDriver(..)
  , WD(..)

  , DriverConfig(..)
  , Session

  -- * Running WebDriver commands
  -- , runSession
  , withSession
  , runWD

  -- ** HTTP request header utilities
  -- , withRequestHeaders
  -- , withAuthHeaders

  -- * WebDriver commands
  , module Test.WebDriver.Commands

  -- * Capabilities (advanced configuration)
  , Capabilities(..)
  , defaultCaps
  , Platform(..)
  , ProxyType(..)

  -- ** Browser-specific capabilities
  -- , Browser(..)
  , Test.WebDriver.Commands.LogLevel(..)

  -- *** Browser defaults
  -- , firefox, chrome, ie, opera, iPhone, iPad, android

  -- * Exception handling
  -- , finallyClose
  -- , closeOnException

  , module Test.WebDriver.Exceptions
  ) where

import Data.Aeson as A
import Test.WebDriver.Capabilities
import Test.WebDriver.Capabilities.Proxy
import Test.WebDriver.Commands
import Test.WebDriver.Exceptions
import Test.WebDriver.Internal
import Test.WebDriver.JSON
import Test.WebDriver.LaunchDriver
import Test.WebDriver.Types

import Data.Map as M
import Control.Monad.Catch (MonadMask)
import Control.Monad
import Data.String.Interpolate
import Test.WebDriver.Util.Aeson (aesonLookup)
import Control.Monad.Logger
import Network.HTTP.Client
import Network.HTTP.Types (RequestHeaders, statusCode)
import UnliftIO.Concurrent
import UnliftIO.Exception


-- startSession :: (MonadReader ctx m, HasWebDriverContext ctx) => DriverConfig -> String -> m Session
-- startSession = undefined

startSession' :: (WebDriverBase m, MonadMask m, MonadLogger m) => WebDriverContext -> DriverConfig -> Capabilities -> String -> m Session
startSession' wdc dc@(DriverConfigSeleniumJar {}) caps sessionName = do
  driver <- modifyMVar (_webDriverSelenium wdc) $ \maybeSelenium -> do
    driver <- maybe (launchDriver dc) return maybeSelenium
    return (Just driver, driver)

  launchSessionInDriver wdc driver caps sessionName
startSession' wdc dc@(DriverConfigChromedriver {}) caps sessionName = do
  driver <- modifyMVar (_webDriverChromedriver wdc) $ \maybeChromedriver -> do
    driver <- maybe (launchDriver dc) return maybeChromedriver
    return (Just driver, driver)

  launchSessionInDriver wdc driver caps sessionName
startSession' wdc dc@(DriverConfigGeckodriver {}) caps sessionName = do
  driver <- launchDriver dc
  onException (launchSessionInDriver wdc driver caps sessionName)
              (teardownDriver driver)

launchSessionInDriver :: (WebDriverBase m, MonadLogger m) => WebDriverContext -> Driver -> Capabilities -> String -> m Session
launchSessionInDriver wdc driver caps sessionName = do
  response <- doCommandBase driver methodPost "/session" $ single "capabilities" $ single "alwaysMatch" caps

  sess <-
    if | statusCode (responseStatus response) == 200 -> do
           case A.eitherDecode (responseBody response) of
             Right x@(A.Object (aesonLookup "value" -> Just (A.Object (aesonLookup "sessionId" -> Just (A.String sessId))))) -> do
               logInfoN [i|Got capabilities from driver: #{x}|]
               return $ Session { sessionDriver = driver, sessionId = SessionId sessId, sessionName = sessionName }
             _ -> throwIO $ SessionCreationResponseHadNoSessionId response
       | otherwise -> throwIO SessionNameAlreadyExists

  modifyMVar (_webDriverSessions wdc) $ \sessionMap ->
    case M.lookup sessionName sessionMap of
      Just _ -> throwIO SessionNameAlreadyExists
      Nothing -> return (M.insert sessionName sess sessionMap, sess)

closeSession' :: (WebDriverBase m, MonadLogger m) => WebDriverContext -> Session -> m ()
closeSession' wdc (Session { sessionId=(SessionId sessId), .. }) = do
  response <- doCommandBase sessionDriver methodDelete ("/session/" <> sessId) Null
  logInfoN [i|Close result: #{response}|]

  modifyMVar_ (_webDriverSessions wdc) (return . M.delete sessionName)

  case _driverConfig sessionDriver of
    DriverConfigGeckodriver {} -> teardownDriver sessionDriver
    _ -> return ()

teardownWebDriverContext :: (WebDriverBase m, MonadLogger m) => WebDriverContext -> m ()
teardownWebDriverContext wdc@(WebDriverContext {..}) = do
  sessions <- readMVar _webDriverSessions
  forM_ [sess | (_, sess) <- M.toList sessions] $ \sess ->
    closeSession' wdc sess

  modifyMVar_ _webDriverSelenium $ \case
    Nothing -> return Nothing
    Just driver -> teardownDriver driver >> return Nothing

  modifyMVar_ _webDriverChromedriver $ \case
    Nothing -> return Nothing
    Just driver -> teardownDriver driver >> return Nothing

-- | Set a temporary list of custom 'RequestHeaders' to use within the given action.
-- All previous custom headers are temporarily removed, and then restored at the end.
withRequestHeaders :: (SessionStatePut m) => RequestHeaders -> m a -> m a
withRequestHeaders _h _action = undefined -- do
  -- withModifiedSession (\s -> s { wdSessRequestHeaders = h }) action

-- | Makes all webdriver HTTP requests in the given action use the session\'s auth headers, typically
-- configured by setting the 'wdAuthHeaders' config. This is useful if you want to temporarily use
-- the same auth headers you used for session creation with other HTTP requests.
withAuthHeaders :: (Monad m, SessionStatePut m) => m a -> m a
withAuthHeaders _wd = undefined -- do
  -- authHeaders <- fmap wdSessAuthHeaders getSession
  -- withRequestHeaders authHeaders wd

-- -- | A finalizer ensuring that the session is always closed at the end of
-- -- the given 'WD' action, regardless of any exceptions.
-- finallyClose :: (HasCallStack, WebDriver wd, MonadMask wd) => wd a -> wd a
-- finallyClose wd = closeOnException wd <* closeSession

-- -- | Exception handler that closes the session when an
-- -- asynchronous exception is thrown, but otherwise leaves the session open
-- -- if the action was successful.
-- closeOnException :: (HasCallStack, MonadMask wd) => WebDriver wd => wd a -> wd a
-- closeOnException wd = wd `onException` closeSession
