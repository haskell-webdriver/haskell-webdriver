
module Test.WebDriver.Commands.Sessions (
  -- * Sessions
  -- See https://www.w3.org/TR/webdriver1/#sessions

  createSession
  , closeSession
  , serverStatus

  -- , sessions
  -- , getActualCaps
  ) where

import Data.Aeson as A
import Data.CallStack
import Data.Maybe
import Prelude -- hides some "unused import" warnings
import Test.WebDriver.Capabilities (Capabilities)
import Test.WebDriver.Class
import Test.WebDriver.Commands.Internal
import Test.WebDriver.JSON
import Test.WebDriver.Session


-- | Create a new session with the given 'Capabilities'. The returned session becomes the \"current session\" for this action.
--
-- Note: if you're using 'runSession' to run your WebDriver commands, you don't need to call this explicitly.
createSession :: (HasCallStack, WebDriver wd) => Capabilities -> wd WDSession
createSession caps = do
  -- This is what the old JSON wire protocol session creation looks like:
  -- ignoreReturn . withAuthHeaders . doCommand methodPost "/session" . single "desiredCapabilities" $ caps

  -- It seems Selenium 3 actually goes into W3C protocol mode if it sees capabilities that look like the following
  ignoreReturn . withAuthHeaders . doCommand methodPost "/session" . single "capabilities" $ single "alwaysMatch" caps
  getSession

-- | Close the current session and the browser associated with it.
closeSession :: (HasCallStack, WebDriver wd) => wd ()
closeSession = do
  s@WDSession {} <- getSession
  noReturn $ doSessCommand methodDelete "" Null
  putSession s { wdSessId = Nothing }

-- | Get information from the server as a JSON 'Object'. For more information
-- about this object see
-- <https://github.com/SeleniumHQ/selenium/wiki/JsonWireProtocol#status>
serverStatus :: (HasCallStack, WebDriver wd) => wd Value   -- todo: make this a record type
serverStatus = doCommand methodGet "/status" Null

-- -- | Retrieve a list of active sessions and their 'Capabilities'.
-- -- TODO: remove, seems not to exist in the W3C spec
-- sessions :: (HasCallStack, WebDriver wd) => wd [(SessionId, Capabilities)]
-- sessions = do
--   objs <- doCommand methodGet "/sessions" Null
--   mapM (parsePair "id" "capabilities" "sessions") objs

-- -- | Get the actual server-side 'Capabilities' of the current session.
-- -- TODO: remove, seems not to exist in the W3C spec
-- getActualCaps :: (HasCallStack, WebDriver wd) => wd Capabilities
-- getActualCaps = doSessCommand methodGet "" Null
