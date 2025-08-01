{-# LANGUAGE TemplateHaskell #-}

module Test.WebDriver.Commands.Sessions (
  -- * Server information
  serverStatus

  -- * Timeouts
  , getTimeouts
  , setTimeouts

  -- ** Set individual timeouts
  , setScriptTimeout
  , setPageLoadTimeout
  , setImplicitWait

  -- * Types
  , Timeouts(..)
  , emptyTimeouts

  -- , sessions
  -- , getActualCaps
  ) where

import Data.Aeson as A
import Data.Aeson.TH as A
import GHC.Stack
import Test.WebDriver.Capabilities.Aeson
import Test.WebDriver.Types
import Test.WebDriver.Util.Commands


-- | Get information from the server as a JSON 'Object'. For more information
-- about this object see
-- <https://github.com/SeleniumHQ/selenium/wiki/JsonWireProtocol#status>
serverStatus :: (HasCallStack, WebDriver wd) => wd Value   -- todo: make this a record type
serverStatus = doCommand methodGet "/status" Null

-- -- | Get the actual server-side 'Capabilities' of the current session.
-- -- TODO: remove, seems not to exist in the W3C spec
-- getActualCaps :: (HasCallStack, WebDriver wd) => wd Capabilities
-- getActualCaps = doSessCommand methodGet "" Null

data Timeouts = Timeouts {
  -- | Determines when to interrupt a script that is being evaluated.
  timeoutsScript :: Maybe Integer
  -- | Provides the timeout limit used to interrupt navigation of the browsing context.
  , timeoutsPageLoad :: Maybe Integer
  -- | Gives the timeout of when to abort locating an element.
  , timeoutsImplicit :: Maybe Integer
  } deriving (Show, Eq)
deriveJSON toCamel1 ''Timeouts
emptyTimeouts :: Timeouts
emptyTimeouts = Timeouts Nothing Nothing Nothing

-- | Get all the 'Timeouts' simultaneously.
getTimeouts :: (HasCallStack, WebDriver wd) => wd Timeouts
getTimeouts = doSessCommand methodGet "/timeouts" Null

-- | Set all the 'Timeouts' simultaneously.
setTimeouts :: (HasCallStack, WebDriver wd) => Timeouts -> wd ()
setTimeouts timeouts = doSessCommand methodPost "/timeouts" (A.toJSON timeouts)

-- | Set the "script" value of the 'Timeouts'.
-- Selenium 3 and 4 accept @null@ for this value, which unsets it. The spec doesn't mention this.
setScriptTimeout :: (HasCallStack, WebDriver wd) => Maybe Integer -> wd ()
setScriptTimeout x = doSessCommand methodPost "/timeouts" (A.object [("script", maybe A.Null (A.Number . fromIntegral) x)])

-- | Set the "pageLoad" value of the 'Timeouts'.
setPageLoadTimeout :: (HasCallStack, WebDriver wd) => Integer -> wd ()
setPageLoadTimeout x = doSessCommand methodPost "/timeouts" (A.object [("pageLoad", A.Number $ fromIntegral x)])

-- | Set the "implicit" value of the 'Timeouts'.
setImplicitWait :: (HasCallStack, WebDriver wd) => Integer -> wd ()
setImplicitWait x = doSessCommand methodPost "/timeouts" (A.object [("implicit", A.Number $ fromIntegral x)])
