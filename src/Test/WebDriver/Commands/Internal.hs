{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_HADDOCK not-home #-}
-- | Internal functions used to implement the functions exported by
-- "Test.WebDriver.Commands". These may be useful for implementing non-standard
-- webdriver commands.
module Test.WebDriver.Commands.Internal (
  -- * Low-level webdriver functions
  doCommand
  -- ** Commands with :sessionId URL parameter
  , doSessCommand
  , SessionId(..)
  -- ** Commands with element :id URL parameters
  , doElemCommand
  , Element(..)
  -- ** Commands with :windowHandle URL parameters
  , WindowHandle(..)

  -- * Exceptions
  , NoSessionId(..)
  ) where

import Control.Applicative
import Control.Exception.Safe
import Data.Aeson
import Data.Aeson.Types
import Data.CallStack
import Data.Text (Text)
import qualified Data.Text as T
import Prelude -- hides some "unused import" warnings
import Test.WebDriver.Class
import Test.WebDriver.JSON
import Test.WebDriver.Session
import Test.WebDriver.Utils (urlEncode)

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as KM

aesonToList :: KM.KeyMap v -> [(A.Key, v)]
aesonToList = KM.toList
#else
import qualified Data.HashMap.Strict        as HM

aesonToList :: HM.KeyMap v -> [(A.Key, v)]
aesonToList = HM.toList
#endif

-- | An opaque identifier for a web page element
newtype Element = Element Text
                  deriving (Eq, Ord, Show, Read)

instance FromJSON Element where
  parseJSON (Object o) = case fmap snd (aesonToList o) of
    (String id' : _) -> pure $ Element id'
    _ -> fail "No elements returned"
  parseJSON v = typeMismatch "Element" v

instance ToJSON Element where
  toJSON (Element e) = object ["ELEMENT" .= e]


-- | An opaque identifier for a browser window
newtype WindowHandle = WindowHandle Text
  deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

instance Exception NoSessionId
-- | A command requiring a session ID was attempted when no session ID was
-- available.
data NoSessionId = NoSessionId String CallStack
  deriving (Eq, Show, Typeable)

-- | This a convenient wrapper around 'doCommand' that automatically prepends
-- the session URL parameter to the wire command URL. For example, passing
-- a URL of \"/refresh\" will expand to \"/session/:sessionId/refresh\", where
-- :sessionId is a URL parameter as described in
-- <https://github.com/SeleniumHQ/selenium/wiki/JsonWireProtocol>
doSessCommand :: (
  HasCallStack, WebDriver wd, ToJSON a, FromJSON b, ToJSON b
  ) => Method -> Text -> a -> wd b
doSessCommand method path args = do
  WDSession { wdSessId = mSessId } <- getSession
  case mSessId of
    Nothing -> throwIO $ NoSessionId msg callStack
      where
        msg = "doSessCommand: No session ID found for relative URL " ++ show path
    Just (SessionId sId) ->
      -- Catch BadJSON exceptions here, since most commands go through this function.
      -- Then, re-throw them with "error", which automatically appends a callstack
      -- to the message in modern GHCs.
      -- This callstack makes it easy to see which command caused the BadJSON exception,
      -- without exposing too many internals.
      catch
        (doCommand method (T.concat ["/session/", urlEncode sId, path]) args)
        (\(e :: BadJSON) -> error $ show e)

-- | A wrapper around 'doSessCommand' to create element URLs.
-- For example, passing a URL of "/active" will expand to
-- \"/session/:sessionId/element/:id/active\", where :sessionId and :id are URL
-- parameters as described in the wire protocol.
doElemCommand :: (
  HasCallStack, WebDriver wd, ToJSON a, FromJSON b, ToJSON b
  ) => Method -> Element -> Text -> a -> wd b
doElemCommand m (Element e) path a =
  doSessCommand m (T.concat ["/element/", urlEncode e, path]) a
