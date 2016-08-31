{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, GeneralizedNewtypeDeriving  #-}
{-# OPTIONS_HADDOCK not-home #-}
-- |Internal functions used to implement the functions exported by
-- "Test.WebDriver.Commands". These may be useful for implementing non-standard
-- webdriver commands.
module Test.WebDriver.Commands.Internal
       (-- * Low-level webdriver functions
         doCommand
        -- ** Commands with :sessionId URL parameter
       , doSessCommand, SessionId(..)
        -- ** Commands with element :id URL parameters
       , doElemCommand, Element(..)
        -- ** Commands with :windowHandle URL parameters
       , doWinCommand, WindowHandle(..), currentWindow
        -- * Exceptions
       , NoSessionId(..)
       ) where

import Test.WebDriver.Class
import Test.WebDriver.Session
import Test.WebDriver.Utils (urlEncode)

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception.Lifted
import Data.Typeable
import Data.Default.Class
import Control.Applicative

import Prelude -- hides some "unused import" warnings

{- |An opaque identifier for a web page element. -}
newtype Element = Element Text
                  deriving (Eq, Ord, Show, Read)

instance FromJSON Element where
  parseJSON (Object o) = Element <$> o .: "ELEMENT"
  parseJSON v = typeMismatch "Element" v

instance ToJSON Element where
  toJSON (Element e) = object ["ELEMENT" .= e]


{- |An opaque identifier for a browser window -}
newtype WindowHandle = WindowHandle Text
                     deriving (Eq, Ord, Show, Read,
                               FromJSON, ToJSON)
instance Default WindowHandle where
  def = currentWindow

-- |A special 'WindowHandle' that always refers to the currently focused window.
-- This is also used by the 'Default' instance.
currentWindow :: WindowHandle
currentWindow = WindowHandle "current"

instance Exception NoSessionId
-- |A command requiring a session ID was attempted when no session ID was
-- available.
newtype NoSessionId = NoSessionId String
                 deriving (Eq, Show, Typeable)

-- |This a convenient wrapper around 'doCommand' that automatically prepends
-- the session URL parameter to the wire command URL. For example, passing
-- a URL of \"/refresh\" will expand to \"/session/:sessionId/refresh\", where
-- :sessionId is a URL parameter as described in
-- <https://github.com/SeleniumHQ/selenium/wiki/JsonWireProtocol>
doSessCommand :: (WebDriver wd, ToJSON a, FromJSON b) =>
                  Method -> Text -> a -> wd b
doSessCommand method path args = do
  WDSession { wdSessId = mSessId } <- getSession
  case mSessId of
      Nothing -> throwIO . NoSessionId $ msg
        where
          msg = "doSessCommand: No session ID found for relative URL "
                ++ show path
      Just (SessionId sId) -> doCommand method
                              (T.concat ["/session/", urlEncode sId, path]) args

-- |A wrapper around 'doSessCommand' to create element URLs.
-- For example, passing a URL of "/active" will expand to
-- \"/session/:sessionId/element/:id/active\", where :sessionId and :id are URL
-- parameters as described in the wire protocol.
doElemCommand :: (WebDriver wd, ToJSON a, FromJSON b) =>
                  Method -> Element -> Text -> a -> wd b
doElemCommand m (Element e) path a =
  doSessCommand m (T.concat ["/element/", urlEncode e, path]) a

-- |A wrapper around 'doSessCommand' to create window handle URLS.
-- For example, passing a URL of \"/size\" will expand to
-- \"/session/:sessionId/window/:windowHandle/\", where :sessionId and
-- :windowHandle are URL parameters as described in the wire protocol
doWinCommand :: (WebDriver wd, ToJSON a, FromJSON b) =>
                 Method -> WindowHandle -> Text -> a -> wd b
doWinCommand m (WindowHandle w) path a =
  doSessCommand m (T.concat ["/window/", urlEncode w, path]) a
