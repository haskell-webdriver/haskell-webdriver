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

import Test.WebDriver.Classes
import Test.WebDriver.Utils (urlEncode)

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception.Lifted
import Data.Typeable
import Data.Default
import Control.Applicative

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
