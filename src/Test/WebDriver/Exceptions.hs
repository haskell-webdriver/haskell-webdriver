{-# LANGUAGE TemplateHaskell #-}

module Test.WebDriver.Exceptions (
  InvalidURL(..)
  , NoSessionId(..)
  , BadJSON(..)

  , HTTPStatusUnknown(..)

  , UnknownCommand(..)
  , ServerError(..)

  , FailedCommand(..)
  , StackFrame(..)
  ) where

import Control.Exception (Exception)
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Stack
import Prelude -- hides some "unused import" warnings
import Test.WebDriver.Capabilities.Aeson
import Test.WebDriver.JSON
import Test.WebDriver.Types


-- | An invalid URL was given
newtype InvalidURL = InvalidURL String
  deriving (Eq, Show, Typeable)
instance Exception InvalidURL

-- | An unexpected HTTP status was sent by the server.
data HTTPStatusUnknown = HTTPStatusUnknown Int String
  deriving (Eq, Show, Typeable)
instance Exception HTTPStatusUnknown

-- | A command was sent to the WebDriver server that it didn't recognize.
newtype UnknownCommand = UnknownCommand String
  deriving (Eq, Show, Typeable)
instance Exception UnknownCommand

-- | An unidentified server-side exception occured
newtype ServerError = ServerError String
  deriving (Eq, Show, Typeable)
instance Exception ServerError

-- | Internal type representing the JSON response object.
data FailedCommand = FailedCommand {
  rspError :: Text
  , rspMessage :: Text
  , rspStacktrace :: Text
  , rspData :: Maybe Value
  } deriving (Eq, Show)
instance Exception FailedCommand
deriveFromJSON toCamel1 ''FailedCommand

-- | A command requiring a session ID was attempted when no session ID was
-- available.
data NoSessionId = NoSessionId String CallStack
  deriving (Show, Typeable)
instance Exception NoSessionId
