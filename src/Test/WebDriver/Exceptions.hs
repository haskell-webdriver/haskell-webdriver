{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-deriving-typeable #-}

module Test.WebDriver.Exceptions (
  InvalidURL(..)
  , NoSessionId(..)
  , BadJSON(..)

  , HTTPStatusUnknown(..)

  , ServerError(..)

  , FailedCommand(..)
  , FailedCommandError(..)

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

-- | An unidentified server-side exception occured
newtype ServerError = ServerError String
  deriving (Eq, Show, Typeable)
instance Exception ServerError

data FailedCommandError =
  -- | The Element Click command could not be completed because the element receiving the events is obscuring the element that was requested clicked.
  ElementClickIntercepted
  -- | A command could not be completed because the element is not pointer- or keyboard interactable.
  | ElementNotInteractable
  -- | Navigation caused the user agent to hit a certificate warning, which is usually the result of an expired or invalid TLS certificate.
  | InsecureCertificate
  -- | The arguments passed to a command are either invalid or malformed.
  | InvalidArgument
  -- | An illegal attempt was made to set a cookie under a different domain than the current page.
  | InvalidCookieDomain
  -- | A command could not be completed because the element is in an invalid state, e.g. attempting to clear an element that isn't both editable and resettable.
  | InvalidElementState
  -- | Argument was an invalid selector.
  | InvalidSelector
  -- | Occurs if the given session id is not in the list of active sessions, meaning the session either does not exist or that it’s not active.
  | InvalidSessionId
  -- | An error occurred while executing JavaScript supplied by the user.
  | JavascriptError
  -- | The target for mouse interaction is not in the browser’s viewport and cannot be brought into that viewport.
  | MoveTargetOutOfBounds
  -- | An attempt was made to operate on a modal dialog when one was not open.
  | NoSuchAlert
  -- | No cookie matching the given path name was found amongst the associated cookies of the current browsing context’s active document.
  | NoSuchCookie
  -- | An element could not be located on the page using the given search parameters.
  | NoSuchElement
  -- | A command to switch to a frame could not be satisfied because the frame could not be found.
  | NoSuchFrame
  -- | A command to switch to a window could not be satisfied because the window could not be found.
  | NoSuchWindow
  -- | A script did not complete before its timeout expired.
  | ScriptTimeout
  -- | A new session could not be created.
  | SessionNotCreated
  -- | A command failed because the referenced element is no longer attached to the DOM.
  | StaleElementReference
  -- | An operation did not complete before its timeout expired.
  | Timeout
  -- | A command to set a cookie’s value could not be satisfied.
  | UnableToSetCookie
  -- | A screen capture was made impossible.
  | UnableToCaptureScreen
  -- | A modal dialog was open, blocking this operation.
  | UnexpectedAlertOpen
  -- | A command could not be executed because the remote end is not aware of it.
  | UnknownCommand
  -- | An unknown error occurred in the remote end while processing the command.
  | UnknownError
  -- | The requested command matched a known URL but did not match an method for that URL.
  | UnknownMethod
  -- | Indicates that a command that should have executed properly cannot be supported for some reason.
  | UnsupportedOperation
  -- | Some error string we weren't able to parse.
  | UnparsedError Text
  deriving (Show, Eq)
deriveFromJSON toSpacedC0 ''FailedCommandError
deriveToJSON toSpacedC0 ''FailedCommandError

-- | Internal type representing the JSON response object.
data FailedCommand = FailedCommand {
  rspError :: FailedCommandError
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
