
-- | This module exports all the actions that can be used to interact with a
-- browser session.
module Test.WebDriver.Commands (
  -- * Sessions
  -- | A session is equivalent to a single instantiation of a
  -- particular user agent, including all its child browsers. WebDriver gives
  -- each session a unique session ID that can be used to differentiate one
  -- session from another, allowing multiple user agents to be controlled from a
  -- single HTTP server, and allowing sessions to be routed via a multiplexer
  -- (known as an intermediary node).
  --
  -- See https://www.w3.org/TR/webdriver1/#sessions.
  module Test.WebDriver.Commands.Sessions

  -- * Navigation
  -- | The commands in this section allow navigation of the current top-level
  -- browsing context to new URLs and introspection of the document currently
  -- loaded in this browsing context.
  --
  -- See https://www.w3.org/TR/webdriver1/#navigation.
  , module Test.WebDriver.Commands.Navigation

  -- * Command contexts
  -- | Many WebDriver commands happen in the context of either the current
  -- browsing context or current top-level browsing context. The current
  -- top-level browsing context is represented in the protocol by its associated
  -- window handle. When a top-level browsing context is selected using the
  -- Switch To Window command, a specific browsing context can be selected using
  -- the Switch to Frame command.
  --
  -- See https://www.w3.org/TR/webdriver1/#command-contexts.
  , module Test.WebDriver.Commands.CommandContexts

  -- * Element Retrieval
  -- | The Find Element, Find Elements, Find Element From Element, and Find
  -- Elements From Element commands allow lookup of individual elements and
  -- collections of elements. Element retrieval searches are performed using
  -- pre-order traversal of the document’s nodes that match the provided
  -- selector’s expression. Elements are serialized and returned as web
  -- elements.
  --
  -- See https://www.w3.org/TR/webdriver1/#element-retrieval.
  , module Test.WebDriver.Commands.ElementRetrieval

  -- * Element State
  --
  -- See https://www.w3.org/TR/webdriver1/#element-state.
  , module Test.WebDriver.Commands.ElementState

  -- * Element Interaction
  --
  -- See https://www.w3.org/TR/webdriver1/#element-interaction.
  , module Test.WebDriver.Commands.ElementInteraction

  -- * Document handling
  --
  -- See https://www.w3.org/TR/webdriver1/#document-handling.
  , module Test.WebDriver.Commands.DocumentHandling
  , ignoreReturn

  -- * Cookies
  --
  -- See https://www.w3.org/TR/webdriver1/#cookies.
  , module Test.WebDriver.Commands.Cookies

  -- * Actions
  --
  -- See https://www.w3.org/TR/webdriver1/#actions.
  , module Test.WebDriver.Commands.Actions

  -- * User Prompts
  --
  -- See https://www.w3.org/TR/webdriver1/#user-prompts.
  , module Test.WebDriver.Commands.UserPrompts

  -- * Screen capture
  --
  -- See https://www.w3.org/TR/webdriver1/#screen-capture.
  , module Test.WebDriver.Commands.ScreenCapture

  -- * Browser logs
  -- | Retrieve browser console logs and other log types.
  , module Test.WebDriver.Commands.Logs

  -- * Selenium-specific
  -- ** Mobile device support
  , module Test.WebDriver.Commands.SeleniumSpecific.Mobile
  -- ** Uploading files to remote server
  -- | These functions allow you to upload a file to a remote server.
  -- Note that this operation isn't supported by all WebDriver servers,
  -- and the location where the file is stored is not standardized.
  , module Test.WebDriver.Commands.SeleniumSpecific.Uploads
  -- ** HTML5
  , module Test.WebDriver.Commands.SeleniumSpecific.HTML5
  -- ** Misc
  , module Test.WebDriver.Commands.SeleniumSpecific.Misc
  ) where

import Test.WebDriver.Commands.Actions
import Test.WebDriver.Commands.CommandContexts
import Test.WebDriver.Commands.Cookies
import Test.WebDriver.Commands.DocumentHandling
import Test.WebDriver.Commands.ElementInteraction
import Test.WebDriver.Commands.ElementRetrieval
import Test.WebDriver.Commands.ElementState
import Test.WebDriver.Commands.Logs
import Test.WebDriver.Commands.Navigation
import Test.WebDriver.Commands.ScreenCapture
import Test.WebDriver.Commands.SeleniumSpecific.HTML5
import Test.WebDriver.Commands.SeleniumSpecific.Misc
import Test.WebDriver.Commands.SeleniumSpecific.Mobile
import Test.WebDriver.Commands.SeleniumSpecific.Uploads
import Test.WebDriver.Commands.Sessions
import Test.WebDriver.Commands.UserPrompts

import Test.WebDriver.JSON (ignoreReturn)
