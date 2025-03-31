{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.WebDriver.Capabilities (
  Capabilities(..)
  , capabilitiesBrowserName
  , capabilitiesBrowserVersion
  , capabilitiesPlatformName
  , capabilitiesAcceptInsecureCerts
  , capabilitiesPageLoadStrategy
  , capabilitiesProxy
  , capabilitiesSetWindowRect
  , capabilitiesTimeouts
  , capabilitiesUnhandledPromptBehavior
  , capabilitiesGoogChromeOptions
  , capabilitiesMozFirefoxOptions
  , defaultCaps

  , module Test.WebDriver.Capabilities.ChromeOptions
  , module Test.WebDriver.Capabilities.FirefoxOptions

  , Platform(..)
  ) where

import Data.Aeson.TH
import Lens.Micro.TH
import Test.WebDriver.Capabilities.Aeson
import Test.WebDriver.Capabilities.ChromeOptions
import Test.WebDriver.Capabilities.FirefoxOptions
import Test.WebDriver.Capabilities.Platform
import Test.WebDriver.Capabilities.Proxy
import Test.WebDriver.Capabilities.Timeouts
import Test.WebDriver.Capabilities.UserPromptHandler


-- | A structure describing the capabilities of a session. This record
-- serves dual roles.
--
-- It's used to specify the desired capabilities for a session before
-- it's created. In this usage, fields that are set to Nothing indicate
-- that we have no preference for that capability.
--
-- When received from the server , it's used to describe the actual
-- capabilities given to us by the WebDriver server. Here a value of
-- Nothing indicates that the server doesn't support the capability.
-- Thus, for Maybe Bool fields, both Nothing and Just False indicate
-- a lack of support for the desired capability.
data Capabilities = Capabilities {
  -- | Identifies the user agent.
  _capabilitiesBrowserName :: Maybe String
  -- | Identifies the version of the user agent.
  , _capabilitiesBrowserVersion :: Maybe String
  -- | Identifies the operating system of the endpoint node.
  , _capabilitiesPlatformName :: Maybe Platform
  -- | Indicates whether untrusted and self-signed TLS certificates are implicitly trusted on navigation for the
  -- duration of the session.
  , _capabilitiesAcceptInsecureCerts :: Maybe Bool
  -- | Defines the current session’s page load strategy.
  , _capabilitiesPageLoadStrategy :: Maybe String
  -- | Defines the current session’s proxy configuration.
  , _capabilitiesProxy :: Maybe Proxy
  -- | Indicates whether the remote end supports all of the commands in Resizing and Positioning Windows.
  , _capabilitiesSetWindowRect :: Maybe Bool
  -- | Describes the timeouts imposed on certain session operations.
  , _capabilitiesTimeouts :: Maybe Timeouts
  -- | Describes the current session’s user prompt handler.
  , _capabilitiesUnhandledPromptBehavior :: Maybe UserPromptHandler

  -- * Vendor-specific stuff
  -- | Chrome options
  , _capabilitiesGoogChromeOptions :: Maybe ChromeOptions
  -- | Firefox options
  , _capabilitiesMozFirefoxOptions :: Maybe FirefoxOptions
  } deriving (Eq, Show)
deriveJSON capabilitiesOptions ''Capabilities
makeLenses ''Capabilities

-- | Default capabilities. This is the same as the 'Default' instance, but with
-- less polymorphism. By default, we use 'firefox' of an unspecified 'version'
-- with default system-wide 'proxy' settings on whatever 'platform' is available
-- . All 'Maybe' capabilities are set to 'Nothing' (no preference).
defaultCaps :: Capabilities
defaultCaps = Capabilities {
  _capabilitiesBrowserName = Nothing
  , _capabilitiesBrowserVersion = Nothing

  , _capabilitiesPlatformName = Nothing

  , _capabilitiesAcceptInsecureCerts = Nothing

  , _capabilitiesPageLoadStrategy = Nothing

  , _capabilitiesProxy = Nothing

  , _capabilitiesSetWindowRect = Nothing

  , _capabilitiesTimeouts = Nothing

  , _capabilitiesUnhandledPromptBehavior = Nothing

  , _capabilitiesGoogChromeOptions = Nothing
  , _capabilitiesMozFirefoxOptions = Nothing
  }
