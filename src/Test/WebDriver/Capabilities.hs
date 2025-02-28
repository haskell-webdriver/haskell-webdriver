{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.WebDriver.Capabilities (
  Capabilities(..)
  , defaultCaps

  , module Test.WebDriver.Capabilities.ChromeOptions
  , module Test.WebDriver.Capabilities.FirefoxOptions

  , Platform(..)
  ) where

import Data.Aeson.TH
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
  capabilitiesBrowserName :: Maybe String
  -- | Identifies the version of the user agent.
  , capabilitiesBrowserVersion :: Maybe String
  -- | Identifies the operating system of the endpoint node.
  , capabilitiesPlatformName :: Maybe Platform
  -- | Indicates whether untrusted and self-signed TLS certificates are implicitly trusted on navigation for the
  -- duration of the session.
  , capabilitiesAcceptInsecureCerts :: Maybe Bool
  -- | Defines the current session’s page load strategy.
  , capabilitiesPageLoadStrategy :: Maybe String
  -- | Defines the current session’s proxy configuration.
  , capabilitiesProxy :: Maybe Proxy
  -- | Indicates whether the remote end supports all of the commands in Resizing and Positioning Windows.
  , capabilitiesSetWindowRect :: Maybe Bool
  -- | Describes the timeouts imposed on certain session operations.
  , capabilitiesTimeouts :: Maybe Timeouts
  -- | Describes the current session’s user prompt handler.
  , capabilitiesUnhandledPromptBehavior :: Maybe UserPromptHandler

  -- * Vendor-specific stuff
  -- | Chrome options
  , capabilitiesGoogChromeOptions :: Maybe ChromeOptions
  -- | Firefox options
  , capabilitiesMozFirefoxOptions :: Maybe FirefoxOptions
  } deriving (Eq, Show)

deriveJSON capabilitiesOptions ''Capabilities

-- | Default capabilities. This is the same as the 'Default' instance, but with
-- less polymorphism. By default, we use 'firefox' of an unspecified 'version'
-- with default system-wide 'proxy' settings on whatever 'platform' is available
-- . All 'Maybe' capabilities are set to 'Nothing' (no preference).
defaultCaps :: Capabilities
defaultCaps = Capabilities {
  capabilitiesBrowserName = Nothing
  , capabilitiesBrowserVersion = Nothing

  , capabilitiesPlatformName = Nothing

  , capabilitiesAcceptInsecureCerts = Nothing

  , capabilitiesPageLoadStrategy = Nothing

  , capabilitiesProxy = Nothing

  , capabilitiesSetWindowRect = Nothing

  , capabilitiesTimeouts = Nothing

  , capabilitiesUnhandledPromptBehavior = Nothing

  , capabilitiesGoogChromeOptions = Nothing
  , capabilitiesMozFirefoxOptions = Nothing
  }
