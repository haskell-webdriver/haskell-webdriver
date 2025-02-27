{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.WebDriver.Capabilities (
  Capabilities(..)
  , defaultCaps

  , ChromeOptions(..)
  , defaultChromeOptions

  , FirefoxOptions(..)
  , defaultFirefoxOptions

  , Platform(..)
  ) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (Parser, typeMismatch, Pair)
import Data.Default (Default, def)
import Data.Maybe (fromMaybe, catMaybes)
import Data.String (fromString)
import Data.Text (Text, toLower, toUpper)
import Data.Word (Word16)
import Test.WebDriver.Capabilities.Aeson
import Test.WebDriver.Capabilities.ChromeOptions
import Test.WebDriver.Capabilities.FirefoxOptions
import Test.WebDriver.Capabilities.Platform
import Test.WebDriver.Capabilities.Proxy
import Test.WebDriver.Capabilities.Timeouts
import Test.WebDriver.Capabilities.UserPromptHandler
import Test.WebDriver.Chrome.Extension
import Test.WebDriver.Firefox.Profile
import Test.WebDriver.JSON

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap          as HM (delete, toList, empty)
#else
import qualified Data.HashMap.Strict        as HM (delete, toList, empty)
#endif


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
  capabilitiesBrowserName :: Maybe String
  , capabilitiesBrowserVersion :: Maybe String

  , capabilitiesPlatformName :: Maybe Platform

  , capabilitiesAcceptInsecureCerts :: Maybe Bool

  , capabilitiesPageLoadStrategy :: Maybe String

  , capabilitiesProxy :: Maybe Proxy

  , capabilitiesSetWindowRect :: Maybe Bool

  , capabilitiesTimeouts :: Maybe Timeouts

  , capabilitiesUnhandledPromptBehavior :: Maybe UserPromptHandler

  , capabilitiesGoogChromeOptions :: Maybe ChromeOptions
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
