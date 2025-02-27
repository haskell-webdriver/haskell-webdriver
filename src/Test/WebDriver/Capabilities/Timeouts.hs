{-# LANGUAGE TemplateHaskell #-}

module Test.WebDriver.Capabilities.Timeouts where

import Data.Aeson
import Data.Aeson.TH
import Data.String (fromString)
import Data.Text (Text, toLower, toUpper)
import Test.WebDriver.Capabilities.Aeson

data Timeouts = Timeouts {
  timeoutsScriptMs :: Maybe Int
  , timeoutsPageLoadMs :: Maybe Int
  , timeoutsImplicitMs :: Maybe Int
  }
  deriving (Show, Eq)
deriveJSON timeoutsOptions ''Timeouts
