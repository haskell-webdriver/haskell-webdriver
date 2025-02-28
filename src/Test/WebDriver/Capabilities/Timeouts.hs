{-# LANGUAGE TemplateHaskell #-}

module Test.WebDriver.Capabilities.Timeouts where

import Data.Aeson.TH
import Test.WebDriver.Capabilities.Aeson


data Timeouts = Timeouts {
  timeoutsScriptMs :: Maybe Int
  , timeoutsPageLoadMs :: Maybe Int
  , timeoutsImplicitMs :: Maybe Int
  }
  deriving (Show, Eq)
deriveJSON toCamelC2 ''Timeouts
