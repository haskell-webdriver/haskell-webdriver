{-# LANGUAGE TemplateHaskell #-}

module Test.WebDriver.Capabilities.Proxy where

import Data.Aeson
import Data.Aeson.TH
import Data.String (fromString)
import Data.Text (Text, toLower, toUpper)
import Test.WebDriver.Capabilities.Aeson

data ProxyType =
  ProxyTypePac
  | ProxyTypeDirect
  | ProxyTypeAutodetect
  | ProxyTypeSystem
  | ProxyTypeManual
  deriving (Show, Eq)
deriveJSON toSnakeC2 ''ProxyType

data Proxy = Proxy {
  proxyType :: ProxyType
  }
  deriving (Show, Eq)
deriveJSON baseOptions ''Proxy
