{-# LANGUAGE TemplateHaskell #-}

module Test.WebDriver.Capabilities.Proxy where

import Data.Aeson.TH
import Test.WebDriver.Capabilities.Aeson


data ProxyType =
  ProxyTypePac
  | ProxyTypeDirect
  | ProxyTypeAutodetect
  | ProxyTypeSystem
  | ProxyTypeManual
  deriving (Show, Eq)
deriveJSON toCamelC2 ''ProxyType

data Proxy = Proxy {
  proxyType :: ProxyType
  }
  deriving (Show, Eq)
deriveJSON baseOptions ''Proxy
