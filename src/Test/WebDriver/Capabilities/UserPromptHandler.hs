{-# LANGUAGE TemplateHaskell #-}

module Test.WebDriver.Capabilities.UserPromptHandler where

import Data.Aeson
import Data.Aeson.TH
import Data.String (fromString)
import Data.Text (Text, toLower, toUpper)
import Test.WebDriver.Capabilities.Aeson

data UserPromptHandler =
  UserPromptHandlerDismiss
  | UserPromptHandlerAccept
  | UserPromptHandlerDismissAndNotify
  | UserPromptHandlerAcceptAndNotify
  | UserPromptHandlerIgnore
  deriving (Show, Eq)
deriveJSON toSpacedC3 ''UserPromptHandler
