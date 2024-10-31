{-# OPTIONS_GHC -F -pgmF sandwich-discover #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec where

import Test.Sandwich hiding (BrowserToUse(..))
import TestLib.Types

#insert_test_imports


spec :: SpecWithWebDriver
spec = describe "Selenium tests" $ do
  $(getSpecFromFolder defaultGetSpecFromFolderOptions)
