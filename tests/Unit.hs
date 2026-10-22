{-# OPTIONS_GHC -F -pgmF sandwich-discover #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Unit (
  tests
  ) where

import Test.Sandwich
import TestLib.Types

#insert_test_imports


tests :: NixSpec
tests = $(getSpecFromFolder defaultGetSpecFromFolderOptions)
