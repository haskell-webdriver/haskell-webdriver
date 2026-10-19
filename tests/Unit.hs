{-# OPTIONS_GHC -F -pgmF sandwich-discover #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Unit (
  tests
  ) where

import Test.Sandwich

#insert_test_imports


tests :: TopSpec
tests = $(getSpecFromFolder defaultGetSpecFromFolderOptions)
