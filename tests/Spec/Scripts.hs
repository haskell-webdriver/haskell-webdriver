{-# LANGUAGE NumericUnderscores #-}

module Spec.Scripts where

import Data.String.Interpolate
import Test.Sandwich
import Test.WebDriver
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Scripts" $ before "Open test page" openSimpleTestPage $ do
  it "Does synchronous script eval" $ do
    executeJS [] [i|return document.title;|] >>= (`shouldBe` ("Test page" :: String))

  it "Does asynchronous script eval" $ do
      asyncJS [] [i|arguments[0](document.title);|] >>= (`shouldBe` (Just ("Test page" :: String)))
