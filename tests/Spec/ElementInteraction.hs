
module Spec.ElementInteraction where

import Data.Aeson as A
import Data.String.Interpolate
import Test.Sandwich
import Test.Sandwich.Waits
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Element interaction" $ before "Open test page" openSimpleTestPage $ do
  it "click" $ do
    findElem (ByCSS "#numberLabel") >>= getText >>= (`shouldBe` "0")

    findElem (ByCSS "#incrementButton") >>= click
    findElem (ByCSS "#numberLabel") >>= getText >>= (`shouldBe` "1")

    findElem (ByCSS "#incrementButton") >>= click
    findElem (ByCSS "#numberLabel") >>= getText >>= (`shouldBe` "2")

  it "clearInput" $ do
    () <- executeJS [] [i|document.querySelector("\#input1").value = "asdf";|]
    waitUntil 5 $ do
      findElem (ByCSS "#input1") >>= (`prop` "value") >>= (`shouldBe` (Just (A.String "asdf")))

    findElem (ByCSS "#input1") >>= clearInput
    waitUntil 5 $ do
      findElem (ByCSS "#input1") >>= (`prop` "value") >>= (`shouldBe` (Just (A.String "")))

  it "sendKeys" $ do
    findElem (ByCSS "#input1") >>= sendKeys "fdsa"
    waitUntil 5 $ do
      findElem (ByCSS "#input1") >>= (`prop` "value") >>= (`shouldBe` (Just (A.String "fdsa")))
