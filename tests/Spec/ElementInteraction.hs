
module Spec.ElementInteraction where

import Test.Sandwich
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
    pending

  it "sendKeys" $ do
    pending
