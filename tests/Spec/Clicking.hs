
module Spec.Clicking where

import Test.Sandwich
import Test.Sandwich.Contexts.Waits
import Test.WebDriver
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Clicking" $ before "Open test page" openSimpleTestPage $ do
  it "Clicks a button" $ do
    findElem (ByCSS "#numberLabel") >>= getText >>= (`shouldBe` "0")

    findElem (ByCSS "#incrementButton") >>= click
    findElem (ByCSS "#numberLabel") >>= getText >>= (`shouldBe` "1")

    findElem (ByCSS "#incrementButton") >>= click
    findElem (ByCSS "#numberLabel") >>= getText >>= (`shouldBe` "2")
