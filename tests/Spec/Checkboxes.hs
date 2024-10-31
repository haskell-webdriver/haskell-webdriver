
module Spec.Checkboxes where

import Test.Sandwich
import Test.WebDriver
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Checkboxes" $ before "Open test page" openSimpleTestPage $ do
  it "Toggles a checkbox" $ do
    findElem (ByCSS "#checkbox") >>= isSelected >>= (`shouldBe` False)

    findElem (ByCSS "#checkbox") >>= click
    findElem (ByCSS "#checkbox") >>= isSelected >>= (`shouldBe` True)

    findElem (ByCSS "#checkbox") >>= click
    findElem (ByCSS "#checkbox") >>= isSelected >>= (`shouldBe` False)
