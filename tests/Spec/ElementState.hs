
module Spec.ElementState where

import Test.Sandwich
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Element state" $ before "Open test page" openSimpleTestPage $ do
  it "isSelected" $ do
    findElem (ByCSS "#checkbox") >>= isSelected >>= (`shouldBe` False)

    findElem (ByCSS "#checkbox") >>= click
    findElem (ByCSS "#checkbox") >>= isSelected >>= (`shouldBe` True)

    findElem (ByCSS "#checkbox") >>= click
    findElem (ByCSS "#checkbox") >>= isSelected >>= (`shouldBe` False)

  it "attr" $ do
    el <- findElem (ByTag "label")
    attr el "id" >>= (`shouldBe` (Just "numberLabel"))

  it "prop" $ do
    pending

  it "cssProp" $ do
    findElem (ByCSS "#input-red") >>= (`cssProp` "color") >>= (`shouldBe` (Just "red"))

  it "getText" $ do
    findElem (ByCSS "#click-here-link") >>= getText >>= (`shouldBe` "Click here")

  it "tagName" $ do
    findElem (ByCSS "#incrementButton") >>= tagName >>= (`shouldBe` "button")
    findElem (ByCSS "#numberLabel") >>= tagName >>= (`shouldBe` "label")
    findElem (ByCSS "a") >>= tagName >>= (`shouldBe` "a")

  it "elemRect" $ do
    pending

  it "isEnabled" $ do
    findElem (ByCSS "#input1") >>= isEnabled >>= (`shouldBe` True)
    findElem (ByCSS "#input-disabled") >>= isEnabled >>= (`shouldBe` False)
