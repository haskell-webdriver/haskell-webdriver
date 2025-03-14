
module Spec.ElementRetrieval where

import Data.String.Interpolate
import Test.Sandwich
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Element retrieval" $ before "Open test page" openSimpleTestPage $ do
  describe "findElem" $ do
    it "ByCSS" $ do
      el <- findElem (ByCSS "#incrementButton")
      info [i|Found elem: #{el}|]

    it "ByLinkText" $ do
      el <- findElem (ByLinkText "Click here")
      attr el "id" >>= (`shouldBe` (Just "click-here-link"))

    it "ByPartialLinkText" $ do
      el <- findElem (ByPartialLinkText "here")
      attr el "id" >>= (`shouldBe` (Just "click-here-link"))

    it "ByTag" $ do
      el <- findElem (ByTag "label")
      attr el "id" >>= (`shouldBe` (Just "numberLabel"))

    it "ByXPath" $ do
      el <- findElem (ByXPath "//a[@id='click-here-link']")
      getText el >>= (`shouldBe` "Click here")

  it "findElems" $ do
    elems <- findElems (ByCSS ".input-box")
    length elems `shouldBe` 3

  it "findElemFrom" $ do
    container <- findElem (ByCSS ".input-boxes")
    el <- findElemFrom container (ByCSS "#input1")
    attr el "class" >>= (`shouldBe` (Just "input-box"))

  it "findElemsFrom" $ do
    container <- findElem (ByCSS ".input-boxes")
    elems <- findElemsFrom container (ByCSS ".input-box")
    length elems `shouldBe` 2
