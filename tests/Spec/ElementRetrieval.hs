
module Spec.ElementRetrieval where

import Test.Sandwich
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Element retrieval" $ before "Open test page" openSimpleTestPage $ do
  describe "findElem" $ do
    it "ByCSS" $ do
      findElem (ByCSS "#incrementButton") >>= getText >>= (`shouldBe` "Increment")

    it "ByLinkText" $ do
      findElem (ByLinkText "Click here") >>= (`attr` "id") >>= (`shouldBe` (Just "click-here-link"))

    it "ByPartialLinkText" $ do
      findElem (ByPartialLinkText "here") >>= (`attr` "id") >>= (`shouldBe` (Just "click-here-link"))

    it "ByTag" $ do
      findElem (ByTag "label") >>= (`attr` "id") >>= (`shouldBe` (Just "numberLabel"))

    it "ByXPath" $ do
      findElem (ByXPath "//a[@id='click-here-link']") >>= getText >>= (`shouldBe` "Click here")

  it "findElems" $ do
    (length <$> findElems (ByCSS ".input-box")) >>= (`shouldBe` 3)

  it "findElemFrom" $ do
    container <- findElem (ByCSS ".input-boxes")
    findElemFrom container (ByCSS "#input1") >>= (`attr` "class") >>= (`shouldBe` (Just "input-box"))

  it "findElemsFrom" $ do
    container <- findElem (ByCSS ".input-boxes")
    (length <$> findElemsFrom container (ByCSS ".input-box")) >>= (`shouldBe` 2)

  it "activeElem" $ do
    container <- findElem (ByCSS "#input1")
    click container

    el <- activeElem
    el `shouldBe` container
