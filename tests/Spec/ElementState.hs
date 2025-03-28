
module Spec.ElementState where

import Data.Aeson as A
import Data.String.Interpolate
import Data.Text as T
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
    findElem (ByTag "label") >>= (`attr` "id") >>= (`shouldBe` (Just "numberLabel"))

  it "prop" $ do
    findElem (ByCSS "#checkbox") >>= isSelected >>= (`shouldBe` False)
    findElem (ByCSS "#checkbox") >>= (`prop` "checked") >>= (`shouldBe` (Just (A.Bool False)))

    findElem (ByCSS "#checkbox") >>= click
    findElem (ByCSS "#checkbox") >>= isSelected >>= (`shouldBe` True)
    findElem (ByCSS "#checkbox") >>= (`prop` "checked") >>= (`shouldBe` (Just (A.Bool True)))

  it "cssProp" $ do
    Just color <- findElem (ByCSS "#input-red") >>= (`cssProp` "color")
    -- Depending on browser, this can produce a couple different values
    ["rgb(255,0,0)", "rgba(255,0,0,1)"] `shouldContain` [T.filter (/= ' ') color]

  it "getText" $ do
    findElem (ByCSS "#click-here-link") >>= getText >>= (`shouldBe` "Click here")

  it "tagName" $ do
    findElem (ByCSS "#incrementButton") >>= tagName >>= (`shouldBe` "button")
    findElem (ByCSS "#numberLabel") >>= tagName >>= (`shouldBe` "label")
    findElem (ByCSS "a") >>= tagName >>= (`shouldBe` "a")

  it "elemRect" $ do
    rect1 <- findElem (ByCSS "#input1") >>= elemRect
    info [i|rect1: #{rect1}|]
    rect2 <- findElem (ByCSS "#input2") >>= elemRect
    info [i|rect2: #{rect2}|]

    rectX rect1 `shouldBe` rectX rect2
    (rectY rect1 < rectY rect2) `shouldBe` True

  it "isEnabled" $ do
    findElem (ByCSS "#input1") >>= isEnabled >>= (`shouldBe` True)
    findElem (ByCSS "#input-disabled") >>= isEnabled >>= (`shouldBe` False)
