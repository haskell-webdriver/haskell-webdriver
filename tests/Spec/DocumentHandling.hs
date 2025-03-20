
module Spec.DocumentHandling where

import Data.Aeson as A
import Data.String.Interpolate
import Data.Text
import Test.Sandwich
import Test.Sandwich.Waits
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Document handling" $ before "Open test page" openSimpleTestPage $ do
  it "getSource" $ do
    src <- getSource
    src `textShouldContain` "Test page"
    src `textShouldContain` [i|<a id="click-here-link" href="\#foo">Click here</a>|]

  it "executeJS" $ do
    () <- executeJS [] [i|document.querySelector("\#input1").value = "asdf";|]

    waitUntil 5 $ do
      findElem (ByCSS "#input1") >>= (`prop` "value") >>= (`shouldBe` (Just (A.String "asdf")))

      executeJS [] [i|return document.querySelector("\#input1").value;|]
        >>= (`shouldBe` ("asdf" :: Text))

    executeJS [] [i|return document.title;|] >>= (`shouldBe` ("Test page" :: String))

  it "asyncJS" $ do
    asyncJS [] [i|setTimeout(() => arguments[0](42), 5);|]
      >>= (`shouldBe` (Just (42 :: Int)))

    asyncJS [] [i|arguments[0](document.title);|] >>= (`shouldBe` (Just ("Test page" :: String)))
