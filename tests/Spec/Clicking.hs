
module Spec.Clicking where

import Data.Aeson as A
import Test.Sandwich
import Test.WebDriver
import TestLib.Contexts.Session
import TestLib.Types


tests :: SpecWithBrowserDeps
tests = introduceSession $ describe "Clicking" $ do
  it "works" $ do
    openPage "http://www.wikipedia.org/"
    r <- asyncJS [] "arguments[0]();"
    r `shouldBe` (Just A.Null)
