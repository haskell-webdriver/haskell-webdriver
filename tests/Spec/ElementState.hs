
module Spec.ElementState where

import Data.String.Interpolate
import Test.Sandwich
import Test.Sandwich.Waits
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Element state" $ before "Open test page" openSimpleTestPage $ do
  it "isSelected" $ do
    pending

  it "attr" $ do
    pending

  it "prop" $ do
    pending

  it "cssProp" $ do
    pending

  it "getText" $ do
    pending

  it "tagName" $ do
    pending

  it "elemRect" $ do
    pending

  it "isEnabled" $ do
    pending
