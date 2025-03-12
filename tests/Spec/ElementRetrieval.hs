
module Spec.ElementRetrieval where

import Data.String.Interpolate
import Test.Sandwich
import Test.Sandwich.Waits
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Element retrieval" $ before "Open test page" openSimpleTestPage $ do
  it "findElem" $ do
    pending

  it "findElems" $ do
    pending

  it "findElemFrom" $ do
    pending

  it "findElemsFrom" $ do
    pending

  it "activeElem" $ do
    pending
