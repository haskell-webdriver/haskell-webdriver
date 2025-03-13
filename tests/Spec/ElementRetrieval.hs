
module Spec.ElementRetrieval where

import Test.Sandwich
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Element retrieval" $ before "Open test page" openSimpleTestPage $ do
  describe "findElem" $ do
    it "ByCSS" $ pending
    it "ByLinkText" $ pending
    it "ByPartialLinkText" $ pending
    it "ByTag" $ pending
    it "ByXPath" $ pending

  it "findElems" $ do
    pending

  it "findElemFrom" $ do
    pending

  it "findElemsFrom" $ do
    pending
