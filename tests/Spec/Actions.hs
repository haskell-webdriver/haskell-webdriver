
module Spec.Actions where

import Test.Sandwich
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Actions" $ before "Open test page" openSimpleTestPage $ do
  it "moveTo" $ pending
  it "moveToCenter" $ pending
  it "moveToFrom" $ pending
  it "clickWith" $ pending
  it "mouseDown" $ pending
  it "mouseUp" $ pending
  it "withMouseDown" $ pending
  it "doubleClick" $ pending
