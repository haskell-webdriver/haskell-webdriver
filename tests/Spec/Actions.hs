{-# LANGUAGE NumericUnderscores #-}

module Spec.Actions where

import Test.Sandwich
import Test.WebDriver
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types
import UnliftIO.Concurrent


tests :: SessionSpec
tests = introduceSession $ describe "Actions" $ before "Open test page" openSimpleTestPage $ do
  it "moveTo" $ pending

  it "moveToCenter" $ do
    box <- findElem (ByCSS "#clickable-box")
    moveToCenter box
    clickWith LeftButton
    threadDelay 120_000_000

  it "moveToFrom" $ pending

  it "clickWith" $ pending

  it "mouseDown" $ pending

  it "mouseUp" $ pending

  it "withMouseDown" $ pending

  it "doubleClick" $ pending
