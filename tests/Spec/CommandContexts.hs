
module Spec.CommandContexts where

import Data.String.Interpolate
import Test.Sandwich
import Test.Sandwich.Waits
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Command contexts" $ before "Open test page" openSimpleTestPage $ do
  it "getCurrentWindow / focusWindow" $ do
    h <- getCurrentWindow
    info [i|Got current window: #{h}|]

  it "focusWindow" $ do
    getCurrentWindow >>= focusWindow

  it "windows" $ do
    ws <- windows
    info [i|windows: #{ws}|]

  it "focusFrame" $ do
    (length <$> findElems (ByCSS ".input-box")) >>= (`shouldBe` 3)

    findElem (ByCSS "#frame1") >>= focusFrame . WithElement
    (length <$> findElems (ByCSS ".input-box")) >>= (`shouldBe` 1)

  it "focusParentFrame" $ do
    focusParentFrame
    (length <$> findElems (ByCSS ".input-box")) >>= (`shouldBe` 3)

  it "maximize" $ do
    maximize

  it "minimize" $ do
    minimize

  it "fullscreen" $ do
    fullscreen

  describe "rect" $ do
    it "getWindowRect" $ do
      rect <- getWindowRect
      info [i|Got rect: #{rect}|]

    it "setWindowRect" $ do
      -- TODO: modify the rect
      getWindowRect >>= setWindowRect

  it "windows / closeWindow" $ do
    findElem (ByCSS "#openWindowButton") >>= click

    waitUntil 30 $ do
      (length <$> windows) >>= (`shouldBe` 2)

    getCurrentWindow >>= closeWindow

    waitUntil 30 $ do
      (length <$> windows) >>= (`shouldBe` 1)
