
module Spec.Windows where

import Data.String.Interpolate
import Test.Sandwich
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Windows" $ before "Open test page" openSimpleTestPage $ do
  it "getCurrentWindow / focusWindow" $ do
    h <- getCurrentWindow
    info [i|Got current window: #{h}|]

  it "focusWindow" $ do
    getCurrentWindow >>= focusWindow

  it "windows" $ do
    ws <- windows
    info [i|windows: #{ws}|]

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

  it "closeWindow" $ do
    getCurrentWindow >>= closeWindow
