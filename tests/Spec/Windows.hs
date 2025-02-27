
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
    focusWindow h

  it "getWindowRect" $ do
    rect <- getWindowRect
    info [i|Got rect: #{rect}|]

  it "setWindowRect" $ do
    -- TODO: modify the rect
    getWindowRect >>= setWindowRect

  it "getWindowSize" $ do
    size <- getWindowSize
    info [i|Got size: #{size}|]

  it "setWindowSize" $ do
    -- TODO: modify the size
    getWindowSize >>= setWindowSize

  it "closeWindow" $ do
    getCurrentWindow >>= closeWindow
