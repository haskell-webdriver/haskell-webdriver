
module Spec.Windows where

import Data.String.Interpolate
import Test.Sandwich
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Windows" $ before "Open test page" openSimpleTestPage $ do
  it "getCurrentWindow" $ do
    pending

  it "getWindowRect" $ do
    rect <- getWindowRect
    info [i|Got rect: #{rect}|]

  it "setWindowRect" $ do
    pending

  it "getWindowSize" $ do
    size <- getWindowSize
    info [i|Got size: #{size}|]

  it "setWindowSize" $ do
    pending

  it "closeWindow" $ do
    pending
