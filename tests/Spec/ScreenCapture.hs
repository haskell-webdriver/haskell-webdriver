
module Spec.ScreenCapture where

import Test.Sandwich
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Screen capture" $ before "Open test page" openSimpleTestPage $ do
  it "saveScreenshot" $ pending
  it "screenshot" $ pending
  it "screenshotBase64" $ pending
