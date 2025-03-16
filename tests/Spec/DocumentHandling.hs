
module Spec.DocumentHandling where

import Data.Aeson as A
import Data.String.Interpolate
import Data.Text as T
import Test.Sandwich
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Document handling" $ before "Open test page" openSimpleTestPage $ do
  it "getSource" $ pending
  it "executeJS" $ pending
  it "asyncJS" $ pending
