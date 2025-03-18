
module Spec.UserPrompts where

import Test.Sandwich
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "User prompts" $ before "Open test page" openSimpleTestPage $ do
  it "dismissAlert" $ pending
  it "acceptAlert" $ pending
  it "getAlertText" $ pending
  it "replyToAlert" $ pending
