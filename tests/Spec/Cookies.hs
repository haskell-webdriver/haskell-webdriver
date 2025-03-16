
module Spec.Cookies where

import Data.Aeson as A
import Data.String.Interpolate
import Data.Text as T
import Test.Sandwich
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Cookies" $ before "Open test page" openSimpleTestPage $ do
  it "cookies" $ pending
  it "cookie" $ pending
  it "asyncJS" $ pending
  it "deleteCookie" $ pending
  it "deleteVisibleCookies" $ pending
  it "deleteCookieByName" $ pending
