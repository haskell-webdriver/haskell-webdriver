
module Spec.Cookies where

import Data.Text
import Test.Sandwich
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Cookies" $ before "Open test page" openSimpleTestPage $ do
  it "cookies" $ do
    cookies >>= (`shouldBe` [])

    setCookie (mkCookie "cookie1" "value1")
    (getCookieBasics <$> cookies) >>= (`shouldBe` [("cookie1", "value1")])

  it "cookie" $ do
    c <- cookie "cookie1"
    getCookieBasics [c] `shouldBe` [("cookie1", "value1")]

  it "setCookie" $ do
    setCookie (mkCookie "cookie1" "value1")
    (getCookieBasics <$> cookies) >>= (`shouldBe` [("cookie1", "value1")])

  it "deleteCookie" $ do
    deleteCookie "cookie1"
    cookies >>= (`shouldBe` [])

  it "deleteCookies" $ do
    setCookie (mkCookie "cookie1" "value1")
    setCookie (mkCookie "cookie2" "value2")
    (getCookieBasics <$> cookies) >>= (`shouldBe` [
                                          ("cookie1", "value1")
                                          , ("cookie2", "value2")
                                          ])

    deleteCookies
    cookies >>= (`shouldBe` [])


getCookieBasics :: [Cookie] -> [(Text, Text)]
getCookieBasics = fmap go
  where
    go (Cookie {..}) = (cookName, cookValue)
