
module Spec.Navigation where

import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI
import Test.Sandwich
import Test.WebDriver.Commands
import Test.WebDriver.Types
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Navigation" $ before "Open test page" openSimpleTestPage $ do
  it "getCurrentURL / getTitle" $ do
    urlShouldEndWith "test.html"
    getTitle >>= (`shouldBe` "Test page")

  it "openPage / getCurrentURL / getTitle" $ do
    url <- parseAbsoluteURI <$> getCurrentURL >>= \case
      Nothing -> expectationFailure [i|Couldn't parse URI|]
      Just u -> pure u
    openPage $ show (url { uriPath = "/test2.html" })
    urlShouldEndWith "test2.html"
    getTitle >>= (`shouldBe` "Test page 2")

  it "back" $ do
    back
    urlShouldEndWith "test.html"

  it "forward" $ do
    forward
    urlShouldEndWith "test2.html"

  it "refresh" $ do
    refresh

  it "getTitle" $ do
    getTitle >>= (`shouldBe` "Test page 2")


urlShouldEndWith :: (WebDriver m) => Text -> m ()
urlShouldEndWith suffix = do
  url <- getCurrentURL
  (suffix `T.isSuffixOf` (T.pack url)) `shouldBe` True
