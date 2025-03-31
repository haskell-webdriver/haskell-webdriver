{-# LANGUAGE OverloadedStrings  #-}

module Spec.SeleniumSpecific.Uploads where

import Test.Sandwich
import Test.WebDriver
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $
  describe "Uploads" $
    before "Open test page" (openStaticServerUrl "/test_file_upload.html") $ do
      it "Upload file and type its path to the input" $ do
        filePath <- uploadRawFile "/shopping.txt" 0 "Eggs, Ham, Cheese"
        findElem (ByCSS "input") >>= sendKeys filePath

      it "Page should display its name and size" $ do
        findElem (ByCSS "#contents") >>= getText >>= (`shouldBe` "shopping.txt 17")
