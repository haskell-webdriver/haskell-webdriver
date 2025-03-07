{-# LANGUAGE OverloadedStrings  #-}

module Spec.SeleniumFileUploads where

import Test.Sandwich
import Test.WebDriver
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $
  describe "SeleniumFileUploads" $
    before "Open test page" (openStaticServerUrl "/test_file_upload.html") $ do
      it "Upload file and type its path to the input" $ do
        filePath <- uploadRawFile "/shopping.txt" 0 "Eggs, Ham, Cheese"
        sendKeys filePath =<< findElem (ByCSS "input")
      it "Page should display its name and size" $ do
        desc <- getText =<< findElem (ByCSS "#contents")
        desc `shouldBe` "shopping.txt 17"
