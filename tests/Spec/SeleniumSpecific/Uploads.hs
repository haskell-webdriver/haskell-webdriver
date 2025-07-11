{-# LANGUAGE OverloadedStrings #-}

module Spec.SeleniumSpecific.Uploads where

import Control.Monad.IO.Class
import qualified Data.Text as T
import System.FilePath
import Test.Sandwich
import Test.WebDriver
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Uploads" $ before "Open test page" (openStaticServerUrl "/test_file_upload.html") $ do
  it "Upload file with sendKeys" $ do
    Just dir <- getCurrentFolder
    let fp = dir </> "test-file.txt"
    liftIO $ writeFile fp "file contents"
    findElem (ByCSS "input") >>= sendKeys (T.pack fp)
    findElem (ByCSS "#contents") >>= getText >>= (`shouldBe` "test-file.txt 13")

  describe "Selenium specific" $ do
    it "upload file to grid and use sendKeys to upload to browser" $ do
      pendingOnNonSelenium
      filePath <- seleniumUploadRawFile "/shopping.txt" 0 "Eggs, Ham, Cheese"
      findElem (ByCSS "input") >>= sendKeys filePath
      findElem (ByCSS "#contents") >>= getText >>= (`shouldBe` "shopping.txt 17")
