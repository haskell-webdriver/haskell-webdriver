{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Spec.CodeMirror where

import Control.Concurrent
import Control.Monad.IO.Class
import qualified Data.Text as T
import System.FilePath
import Test.Sandwich
import Test.WebDriver
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "CodeMirror" $ before "Open test page" (openStaticServerUrl "/test_codemirror.html") $ do
  it "Send text with sendKeys" $ do
    liftIO $ threadDelay 3_000_000
    findElem (ByCSS ".CodeMirror textarea") >>= sendKeys "asdf"
