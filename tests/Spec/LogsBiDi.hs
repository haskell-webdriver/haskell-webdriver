{-# LANGUAGE NumericUnderscores #-}

module Spec.LogsBiDi where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.String.Interpolate
import Test.Sandwich
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Browser logs via BiDi" $ before "Open test page" openSimpleTestPage $ do
  it "captures console.log messages" $ do
    let cb logEntry = info [i|GOT LOG ENTRY: #{logEntry}|]
    withRecordBiDiLogs cb $ do
      ignoreReturn $ executeJS [] [i|console.log('log message from haskell-webdriver');|]
      ignoreReturn $ executeJS [] [i|console.warn('warn message from haskell-webdriver');|]
      ignoreReturn $ executeJS [] [i|console.error('error message from haskell-webdriver');|]
      liftIO $ threadDelay 3_000_000
