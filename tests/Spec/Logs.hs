module Spec.Logs where

import Data.String.Interpolate
import qualified Data.Text as T
import Test.Sandwich
import Test.Sandwich.Waits
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Browser logs" $ before "Open test page" openSimpleTestPage $ do
  it "captures different log levels" $ do
    pendingOnFirefox

    ignoreReturn $ executeJS [] [i|
      console.log('info message');
      console.warn('warning message');
      console.error('error message');
    |]
    waitUntil 15 $ do
      logs <- getLogs "server"
      info [i|logs: #{logs}|]

      consoleLogs <- getLogs "browser"
      info [i|consoleLogs: #{consoleLogs}|]

      let logMessages = map logMsg consoleLogs
      let hasInfo = any (T.isInfixOf "info message") logMessages
      let hasWarning = any (T.isInfixOf "warning message") logMessages
      let hasError = any (T.isInfixOf "error message") logMessages

      (hasInfo || hasWarning || hasError) `shouldBe` True
