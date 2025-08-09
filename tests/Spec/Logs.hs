module Spec.Logs where

import Control.Monad (unless)
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
  it "captures console.log messages" $ do
    ignoreReturn $ executeJS [] [i|console.log('test message from haskell-webdriver');|]

    logTypes <- getLogTypes
    info [i|logTypes: #{logTypes}|]

    waitUntil 15 $ do
      logs <- getLogs "server"
      info [i|logs: #{logs}|]

      consoleLogs <- getConsoleLogs
      info [i|consoleLogs: #{consoleLogs}|]
      let hasTestMessage = any (T.isInfixOf "test message from haskell-webdriver" . logMsg) consoleLogs
      hasTestMessage `shouldBe` True

  it "captures different log levels" $ do
    -- Execute different log levels via JavaScript
    ignoreReturn $ executeJS [] [i|
      console.log('info message');
      console.warn('warning message');
      console.error('error message');
    |]

    logTypes <- getLogTypes
    info [i|logTypes: #{logTypes}|]

    waitUntil 15 $ do
      logs <- getLogs "server"
      info [i|logs: #{logs}|]

      consoleLogs <- getConsoleLogs
      info [i|consoleLogs: #{consoleLogs}|]

      let logMessages = map logMsg consoleLogs
      let hasInfo = any (T.isInfixOf "info message") logMessages
      let hasWarning = any (T.isInfixOf "warning message") logMessages
      let hasError = any (T.isInfixOf "error message") logMessages

      (hasInfo || hasWarning || hasError) `shouldBe` True

  -- it "getLogs with browser type works equivalently to getConsoleLogs" $ do
  --   -- Execute console.log
  --   () <- executeJS [] [i|console.log('browser type test message');|]

  --   waitUntil 2 $ do
  --     -- Get logs both ways
  --     consoleLogs <- getConsoleLogs
  --     browserLogs <- getLogs "browser"

  --     -- Both should return some logs
  --     (length consoleLogs > 0) `shouldBe` True
  --     (length browserLogs > 0) `shouldBe` True

  --     -- Both should contain our test message
  --     let consoleHasMessage = any (T.isInfixOf "browser type test message" . logMsg) consoleLogs
  --     let browserHasMessage = any (T.isInfixOf "browser type test message" . logMsg) browserLogs

  --     unless (consoleHasMessage && browserHasMessage) $ do
  --       info $ T.pack $ "Console logs: " ++ show consoleLogs
  --       info $ T.pack $ "Browser logs: " ++ show browserLogs

  --     consoleHasMessage `shouldBe` True
  --     browserHasMessage `shouldBe` True

  -- it "handles multiple console messages in sequence" $ do
  --   -- Execute multiple console.log statements
  --   () <- executeJS [] [i|
  --     for (let i = 0; i < 3; i++) {
  --       console.log('sequence message ' + i);
  --     }
  --   |]

  --   waitUntil 3 $ do
  --     logs <- getConsoleLogs
  --     (length logs > 0) `shouldBe` True

  --     -- Check for sequence messages
  --     let logMessages = map logMsg logs
  --     let sequenceMessages = filter (T.isInfixOf "sequence message") logMessages

  --     unless (length sequenceMessages >= 1) $ do
  --       info $ T.pack $ "Available logs: " ++ show logs
  --       info $ T.pack $ "Sequence messages found: " ++ show sequenceMessages

  --     -- Should find at least one sequence message (browsers may batch or limit logs)
  --     (length sequenceMessages >= 1) `shouldBe` True

  -- it "can retrieve driver logs (if supported)" $ do
  --   driverLogs <- getLogs "driver"
  --   info [i|Got driverLogs: #{driverLogs}|]
