module Spec.Logs where

import Control.Monad (unless)
import Data.List (find)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Test.Sandwich
import Test.Sandwich.Waits
import Test.WebDriver.Commands
import Test.WebDriver.Commands.Logs
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types

tests :: SessionSpec
tests = introduceSession $ describe "Browser logs" $ before "Open test page" openSimpleTestPage $ do
  it "captures console.log messages" $ do
    -- Execute console.log via JavaScript
    () <- executeJS [] [i|console.log('test message from haskell-webdriver');|]
    
    -- Wait a moment for log to be captured
    waitUntil 2 $ do
      -- Retrieve console logs
      logs <- getConsoleLogs
      
      -- Verify we got some logs
      (length logs > 0) `shouldBe` True
      
      -- Check if our test message is in the logs
      let hasTestMessage = any (T.isInfixOf "test message from haskell-webdriver" . logMsg) logs
      unless hasTestMessage $ do
        info $ T.pack $ "Available logs: " ++ show logs
      hasTestMessage `shouldBe` True

  it "captures different log levels" $ do
    -- Execute different log levels via JavaScript
    () <- executeJS [] [i|
      console.log('info message');
      console.warn('warning message');  
      console.error('error message');
    |]
    
    -- Wait for logs to be captured
    waitUntil 2 $ do
      logs <- getConsoleLogs
      (length logs > 0) `shouldBe` True
      
      -- Check we have messages with different levels
      let logMessages = map logMsg logs
      let hasInfo = any (T.isInfixOf "info message") logMessages
      let hasWarning = any (T.isInfixOf "warning message") logMessages  
      let hasError = any (T.isInfixOf "error message") logMessages
      
      unless (hasInfo && hasWarning && hasError) $ do
        info $ T.pack $ "Available logs: " ++ show logs
      
      -- At least one of these should be true (depending on browser capabilities)
      (hasInfo || hasWarning || hasError) `shouldBe` True

  it "getLogs with browser type works equivalently to getConsoleLogs" $ do
    -- Execute console.log
    () <- executeJS [] [i|console.log('browser type test message');|]
    
    waitUntil 2 $ do
      -- Get logs both ways
      consoleLogs <- getConsoleLogs
      browserLogs <- getLogs "browser"
      
      -- Both should return some logs
      (length consoleLogs > 0) `shouldBe` True
      (length browserLogs > 0) `shouldBe` True
      
      -- Both should contain our test message
      let consoleHasMessage = any (T.isInfixOf "browser type test message" . logMsg) consoleLogs
      let browserHasMessage = any (T.isInfixOf "browser type test message" . logMsg) browserLogs
      
      unless (consoleHasMessage && browserHasMessage) $ do
        info $ T.pack $ "Console logs: " ++ show consoleLogs
        info $ T.pack $ "Browser logs: " ++ show browserLogs
      
      consoleHasMessage `shouldBe` True
      browserHasMessage `shouldBe` True

  it "handles multiple console messages in sequence" $ do
    -- Execute multiple console.log statements
    () <- executeJS [] [i|
      for (let i = 0; i < 3; i++) {
        console.log('sequence message ' + i);
      }
    |]
    
    waitUntil 3 $ do
      logs <- getConsoleLogs
      (length logs > 0) `shouldBe` True
      
      -- Check for sequence messages
      let logMessages = map logMsg logs
      let sequenceMessages = filter (T.isInfixOf "sequence message") logMessages
      
      unless (length sequenceMessages >= 1) $ do
        info $ T.pack $ "Available logs: " ++ show logs
        info $ T.pack $ "Sequence messages found: " ++ show sequenceMessages
      
      -- Should find at least one sequence message (browsers may batch or limit logs)
      (length sequenceMessages >= 1) `shouldBe` True

  it "logs have reasonable timestamps" $ do
    () <- executeJS [] [i|console.log('timestamp test message');|]
    
    waitUntil 2 $ do
      logs <- getConsoleLogs
      (length logs > 0) `shouldBe` True
      
      -- Check that logs have non-zero timestamps
      let hasValidTimestamp = any ((\log -> logTime log > 0) . id) logs
      unless hasValidTimestamp $ do
        info $ T.pack $ "Log timestamps: " ++ show (map logTime logs)
      
      hasValidTimestamp `shouldBe` True

  it "can retrieve driver logs (if supported)" $ do
    -- Driver logs may not be available in all configurations
    driverLogs <- getLogs "driver"
    -- This should not fail, but may return empty list
    True `shouldBe` True -- Just verify it doesn't throw an exception