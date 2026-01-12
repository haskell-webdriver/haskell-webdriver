{-# LANGUAGE NumericUnderscores #-}

module Spec.LogsBiDi where

import Control.Monad.IO.Class
import Data.Foldable (toList)
import Data.Sequence
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Test.Sandwich
import Test.Sandwich.Waits
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types
import UnliftIO.IORef


tests :: SessionSpec
tests = introduceSession $ describe "Browser logs via BiDi" $ before "Open test page" openSimpleTestPage $ do
  it "captures console.log messages" $ do
    -- This test should pass on Selenium 4, but we get an error connecting to the BiDi socket.
    pendingOnSelenium

    allLogEntries <- newIORef (mempty :: Seq LogEntry)

    let cb logEntry = atomicModifyIORef' allLogEntries (\x -> (x |> logEntry, ()))
    withRecordLogsViaBiDi defaultBiDiOptions cb $ do
      ignoreReturn $ executeJS [] [i|console.log('log message from haskell-webdriver');|]
      ignoreReturn $ executeJS [] [i|console.warn('warn message from haskell-webdriver');|]
      ignoreReturn $ executeJS [] [i|console.error('error message from haskell-webdriver');|]

      waitUntil 15 $ do
        entries <- toList <$> readIORef allLogEntries
        someMessageShouldContain entries "log message"
        someMessageShouldContain entries "warn message"
        someMessageShouldContain entries "error message"


someMessageShouldContain :: MonadIO m => [LogEntry] -> Text -> m ()
someMessageShouldContain entries needle = case [x | x@(LogEntry {..}) <- entries, needle `T.isInfixOf` logMsg] of
  [] -> expectationFailure [i|Couldn't find a log message containing needle '#{needle}'|]
  _ -> return ()
