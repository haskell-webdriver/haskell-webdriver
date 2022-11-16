module Test.WebDriver.IO where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Test.WebDriver
import Test.WebDriver.Class (WebDriver)


sleepIO :: Double -> IO ()
sleepIO seconds = Control.Concurrent.threadDelay (round (seconds * 1e6))

sleepWD :: (MonadIO wd, WebDriver wd) => Double -> wd ()
sleepWD = liftIO . sleepIO

printWD :: Show s => s -> WD ()
printWD = liftIO . print
