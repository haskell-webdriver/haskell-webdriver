module Test.WebDriver.IO where
import Test.WebDriver
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)

sleepIO :: Double -> IO ()
sleepIO seconds = Control.Concurrent.threadDelay (round (seconds * 1e6))

sleepWD :: WebDriver wd => Double -> wd ()
sleepWD = liftIO . sleepIO

printWD :: Show s => s -> WD ()
printWD = liftIO . print
