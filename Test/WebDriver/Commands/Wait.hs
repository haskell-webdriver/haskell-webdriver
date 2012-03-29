module Test.WebDriver.Commands.Wait 
       ( waitUntil, waitUntil'
       , waitWhile, waitWhile'
       ) where
import Test.WebDriver.Types
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception.Lifted
import Control.Concurrent
import Data.Time.Clock
import Prelude hiding (catch)

waitUntil :: Double -> WD a -> WD a
waitUntil = waitUntil' 250000

waitUntil' :: Int -> Double -> WD a -> WD a
waitUntil' waitAmnt t wd = waitLoop =<< liftIO getCurrentTime
  where
    timeout = realToFrac t
    waitLoop startTime = wd `catch` handler
      where
        handler (FailedCommand NoSuchElement _) = retry
        handler (WDZero _) = retry
        handler otherErr = throwIO otherErr
    
        retry = do
          now <- liftIO getCurrentTime
          if diffUTCTime now startTime >= timeout
            then 
              failedCommand Timeout "waitUntil': explicit wait timed out."
            else do
              liftIO . threadDelay $ waitAmnt
              waitLoop startTime

waitWhile :: Double -> WD a -> WD ()
waitWhile = waitWhile' 250000

waitWhile' :: Int -> Double -> WD a -> WD ()
waitWhile' waitAmnt t wd = waitLoop =<< liftIO getCurrentTime
  where
    timeout = realToFrac t
    waitLoop startTime = do 
      void wd `catch` handler
      now <- liftIO getCurrentTime
      if diffUTCTime now startTime >= timeout
        then
          failedCommand Timeout "waitUntil': explicit wait timed out."
        else do
          liftIO . threadDelay $ waitAmnt
          waitLoop startTime
      where
        handler (FailedCommand NoSuchElement _) = return ()
        handler (WDZero _) = return ()
        handler otherErr = throwIO otherErr
