module Test.WebDriver.Commands.Wait 
       ( waitUntil, waitUntil'
       , waitWhile, waitWhile'
       ) where
import Test.WebDriver.Types
import Control.Concurrent
import Control.Monad.Error
import Data.Time.Clock

waitUntil :: Double -> WD a -> WD a
waitUntil = waitUntil' 250000

waitUntil' :: Int -> Double -> WD a -> WD a
waitUntil' waitAmnt t wd = waitLoop =<< liftIO getCurrentTime
  where
    timeout = realToFrac t
    waitLoop startTime = wd `catchError` handler
      where
        handler (NoSuchElement _) = retry
        handler (WDZero _) = retry
        handler otherErr = throwError otherErr
    
        retry = do
          now <- liftIO getCurrentTime
          if diffUTCTime now startTime >= timeout
            then 
              throwError $ Timeout "waitUntil': explicit wait timed out."
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
      void wd `catchError` handler
      now <- liftIO getCurrentTime
      if diffUTCTime now startTime >= timeout
        then
          throwError $ Timeout "waitUntil': explicit wait timed out."
        else do
          liftIO . threadDelay $ waitAmnt
          waitLoop startTime
      where
        handler (NoSuchElement _) = return ()
        handler (WDZero _) = return ()
        handler otherErr = throwError otherErr
