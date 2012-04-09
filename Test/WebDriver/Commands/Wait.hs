{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Test.WebDriver.Commands.Wait 
       ( -- * Expected conditions
         ExpectFailed, expect, unexpected
         -- ** Convenience functions
       , expectAny, expectAll
       , (<||>), (<&&>)
                   
         -- * Explicit waiting
       , waitUntil, waitUntil'
       , waitWhile, waitWhile'
       ) where
import Test.WebDriver.Types
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception.Lifted
import Control.Concurrent
import Data.Time.Clock
import Data.Typeable
import Prelude hiding (catch)

instance Exception ExpectFailed
-- |An exception representing a failure of an expected condition.
data ExpectFailed = ExpectFailed deriving (Show, Eq, Typeable)

-- |Raises ExpectFailed.
unexpected :: WD a
unexpected = throwIO ExpectFailed

-- |An expected condition. This function allows you to express assertions in
-- your explicit wait. This function raises 'ExpectFailed' if the given
-- boolean is False, and otherwise does nothing.
expect :: Bool -> WD ()
expect b
  | b         = return ()
  | otherwise = unexpected


-- |Lifted boolean and
(<&&>) :: Monad m  => m Bool -> m Bool -> m Bool
(<&&>) = liftM2 (&&)

-- |Lifted boolean or
(<||>) :: Monad m => m Bool -> m Bool -> m Bool
(<||>) = liftM2 (||)

expectAny :: (a -> WD Bool) -> [a] -> WD ()
expectAny p xs = expect . or =<< mapM p xs

expectAll :: (a -> WD Bool) -> [a] -> WD ()
expectAll p xs = expect . and =<< mapM p xs

-- |Wait until either the given action succeeds or the timeout is reached.
-- The action will be retried every .25 seconds until no ExpectFailed or
-- NoSuchElement exceptions occur. The timeout value is expressed in seconds.
waitUntil :: Double -> WD a -> WD a
waitUntil = waitUntil' 250000

-- |Similar to waitUntil but allows you to also specify the poll frequency
-- of the WD action. The frequency is expressed as an integer in microseconds.
waitUntil' :: Int -> Double -> WD a -> WD a
waitUntil' = wait' handler
  where
    handler retry = (`catches` [Handler handleFailedCommand
                               ,Handler handleExpectFailed]
                    )
      where
        handleFailedCommand (FailedCommand NoSuchElement _) = retry
        handleFailedCommand err = throwIO err
                              
        handleExpectFailed (_ :: ExpectFailed) = retry

-- |Like waitWhile, but retries the action until it fails or until the timeout
-- is exceeded.
waitWhile :: Double -> WD a -> WD ()
waitWhile = waitWhile' 250000

-- |Like waitWhile', but retries the action until it either fails or 
-- until the timeout is exceeded.
waitWhile' :: Int -> Double -> WD a -> WD ()
waitWhile' = wait' handler
  where
    handler retry wd = do 
      void wd `catches` [Handler handleFailedCommand
                        ,Handler handleExpectFailed
                        ]
      retry
      where
        handleFailedCommand (FailedCommand NoSuchElement _) = return ()
        handleFailedCommand err = throwIO err
                               
        handleExpectFailed (_ :: ExpectFailed) = return ()
    
wait' :: (WD b -> WD a -> WD b) -> Int -> Double -> WD a -> WD b
wait' handler waitAmnt t wd = waitLoop =<< liftIO getCurrentTime
  where timeout = realToFrac t
        waitLoop startTime = handler retry wd
          where 
            retry = do
              now <- liftIO getCurrentTime
              if diffUTCTime now startTime >= timeout
                then 
                  failedCommand Timeout "waitUntil': explicit wait timed out."
                else do
                  liftIO . threadDelay $ waitAmnt
                  waitLoop startTime