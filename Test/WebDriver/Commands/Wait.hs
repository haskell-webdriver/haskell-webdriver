{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Test.WebDriver.Commands.Wait 
       ( expect, unexpected
       , (<||>), (<&&>)
       , expectAny, expectAll
                   
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
data ExpectFailed = ExpectFailed deriving (Show, Eq, Typeable)



unexpected :: WD a
unexpected = throwIO ExpectFailed

expect :: Bool -> WD ()
expect b
  | b         = return ()
  | otherwise = unexpected


(<&&>) :: Monad m  => m Bool -> m Bool -> m Bool
(<&&>) = liftM2 (&&)

(<||>) :: Monad m => m Bool -> m Bool -> m Bool
(<||>) = liftM2 (||)

expectAny :: (a -> WD Bool) -> [a] -> WD ()
expectAny p xs = expect . or =<< mapM p xs

expectAll :: (a -> WD Bool) -> [a] -> WD ()
expectAll p xs = expect . and =<< mapM p xs

waitUntil :: Double -> WD a -> WD a
waitUntil = waitUntil' 250000

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

waitWhile :: Double -> WD a -> WD ()
waitWhile = waitWhile' 250000

waitWhile' :: Int -> Double -> WD a -> WD ()
waitWhile' = wait' handler
  where
    handler retry = (`catches` [Handler handleFailedCommand
                               ,Handler handleExpectFailed
                               ]
                    ) . (>> retry)
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