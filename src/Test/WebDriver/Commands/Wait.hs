{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, FlexibleContexts #-}
module Test.WebDriver.Commands.Wait 
       ( -- * Wait on expected conditions
         waitUntil, waitUntil'
       , waitWhile, waitWhile'
         -- * Expected conditions
       , ExpectFailed, expect, unexpected
         -- ** Convenience functions
       , onTimeout
       , expectAny, expectAll
       , ifM, (<||>), (<&&>), notM
       ) where
import Test.WebDriver.Exceptions
import Test.WebDriver.Classes
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Exception.Lifted
import Control.Concurrent
import Data.Time.Clock
import Data.Typeable
import Control.Conditional (ifM, (<||>), (<&&>), notM)
import Prelude hiding (catch)

instance Exception ExpectFailed
-- |An exception representing the failure of an expected condition.
data ExpectFailed = ExpectFailed deriving (Show, Eq, Typeable)

-- |throws 'ExpectFailed'. This is nice for writing your own abstractions.
unexpected :: MonadBaseControl IO m => m a
unexpected = throwIO ExpectFailed

-- |An expected condition. This function allows you to express assertions in
-- your explicit wait. This function raises 'ExpectFailed' if the given
-- boolean is False, and otherwise does nothing.
expect :: MonadBaseControl IO m => Bool -> m ()
expect b
  | b         = return ()
  | otherwise = unexpected

-- |Apply a monadic predicate to every element in a list, and 'expect' that 
-- at least one succeeds.
expectAny :: MonadBaseControl IO m => (a -> m Bool) -> [a] -> m ()
expectAny p xs = expect . or =<< mapM p xs

-- |Apply a monadic predicate to every element in a list, and 'expect' that all 
-- succeed.
expectAll :: MonadBaseControl IO m => (a -> m Bool) -> [a] -> m ()
expectAll p xs = expect . and =<< mapM p xs

-- |Wait until either the given action succeeds or the timeout is reached.
-- The action will be retried every .5 seconds until no 'ExpectFailed' or
-- 'FailedCommand' 'NoSuchElement' exceptions occur. If the timeout is reached, 
-- then a 'Timeout' exception will be raised. The timeout value 
-- is expressed in seconds.
waitUntil :: SessionState m => Double -> m a -> m a
waitUntil = waitUntil' 500000

-- |Similar to 'waitUntil' but allows you to also specify the poll frequency
-- of the 'WD' action. The frequency is expressed as an integer in microseconds.
waitUntil' :: SessionState m => Int -> Double -> m a -> m a
waitUntil' = wait' handler
  where
    handler retry = (`catches` [Handler handleFailedCommand
                               ,Handler handleExpectFailed]
                    )
      where
        handleFailedCommand (FailedCommand NoSuchElement _) = retry
        handleFailedCommand err = throwIO err
                              
        handleExpectFailed (_ :: ExpectFailed) = retry

-- |Like 'waitUntil', but retries the action until it fails or until the timeout
-- is exceeded.
waitWhile :: SessionState m => Double -> m a -> m ()
waitWhile = waitWhile' 500000

-- |Like 'waitUntil'', but retries the action until it either fails or 
-- until the timeout is exceeded.
waitWhile' :: SessionState m => Int -> Double -> m a -> m ()
waitWhile' = wait' handler
  where
    handler retry wd = do 
      b <- (wd >> return True) `catches` [Handler handleFailedCommand
                                         ,Handler handleExpectFailed
                                         ]
      when b retry
      where
        handleFailedCommand (FailedCommand NoSuchElement _) = return False
        handleFailedCommand err = throwIO err
                               
        handleExpectFailed (_ :: ExpectFailed) = return False
    
wait' :: SessionState m => 
         (m b -> m a -> m b) -> Int -> Double -> m a -> m b
wait' handler waitAmnt t wd = waitLoop =<< liftBase getCurrentTime
  where timeout = realToFrac t
        waitLoop startTime = handler retry wd
          where 
            retry = do
              now <- liftBase getCurrentTime
              if diffUTCTime now startTime >= timeout
                then 
                  failedCommand Timeout "wait': explicit wait timed out."
                else do 
                  liftBase . threadDelay $ waitAmnt
                  waitLoop startTime

-- |Convenience function to catch 'FailedCommand' 'Timeout' exceptions 
-- and perform some action.
--
-- Example:
--
-- > waitUntil 5 (getText <=< findElem $ ByCSS ".class")
-- >    `onTimeout` return ""
onTimeout :: MonadBaseControl IO m => m a -> m a -> m a
onTimeout m r = m `catch` handler
  where
    handler (FailedCommand Timeout _) = r
    handler other = throwIO other
