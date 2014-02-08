{-# LANGUAGE CPP, DeriveDataTypeable, ScopedTypeVariables, FlexibleContexts #-}
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
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Exception.Lifted
import Control.Concurrent
import Data.Time.Clock
import Data.Typeable
import Control.Conditional (ifM, (<||>), (<&&>), notM)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif

instance Exception ExpectFailed
-- |An exception representing the failure of an expected condition.
data ExpectFailed = ExpectFailed String deriving (Show, Eq, Typeable)

-- |throws 'ExpectFailed'. This is nice for writing your own abstractions.
unexpected :: MonadBaseControl IO m =>
              String -- ^ Reason why the expected condition failed.
           -> m a
unexpected = throwIO . ExpectFailed

-- |An expected condition. This function allows you to express assertions in
-- your explicit wait. This function raises 'ExpectFailed' if the given
-- boolean is False, and otherwise does nothing.
expect :: MonadBaseControl IO m => Bool -> m ()
expect b
  | b         = return ()
  | otherwise = unexpected "Test.WebDriver.Commands.Wait.expect"

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
waitUntil' = waitEither id (\_ -> return)

-- |Like 'waitUntil', but retries the action until it fails or until the timeout
-- is exceeded.
waitWhile :: SessionState m => Double -> m a -> m ()
waitWhile = waitWhile' 500000

-- |Like 'waitUntil'', but retries the action until it either fails or
-- until the timeout is exceeded.
waitWhile' :: SessionState m => Int -> Double -> m a -> m ()
waitWhile' =
  waitEither  (\_ _ -> return ())
              (\retry _ -> retry "waitWhile: action did not fail")


-- |Internal function used to implement explicit wait commands using success and failure continuations
waitEither :: SessionState m =>
               ((String -> m b) -> String -> m b)
            -> ((String -> m b) -> a -> m b)
            -> Int -> Double -> m a -> m b
waitEither failure success = wait' handler
 where
  handler retry wd = do
    e <- fmap Right wd  `catches` [Handler handleFailedCommand
                                  ,Handler handleExpectFailed
                                  ]
    either (failure retry) (success retry) e
   where
    handleFailedCommand e@(FailedCommand NoSuchElement _) = return . Left . show $ e
    handleFailedCommand err = throwIO err

    handleExpectFailed (e :: ExpectFailed) = return . Left . show $ e

wait' :: SessionState m =>
         ((String -> m b) -> m a -> m b) -> Int -> Double -> m a -> m b
wait' handler waitAmnt t wd = waitLoop =<< liftBase getCurrentTime
  where 
    timeout = realToFrac t
    waitLoop startTime = handler retry wd
      where
        retry why = do
          now <- liftBase getCurrentTime
          if diffUTCTime now startTime >= timeout 
            then 
              failedCommand Timeout $ "wait': explicit wait timed out (" ++ why ++ ")."
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
