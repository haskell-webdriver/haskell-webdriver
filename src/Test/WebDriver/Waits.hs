{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-deriving-typeable #-}

module Test.WebDriver.Waits (
  -- * Wait on expected conditions
  waitUntil
  , waitUntil'
  , waitWhile
  , waitWhile'

  -- * Expected conditions
  , ExpectFailed (..)
  , expect
  , unexpected
  , expectAny
  , expectAll
  , expectNotStale
  , expectAlertOpen
  , catchFailedCommand

  -- ** Convenience functions
  , onTimeout
  ) where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Stack
import Test.WebDriver.Commands
import Test.WebDriver.Exceptions
import Test.WebDriver.Types
import UnliftIO.Exception


instance Exception ExpectFailed
-- | An exception representing the failure of an expected condition.
data ExpectFailed = ExpectFailed String deriving (Show, Eq, Typeable)

-- | Throws 'ExpectFailed'. This is nice for writing your own abstractions.
unexpected :: (
  MonadIO m
  )
  => String -- ^ Reason why the expected condition failed.
  -> m a
unexpected = throwIO . ExpectFailed

-- |An expected condition. This function allows you to express assertions in
-- your explicit wait. This function raises 'ExpectFailed' if the given
-- boolean is False, and otherwise does nothing.
expect :: (MonadIO m) => Bool -> m ()
expect b
  | b = return ()
  | otherwise = unexpected "Test.WebDriver.Commands.Wait.expect"

-- |Apply a monadic predicate to every element in a list, and 'expect' that
-- at least one succeeds.
expectAny :: (F.Foldable f, MonadIO m) => (a -> m Bool) -> f a -> m ()
expectAny p xs = expect . F.or =<< mapM p (F.toList xs)

-- |Apply a monadic predicate to every element in a list, and 'expect' that all
-- succeed.
expectAll :: (F.Foldable f, MonadIO m) => (a -> m Bool) -> f a -> m ()
expectAll p xs = expect . F.and =<< mapM p (F.toList xs)

-- | 'expect' the given 'Element' to not be stale and returns it
expectNotStale :: (HasCallStack, WebDriver wd) => Element -> wd Element
expectNotStale e = catchFailedCommand StaleElementReference $ do
    _ <- isEnabled e -- Any command will force a staleness check
    return e

-- | 'expect' an alert to be present on the page, and returns its text.
expectAlertOpen :: (HasCallStack, WebDriver wd) => wd Text
expectAlertOpen = catchFailedCommand NoSuchAlert getAlertText

-- | Catches any `FailedCommand` exceptions with the given `FailedCommandType` and rethrows as 'ExpectFailed'
catchFailedCommand :: (MonadUnliftIO m) => FailedCommandError -> m a -> m a
catchFailedCommand needle m = m `catch` handler
  where
    handler e@(FailedCommand {rspError})
      | rspError == needle = unexpected . show $ e
    handler e = throwIO e

-- | Wait until either the given action succeeds or the timeout is reached.
-- The action will be retried every .5 seconds until no 'ExpectFailed' or
-- 'FailedCommand' 'NoSuchElement' exceptions occur. If the timeout is reached,
-- then a 'Timeout' exception will be raised. The timeout value
-- is expressed in seconds.
waitUntil :: (MonadUnliftIO m, HasCallStack) => Double -> m a -> m a
waitUntil = waitUntil' 500000

-- |Similar to 'waitUntil' but allows you to also specify the poll frequency
-- of the 'WD' action. The frequency is expressed as an integer in microseconds.
waitUntil' :: (MonadUnliftIO m, HasCallStack) => Int -> Double -> m a -> m a
waitUntil' = waitEither id (\_ -> return)

-- |Like 'waitUntil', but retries the action until it fails or until the timeout
-- is exceeded.
waitWhile :: (MonadUnliftIO m, HasCallStack)  => Double -> m a -> m ()
waitWhile = waitWhile' 500000

-- |Like 'waitUntil'', but retries the action until it either fails or
-- until the timeout is exceeded.
waitWhile' :: (MonadUnliftIO m, HasCallStack)  => Int -> Double -> m a -> m ()
waitWhile' =
  waitEither  (\_ _ -> return ())
              (\retry _ -> retry "waitWhile: action did not fail")


-- | Internal function used to implement explicit wait commands using success and failure continuations
waitEither :: (
  HasCallStack, MonadUnliftIO m
  )
  => ((String -> m b) -> String -> m b) -> ((String -> m b) -> a -> m b)
  -> Int
  -> Double
  -> m a
  -> m b
waitEither failure success = wait' handler
 where
  handler retry wd = do
    e <- fmap Right wd  `catches` [Handler handleFailedCommand
                                  , Handler handleExpectFailed
                                  ]
    either (failure retry) (success retry) e
   where
    handleFailedCommand e@(FailedCommand {rspError})
      | rspError == NoSuchElement = return . Left . show $ e
    handleFailedCommand err = throwIO err

    handleExpectFailed (e :: ExpectFailed) = return . Left . show $ e

wait' :: (
  HasCallStack, MonadIO m
  ) => ((String -> m b) -> m a -> m b) -> Int -> Double -> m a -> m b
wait' handler waitAmnt t wd = waitLoop =<< liftIO getCurrentTime
  where
    timeout = realToFrac t
    waitLoop startTime = handler retry wd
      where
        retry why = do
          now <- liftIO getCurrentTime
          if diffUTCTime now startTime >= timeout
            then
              throwIO $ FailedCommand {
                rspError = Timeout
                , rspMessage = "wait': explicit wait timed out (" <> T.pack why <> ")."
                , rspStacktrace = T.pack $ show (popCallStack callStack)
                , rspData = Nothing
                }
            else do
              liftIO . threadDelay $ waitAmnt
              waitLoop startTime

-- | Convenience function to catch 'FailedCommand' 'Timeout' exceptions
-- and perform some action.
--
-- Example:
--
-- > waitUntil 5 (getText <=< findElem $ ByCSS ".class")
-- >    `onTimeout` return ""
onTimeout :: (MonadUnliftIO m) => m a -> m a -> m a
onTimeout m r = m `catch` handler
  where
    handler (FailedCommand {rspError})
      | rspError `L.elem` [Timeout, ScriptTimeout] = r
    handler other = throwIO other
