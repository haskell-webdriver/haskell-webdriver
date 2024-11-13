
module TestLib.Waits where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Retry
import Data.String.Interpolate
import Network.Socket
import UnliftIO.Timeout



-- * Socket waits

-- | Each individual connect attempt needs a timeout to prevent it from hanging
-- indefinitely. This policy allows us to make that timeout length adaptive,
-- based on the 'RetryStatus' of the outer retry policy.
--
-- Thus, the first attempt to connect will have a short timeout (currently 100ms),
-- and then successive attempts will get longer timeouts via "FullJitter" backoff.
-- The goals of this are twofold:
--
-- 1) If a connect call hangs during the first few attempts, it is timed out quickly
-- and re-attempted, so on a healthy network you aren't penalized too much by the hang.
-- The outer retry policy can control the time between attempts, so the user can set
-- it high enough to make this be the case.
--
-- 2) If the network is slow, we will eventually reach the maximum timeout of 3 seconds,
-- which should be long enough. Note that the popular wait-for script uses 1 second
-- timeouts, so this is extra conservative:
-- https://github.com/eficode/wait-for/blob/7586b3622f010808bb2027c19aaf367221b4ad54/wait-for#L72
connectRetryPolicy :: MonadIO m => RetryPolicyM m
connectRetryPolicy = capDelay (3000000) (fullJitterBackoff 100000)

waitForSocket :: (
  MonadUnliftIO m, MonadLogger m, MonadMask m
  )
  => RetryPolicyM m
  -> AddrInfo
  -> m ()
waitForSocket policy addr = recoverAll policy $ \retryStatus@(RetryStatus {..}) -> do
  flip withException (\(e :: SomeException) -> logErrorN [i|waitForSocket: failed to connect to #{addr}: #{e}|]) $ do
    when (rsIterNumber > 0) $
      logDebugN [i|waitForSocket: attempt \##{rsIterNumber} to connect to #{addr}|]

    bracket (liftIO initSocket) (liftIO . close) $ \sock -> do
      connectTimeoutUs <- (getRetryPolicyM connectRetryPolicy) retryStatus >>= \case
        Nothing -> throwIO $ userError "Timeout due to connect retry policy"
        Just us -> pure us

      timeout connectTimeoutUs (liftIO $ connect sock (addrAddress addr)) >>= \case
        Nothing -> throwIO $ userError "Timeout in connect attempt"
        Just () -> pure ()

  where
    initSocket = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
