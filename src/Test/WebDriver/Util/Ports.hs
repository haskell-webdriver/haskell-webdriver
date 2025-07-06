{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

{-|
Helper module for finding free ports, with various options for port ranges, retries, and excluded ports.
-}

module Test.WebDriver.Util.Ports (
  findFreePort

  -- * Exception-throwing versions
  , findFreePortOrException
  , findFreePortOrException'

  -- * Lower-level
  , findFreePortInRange
  , findFreePortInRange'

  -- * Lower-level
  , isPortFree
  , tryOpenAndClosePort
  , ephemeralPortRange
  ) where

import Control.Monad.Catch (MonadCatch, catch)
import Control.Monad.IO.Class
import Control.Retry
import Data.Maybe
import Network.Socket
import System.Random (randomRIO)
import UnliftIO.Exception (SomeException)


-- | Find an unused port in the ephemeral port range.
-- See https://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers.
findFreePort :: (MonadIO m, MonadCatch m) => m (Maybe PortNumber)
findFreePort = findFreePortInRange ephemeralPortRange []

-- | Find a free port in the ephemeral range, throwing an exception if one isn't found.
findFreePortOrException :: (MonadIO m, MonadCatch m) => m PortNumber
findFreePortOrException = findFreePortOrException' (const True)

-- | Same as 'findFreePortOrException', but with a callback to test if the port is acceptable or not.
findFreePortOrException' :: (MonadIO m, MonadCatch m) => (PortNumber -> Bool) -> m PortNumber
findFreePortOrException' isAcceptable = findFreePort >>= \case
  Just port
    | isAcceptable port -> return port
    | otherwise -> findFreePortOrException' isAcceptable
  Nothing -> error "Couldn't find free port"

-- | Find an unused port in a given range, excluding certain ports.
-- If the retries time out, returns 'Nothing'.
findFreePortInRange :: (
  MonadIO m, MonadCatch m
  )
  -- | Candidate port range
  => (PortNumber, PortNumber)
  -- | Ports to exclude
  -> [PortNumber]
  -> m (Maybe PortNumber)
findFreePortInRange = findFreePortInRange' (limitRetries 50)

-- | Same as 'findFreePortInRange', but with a configurable retry policy.
findFreePortInRange' :: forall m. (
  MonadIO m, MonadCatch m
  )
  -- | Retry policy
  => RetryPolicy
  -- | Candidate port range
  -> (PortNumber, PortNumber)
  -- | Ports to exclude
  -> [PortNumber]
  -> m (Maybe PortNumber)
findFreePortInRange' retryPolicy (start, end) blacklist = retrying retryPolicy callback (const findFreePortInRange')
  where
    callback _retryStatus result = return $ isNothing result

    getAcceptableCandidate :: m PortNumber
    getAcceptableCandidate = do
      candidate <- liftIO (fromInteger <$> randomRIO (fromIntegral start, fromIntegral end))
      if | candidate `elem` blacklist -> getAcceptableCandidate
         | otherwise -> return candidate

    findFreePortInRange' :: m (Maybe PortNumber)
    findFreePortInRange' = do
      candidate <- getAcceptableCandidate
      isPortFree candidate >>= \case
        False -> return Nothing
        True -> return $ Just candidate

-- | Test if a given 'PortNumber' is currently available.
isPortFree :: (MonadIO m, MonadCatch m) => PortNumber -> m Bool
isPortFree candidate = catch (tryOpenAndClosePort candidate >> return True)
                             (\(_ :: SomeException) -> return False)

-- | Test a given 'PortNumber' availability by trying to open and close a socket on it.
-- Throws an exception on failure.
tryOpenAndClosePort :: MonadIO m => PortNumber -> m PortNumber
tryOpenAndClosePort port = liftIO $ do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  let hints = defaultHints { addrSocketType = Stream, addrFamily = AF_INET }
  getAddrInfo (Just hints) (Just "127.0.0.1") (Just $ show port) >>= \case
    ((AddrInfo {addrAddress=addr}):_) -> do
      bind sock addr
      close sock
      return $ fromIntegral port
    [] -> error "Couldn't resolve address 127.0.0.1"

-- | The ephemeral port range.
-- See https://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers.
ephemeralPortRange :: (PortNumber, PortNumber)
ephemeralPortRange = (49152, 65535)
