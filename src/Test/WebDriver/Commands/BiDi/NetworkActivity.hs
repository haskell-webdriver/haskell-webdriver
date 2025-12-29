
module Test.WebDriver.Commands.BiDi.NetworkActivity where

import Control.Monad.IO.Unlift
import Control.Monad.Logger (MonadLogger, logDebugN)
import Data.Map (Map)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Network.URI as URI
import Test.WebDriver.Commands.BiDi.Session
import Test.WebDriver.Types
import UnliftIO.STM


networkEvents :: [Text]
networkEvents = [
  "network.beforeRequestSent"
  , "network.responseCompleted"
  , "network.responseStarted"
  , "network.fetchError"
  ]

type RequestId = Text

data RequestInfo = RequestInfo -- TODO

type NetworkActivityVar = TVar (Map RequestId RequestInfo)

-- | Wrapper around 'withRecordLogsViaBiDi'' which uses the WebSocket URL from
-- the current 'Session'. You must make sure to pass '_capabilitiesWebSocketUrl'
-- = @Just True@ to enable this. This will not work with Selenium 3.
withRecordNetworkActivityViaBiDi :: (WebDriver m, MonadLogger m) => NetworkActivityVar -> m a -> m a
withRecordNetworkActivityViaBiDi networkActivityVar action = do
  withBiDiSession networkEvents (mkCallback networkActivityVar) action

-- | Connect to WebSocket URL and subscribe to log events using the W3C BiDi protocol; see
-- <https://w3c.github.io/webdriver-bidi/>.
withRecordNetworkActivityViaBiDi' :: forall m a. (MonadUnliftIO m, MonadLogger m) => Int -> URI.URI -> NetworkActivityVar -> m a -> m a
withRecordNetworkActivityViaBiDi' bidiSessionId uri networkActivityVar action =
  withBiDiSession' bidiSessionId uri networkEvents (mkCallback networkActivityVar) action

mkCallback :: (MonadLogger m) => NetworkActivityVar -> BiDiEvent -> m ()
mkCallback nav (BiDiEvent "event" "network.beforeRequestSent" params) = undefined
mkCallback nav (BiDiEvent "event" "network.responseCompleted" params) = undefined
mkCallback nav (BiDiEvent "event" "network.responseStarted" params) = undefined
mkCallback nav (BiDiEvent "event" "network.fetchError" params) = undefined
mkCallback _nav x = logDebugN [i|BiDi: Ignoring event: #{x}|]
