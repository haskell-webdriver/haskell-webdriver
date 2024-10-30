{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TestLib.Contexts.StaticServer where

import Control.Monad
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Function
import Data.String.Interpolate
import GHC.Stack
import Network.Socket (PortNumber)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Test.Sandwich hiding (BrowserToUse(..))
import Test.Sandwich.Contexts.Util.Ports
import UnliftIO.Async
import UnliftIO.Exception
import UnliftIO.MVar


data StaticServerContext = StaticServerContext {
  staticServerHostname :: String
  , staticServerPort :: PortNumber
  }

staticServer :: Label "staticServer" StaticServerContext
staticServer = Label

type HasStaticServerContext context = HasLabel context "staticServer" StaticServerContext

introduceStaticServer :: forall context m. (
  HasCallStack, MonadUnliftIO m, MonadCatch m
  )
  => SpecFree (LabelValue "staticServer" StaticServerContext :> context) m ()
  -> SpecFree context m ()
introduceStaticServer = introduceWith "Introduce static server" staticServer withAlloc
  where
    withAlloc action = do
      port <- findFreePortOrException

      baton <- newEmptyMVar

      let serverConf = defaultSettings
                     & setPort (fromIntegral port)
                     & setBeforeMainLoop (putMVar baton ())

      bracket
        (do
            asy <- async $ liftIO $ runSettings serverConf (staticApp serverStaticConf)
            takeMVar baton
            info [i|Started static server on http://localhost:#{port}|]
            return asy
        )
        (\_ -> void $ action (StaticServerContext "localhost" port))
        cancel

    serverStaticConf = defaultFileServerSettings "test-static"
