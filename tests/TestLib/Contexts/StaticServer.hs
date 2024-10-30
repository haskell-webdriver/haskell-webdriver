{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module TestLib.Contexts.StaticServer where

import Control.Monad
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import GHC.Stack
import Network.Socket (PortNumber)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Test.Sandwich hiding (BrowserToUse(..))
import UnliftIO.Async
import UnliftIO.Exception
import WaiAppStatic.Types


data StaticServerContext = StaticServerContext {
  staticServerHostname :: String
  , staticServerPort :: PortNumber
  }

staticServer :: Label "staticServer" StaticServerContext
staticServer = Label

type HasStaticServerContext context = HasLabel context "staticServer" StaticServerContext

type BaseMonad m = (HasCallStack, MonadUnliftIO m)
type BaseMonadContext m context = (BaseMonad m, HasBaseContext context)


introduceStaticServer :: forall context m. (
  BaseMonadContext m context, MonadCatch m
  )
  => SpecFree (LabelValue "staticServer" StaticServerContext :> context) m () -> SpecFree context m ()
introduceStaticServer = introduceWith "Introduce static server" staticServer withAlloc
  where
    withAlloc action = bracket
      (async $ liftIO $ runSettings serverConf (staticApp serverStaticConf))
      (\_ -> void $ action (StaticServerContext "localhost" undefined))
      cancel

    serverConf = undefined
    serverStaticConf = undefined

    -- serverConf = setPort Config.serverPort
    --            $ defaultSettings

    -- serverStaticConf = defaultFileServerSettings staticContentPath
