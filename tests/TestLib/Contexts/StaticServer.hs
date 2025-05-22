{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TestLib.Contexts.StaticServer (
  introduceStaticServer

  , staticServer
  , HasStaticServerContext
  , StaticServerContext(..)

  , openStaticServerUrl
  , openSimpleTestPage
  ) where

import Control.Monad
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Function
import Data.String.Interpolate
import GHC.Stack
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import System.FilePath
import Test.Sandwich hiding (BrowserToUse(..))
import Test.Sandwich.Contexts.Util.Ports
import Test.WebDriver.Commands
import Test.WebDriver.Types
import TestLib.Types
import UnliftIO.Async
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.MVar


introduceStaticServer :: forall context m. (
  HasCallStack, MonadUnliftIO m, MonadCatch m
  )
  => SpecFree (LabelValue "staticServer" StaticServerContext :> context) m ()
  -> SpecFree context m ()
introduceStaticServer = introduceWith "Introduce static server" staticServer withAlloc
  where
    withAlloc action = do
      staticDir <- getCurrentDirectory >>= findGitRoot >>= \case
        Just dir -> pure (dir </> "test-static")
        Nothing -> expectationFailure [i|Couldn't find test-static directory|]
      info [i|Got static dir: #{staticDir}|]
      let serverStaticConf = defaultFileServerSettings staticDir

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
        cancel
        (\_ -> void $ action (StaticServerContext "localhost" port))

findGitRoot :: MonadIO m => FilePath -> m (Maybe FilePath)
findGitRoot dir = do
  let gitDir = dir </> ".git"
  doesDirectoryExist gitDir >>= \case
    True -> return (Just dir)
    False -> do
      let parent = takeDirectory dir
      if parent == dir
        then return Nothing
        else findGitRoot parent

openStaticServerUrl :: (
  MonadReader context m, HasStaticServerContext context, WebDriver m
  ) => FilePath -> m ()
openStaticServerUrl url = do
  StaticServerContext {..} <- getContext staticServer
  openPage [i|http://#{staticServerHostname}:#{staticServerPort}#{url}|]

openSimpleTestPage :: (
  MonadReader context m, HasStaticServerContext context, WebDriver m
  ) => m ()
openSimpleTestPage = openStaticServerUrl "/test.html"
