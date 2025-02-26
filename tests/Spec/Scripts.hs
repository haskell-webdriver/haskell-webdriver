{-# LANGUAGE NumericUnderscores #-}

module Spec.Scripts where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.String.Interpolate
import Test.Sandwich
import Test.WebDriver
import Test.WebDriver.Commands.Wait
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Scripts" $ before "Open test page" openSimpleTestPage $ do
  it "Does synchronous script eval" $ do
    -- liftIO $ threadDelay 99999999999

    -- liftIO $ threadDelay 5_000_000

    waitUntil 30 $ do
      title :: String <- executeJS [] [i|return document.title;|]
      info [i|Got title: #{title}|]
      title `shouldBe` "Test page"

  it "Does asynchronous script eval" $ do
    -- liftIO $ threadDelay 99999999999

    -- liftIO $ threadDelay 5_000_000

    waitUntil 30 $ do
      title :: Maybe String <- asyncJS [] [i|arguments[0](document.title);|]
      info [i|Got title: #{title}|]
      title `shouldBe` Just "Test page"

  -- it "Tries just 2 + 2" $ do
  --   -- liftIO $ threadDelay 99999999999

  --   -- liftIO $ threadDelay 5_000_000

  --   waitUntil 30 $ do
  --     result :: Int <- executeJS [] [i|return (2 + 2);|]
  --     info [i|Got result: #{result}|]
  --     result `shouldBe` 4
