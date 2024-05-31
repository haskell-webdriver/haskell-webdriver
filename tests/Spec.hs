{-# OPTIONS_GHC -F -pgmF sandwich-discover #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Spec where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import qualified Data.Aeson as A
import Data.String.Interpolate
import qualified Data.Text as T
import System.Environment
import Test.Sandwich hiding (BrowserToUse(..))
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.WebDriver
import TestLib.Types
import TestLib.WebDriverContext
import UnliftIO.Process

#insert_test_imports


spec :: TopSpec
spec = describe "Selenium tests" $ do
  $(getSpecFromFolder defaultGetSpecFromFolderOptions)

-- spec :: SpecWithWebDirver
-- spec = do
--   tests

--   it "tests async" $ do
--     WebDriver {..} <- getContext webdriver
--     chromeBinaryPath <- askFile @"google-chrome-stable"

--     let (w, h) = (1920, 1080) :: (Int, Int)
--     let config = defaultConfig {
--           wdHost = webDriverHostname
--           , wdPort = fromIntegral webDriverPort
--           , wdCapabilities = defaultCaps {
--               browser = (chrome {
--                             chromeBinary = Just chromeBinaryPath
--                             -- , chromeOptions = "--headless=new":[i|--window-size=#{w},#{h}|]:(chromeOptions chrome)
--                             })
--               }
--           }

--     withRunInIO $ \runInIO -> do
--       liftIO $ runSession config . finallyClose $ do
--         liftIO $ runInIO $ info "Got here 1"
--         openPage "http://www.wikipedia.org/"
--         liftIO $ runInIO $ info "Got here 2"
--         r <- asyncJS [] "arguments[0]();"
--         liftIO $ runInIO $ info "Got here 3"
--         r `shouldBe` (Just A.Null)

--   -- it "takes a screenshot" $ liftIO $ do
--   --   openPage [i|https://www.google.com|]
--   --   search <- findElem (ByCSS [i|*[title="Search"]|])
--   --   click search
--   --   sendKeys "Haskell Sandwich" search
--   --   findElem (ByCSS [i|input[type="submit"]|]) >>= click

--   --   Just dir <- getCurrentFolder
--   --   screenshot >>= liftIO . BL.writeFile (dir </> "screenshot.png")

--   --   liftIO $ threadDelay 3000000
