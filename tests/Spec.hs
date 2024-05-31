{-# OPTIONS_GHC -F -pgmF sandwich-discover #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Spec where

import Test.Sandwich hiding (BrowserToUse(..))
import TestLib.Types

#insert_test_imports


spec :: SpecWithWebDriver
spec = describe "Selenium tests" $ do
  $(getSpecFromFolder defaultGetSpecFromFolderOptions)

-- spec :: SpecWithWebDriver
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
