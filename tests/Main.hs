{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Data.String.Interpolate
import System.Environment
import Test.Sandwich hiding (BrowserToUse(..))
import Test.WebDriver
import TestLib.Types
import TestLib.WebDriverContext


spec :: TopSpecWithOptions' UserOptions
spec = do
  introduceWebDriver () $ do
    it "tests async" $ liftIO $ do
      runSession (defaultConfig { wdCapabilities = defaultCaps { browser = (chrome { chromeBinary = Just "/nix/store/nb4vdqyy0by2h75794aqhw409iccpvmq-google-chrome-125.0.6422.60/bin/google-chrome-stable" }) } }) . finallyClose $ do
        liftIO $ putStrLn "Got here 1"
        openPage "http://www.wikipedia.org/"
        liftIO $ putStrLn "Got here 2"
        r <- asyncJS [] "arguments[0]();"
        liftIO $ putStrLn "Got here 3"
        r `shouldBe` (Just A.Null)

main :: IO ()
main = do
  args <- getArgs
  putStrLn [i|args: #{args}|]

  clo <- parseCommandLineArgs userOptions spec
  putStrLn [i|clo: #{clo}|]

  runSandwichWithCommandLineArgs' defaultOptions userOptions spec


  -- introduceWebDriverOptions @() (defaultWdOptions "/tmp/tools") $ do
  --   it "opens Google and searches" $ withSession1 $ do
  --     openPage [i|https://www.google.com|]
  --     search <- findElem (ByCSS [i|*[title="Search"]|])
  --     click search
  --     sendKeys "Haskell Sandwich" search
  --     findElem (ByCSS [i|input[type="submit"]|]) >>= click

  --     Just dir <- getCurrentFolder
  --     screenshot >>= liftIO . BL.writeFile (dir </> "screenshot.png")

  --     liftIO $ threadDelay 3000000
