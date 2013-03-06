{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.WebDriver
import Test.WebDriver.Classes
import Test.WebDriver.Commands.Wait
import Data.List
import qualified Data.Text as T
import Control.Concurrent
import Control.Parallel

capsChrome = defaultCaps { browser = chrome }
capsFF = defaultCaps

baidu :: WD ()
baidu = openPage "http://www.baidu.com/"

searchBaidu :: WD Bool
searchBaidu = do
  searchBox <- findElem (ByName "wd")
  sendKeys "Cheese!" searchBox
  submit searchBox
  setImplicitWait 50
  waitUntil 2000 $ do
    title <- getTitle
    return ("cheese!" `T.isSuffixOf` title)

testCase c = runSession defaultSession c (baidu >> searchBaidu)
             >> return ()
testSuits = mapM_ testCase  [capsFF, capsChrome]

main = do
  --testCase capsChrome `seq` testCase capsFF
  --mapM_ (forkOS . testCase) [capsFF, capsChrome]
  testSuits
  print "done"
