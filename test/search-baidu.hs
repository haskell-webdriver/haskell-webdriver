{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Parallel
import           Control.Monad (void)
import           Data.List
import qualified Data.Text                    as T
import           Test.WebDriver
--import           Test.WebDriver.Classes
import           Test.WebDriver.Commands.Wait

capsChrome = defaultCaps { browser = chrome }
capsFF = defaultCaps

-- Have no fun with baidu but only cause it is loading fast than google in China.
--
baidu :: WD ()
baidu = openPage "http://www.baidu.com/"

searchBaidu :: WD Bool
searchBaidu = do
  searchBox <- findElem (ById "kw1")
  sendKeys "Cheese!" searchBox
  submit searchBox
  setImplicitWait 50
  waitUntil 2000 $ do
    title <- getTitle
    return ("cheese!" `T.isSuffixOf` title)

testCase c = void $ runSession (defaultConfig { wdCapabilities = c }) (baidu >> searchBaidu)

testSuits = mapM_ testCase  [capsFF, capsChrome]

main = do
  --testCase capsChrome `seq` testCase capsFF
  --mapM_ (forkOS . testCase) [capsFF, capsChrome]
  testSuits
  print "done"
