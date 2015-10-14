{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (void)
import           Data.List
import qualified Data.Text                    as T
import           Test.WebDriver
import           Test.WebDriver.Commands.Wait

chromeConf = useBrowser chrome defaultConfig
ffConf = defaultConfig

-- Have no fun with baidu but only cause it is loading fast than google in China.
--
baidu :: WD ()
baidu = openPage "http://www.baidu.com/"

searchBaidu :: WD Bool
searchBaidu = do
  searchBox <- findElem (ById "kw")
  sendKeys "Cheese!" searchBox
  submit searchBox
  setImplicitWait 50
  waitUntil 2000 $ do
    title <- getTitle
    return ("cheese!" `T.isSuffixOf` title)

testCase c = void . runSession c . finallyClose $ (baidu >> searchBaidu)

testSuits = mapM_ testCase  [ffConf, chromeConf]

main = do
  --testCase capsChrome `seq` testCase capsFF
  --mapM_ (forkOS . testCase) [capsFF, capsChrome]
  testSuits
  print "done"
