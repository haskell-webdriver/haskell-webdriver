{-# LANGUAGE OverloadedStrings #-}
module SearchBaidu where

import Control.Monad
import Control.Applicative
import Data.List
import qualified Data.Text as T
import Test.WebDriver
import Test.WebDriver.Commands.Wait

import Prelude

chromeConf = useBrowser chrome defaultConfig
ffConf = defaultConfig

-- Have no fun with baidu but only cause it is loading fast than google in China.
--
baidu :: WD ()
baidu = do 
  openPage "http://www.baidu.com/"
  waitUntil 15 $
    expect . (== "http://www.baidu.com/") =<< getCurrentURL

searchBaidu :: WD ()
searchBaidu = do
  searchBox <- findElem (ById "kw")
  sendKeys "Cheese!" searchBox
  submit searchBox
  waitUntil 15 $ do
    title <- getTitle
    expect ("Cheese!" `T.isInfixOf` title)

  container <- findElem (ById "container")
  eList1 <- findElems (ByCSS "c-container")
  eList2 <- findElems (ByClass "c-container")
  expect =<< (fmap and $ zipWithM (==) eList1 eList2)

  forM_ eList1 $ \e -> findElemsFrom e (ByTag "a")



testCase c = void . runSession c . finallyClose $ (baidu >> searchBaidu)

testSuites = mapM_ testCase  [ffConf, chromeConf]

main = do
  --testCase capsChrome `seq` testCase capsFF
  --mapM_ (forkOS . testCase) [capsFF, capsChrome]
  testSuites
  print "done"
