{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Test.Sandwich
import Test.WebDriver


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions $ do
  it "tests async" $ liftIO $ do
    runSession defaultConfig . finallyClose $ do
      openPage "http://www.wikipedia.org/"
      r <- asyncJS [] "arguments[0]();"
      if r /= Just A.Null
        then error $ "Async returned " ++ show r
        else return ()


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
