{-# LANGUAGE OverloadedStrings #-}
module Async where

import qualified Data.Aeson     as A
import           Test.WebDriver

main :: IO ()
main = runSession defaultConfig . finallyClose $ do
    openPage "http://www.wikipedia.org/"
    r <- asyncJS [] "arguments[0]();"
    Control.Monad.when (r /= Just A.Null) $ error $ "Async returned " ++ show r
