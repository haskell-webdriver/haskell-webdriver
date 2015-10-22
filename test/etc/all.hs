module Main where

import qualified Async
import qualified QuickCheck
import qualified SearchBaidu

main :: IO () 
main = do
    Async.main
    QuickCheck.main
    SearchBaidu.main
