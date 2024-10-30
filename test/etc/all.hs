module Main where

import qualified Async
import qualified SearchBaidu

main :: IO ()
main = do
    Async.main
    SearchBaidu.main
