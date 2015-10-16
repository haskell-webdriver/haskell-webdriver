module Main where

import qualified Test.Etc.Async as Async
import qualified Test.Etc.QuickCheck as QuickCheck
import qualified Test.Etc.SearchBaidu as SearchBaidu

main :: IO () 
main = do
	Async.main
	QuickCheck.main
	SearchBaidu.main
