{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           GHC.Conc           (numCapabilities)
import           System.Environment (getArgs)

{-
  TODO:
  try to run tests parallelly but doesn't work when using forkIO.
-}
main2 = do
  forkIO $ print "world"
  forkIO $ print "hello"
  print "done"

main = do
  args <- getArgs
  putStrLn $ "command line arguments: " ++ show args
  putStrLn $ "number of cores: " ++ show numCapabilities
