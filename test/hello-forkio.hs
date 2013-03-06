{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import GHC.Conc (numCapabilities)
import System.Environment (getArgs)

main2 = do
  forkIO $ print "world"
  forkIO $ print "hello"
  print "done"

main = do
  args <- getArgs
  putStrLn $ "command line arguments: " ++ show args
  putStrLn $ "number of cores: " ++ show numCapabilities
