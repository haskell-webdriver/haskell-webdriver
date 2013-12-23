{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fwarn-unused-imports #-}

module Main where

import Control.Applicative
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Data.List as List
import Data.Monoid
import Data.String.Conversions
import Prelude hiding ((++))
import System.Cmd
import Test.QuickCheck as QC
import Test.QuickCheck.Property as QC
import Test.WebDriver
import Test.WebDriver.Classes

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as ST
import qualified Data.Array as AR

session :: WDSession
session = defaultSession

main :: IO ()
main = quickCheck prop_pageOpen

data ValidUrl = Reddit | Stackoverflow | Bad
  deriving (Eq, Enum, Bounded)

instance Show ValidUrl where
  show Reddit = "http://reddit.com/"
  show Stackoverflow = "http://stackoverflow.com/"
  show Bad = "ht://.com/"

instance Arbitrary ValidUrl where
  arbitrary = elements [minBound..]
  shrink _ = []

prop_pageOpen :: ValidUrl -> QC.Property
prop_pageOpen url =
    morallyDubiousIOProperty $
        runSession session defaultCaps $
            wt_pageOpen url

wt_pageOpen :: ValidUrl -> WD Bool
wt_pageOpen url = catch (openPage (show url) >> return True)
                        (const $ return False :: SomeException -> WD Bool)
