module Test.WebDriver.JSON.Internal where

import qualified Data.Char as C

lower1 :: String -> String
lower1 [] = []
lower1 (c:cs) = C.toLower c : cs
