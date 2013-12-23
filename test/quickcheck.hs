{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -fwarn-unused-imports #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Data.List as List
import Data.String.Conversions
import Prelude hiding ((++))
import Test.QuickCheck as QC
import Test.QuickCheck.Property as QC
import Test.WebDriver

import qualified Data.Text as ST
import qualified Data.Array as AR

session :: WDSession
session = defaultSession

main :: IO ()
main = do
    session :: WDSession <- runWD session (createSession defaultCaps)
    runWD session $ openPage "about:blank"
    quickCheck $ prop_cutArray session


-- | some javascript function i want to test (included in this haskell
-- module for convenience; usually it would more likely be located in
-- the test target).
cutArrayJS :: [ST]
cutArrayJS = "// Array Remove - By John Resig (MIT Licensed)" :
             "function cutArray(a, from, to) {" :
             "    var rest = a.slice((to || from) + 1 || a.length);" :
             "    a.length = from < 0 ? a.length + from : from;" :
             "    a.push.apply(a, rest);" :
             "    return a;" :
             "};" :
             []


-- | reference implementation in haskell
cutArray :: CutArray -> [Int]
cutArray (CutArray i a _)         | abs(a) > (length i - 1)  = error "cutArray: a out of bounds!"
cutArray (CutArray i _ (Just b))  | abs(b) > (length i - 1)  = error "cutArray: b out of bounds!"
cutArray (CutArray i a (Just b))  | a * b < 0                = error ("cutArray: a and b must have same sign! " ++ show (a, b))
cutArray (CutArray i a (Just b))  | abs(b) < abs(a)          = error ("cutArray: negative range! " ++ show (a, b))
cutArray (CutArray i a b) = f i a b
  where
    f i a Nothing = f i a (Just a)
    f i a (Just b) = if a >= 0
                       then take a i ++ drop (b + 1) i
                       else reverse (f (reverse i) (-b - 1) (Just (-a - 1)))


-- | data type with input for cutArray
data CutArray = CutArray [Int] Int (Maybe Int)
  deriving (Eq, Show)

_a_range ar = (1 - length ar, length ar - 1)
_b_range ar a = if a >= 0
    then (a, length ar - 1)
    else (1 - length ar, a)

-- | arbitrary *valid* inputs for 'CutArray'.
instance Arbitrary CutArray where
    arbitrary = do
        ar <- (:) <$> arbitrary <*> arbitrary
        let ar' = map (`mod` (1000 :: Int)) ar  -- (avoid int overflows in js)
        a  <- choose (_a_range ar)
        b  <- frequency [(20, pure Nothing), (80, Just <$> choose (_b_range ar a))]
        return $ CutArray ar' a b
    shrink (CutArray ar a b) = filter (\ (CutArray ar a b) ->
                                                AR.inRange (_a_range ar) a &&
                                                maybe True (AR.inRange (_b_range ar a)) b
                                         ) $
                                      CutArray <$> shrink ar <*> shrink a <*> shrink b


-- | the quickcheck property
prop_cutArray :: WDSession -> CutArray -> QC.Property
prop_cutArray session tc = morallyDubiousIOProperty $ runWD session $ wd_cutArray tc


-- | the underlying property in the web driver monad.
wd_cutArray :: CutArray -> WD Bool
wd_cutArray tc@(CutArray arrayIn a b) = do
    liftIO $ print tc
    let arrayOutShould :: [Int] = cutArray tc
    arrayOutActually :: [Int] <- callff arrayIn a b
    return $ arrayOutShould == arrayOutActually
  where
    callff :: [Int] -> Int -> Maybe Int -> WD [Int]
    callff arrayIn a b = executeJS [JSArg arrayIn, JSArg a, JSArg b] . ST.intercalate "\n" $
        "var arrayIn = arguments[0];" :
        "var a = arguments[1];" :
        "var b = arguments[2];" :
        "" :
        -- "console.log([arrayIn, a, b].toString());" :
        cutArrayJS ++
        "" :
        "return cutArray(arrayIn, a, b);" :
        []
