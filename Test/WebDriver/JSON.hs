{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.JSON where

import Test.WebDriver.Types

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Attoparsec.ByteString.Lazy (Result(..))
import qualified Data.Attoparsec.ByteString.Lazy as AP

import Control.Applicative
import Control.Exception.Lifted
import Data.String

-- JSON object constructors

single :: ToJSON a => Text -> a -> Value
single a x = object [a .= x]

pair :: (ToJSON a, ToJSON b) => (Text,Text) -> (a,b) -> Value
pair (a,b) (x,y) = object [a .= x, b .= y]

triple :: (ToJSON a, ToJSON b, ToJSON c) => 
          (Text,Text,Text) -> (a,b,c) -> Value
triple (a,b,c) (x,y,z) = object [a .= x, b.= y, c .= z]

-- conversion to/from JSON within WD

parseJSON' :: FromJSON a => ByteString -> WD a
parseJSON' = apResultToWD . AP.parse json

fromJSON' :: FromJSON a => Value -> WD a
fromJSON' = aesonResultToWD . fromJSON


-- JSON object key accessor

(!:) :: FromJSON a => Object -> Text -> WD a
o !: k = aesonResultToWD $ parse (.: k) o


-- extracting JSON objects into tuples

parsePair :: (FromJSON a, FromJSON b) => 
             String -> String -> String -> Value -> WD (a, b)
parsePair a b funcName v = 
  case v of
    Object o -> (,) <$> o !: fromString a <*> o !: fromString b
    _        -> throwIO . BadJSON $ funcName ++ 
                ": cannot parse non-object JSON response as a (" ++ a  
                ++ ", " ++ b ++ ") pair" ++ ")"

parseTriple :: (FromJSON a, FromJSON b, FromJSON c) =>
               String -> String -> String ->  String -> Value -> WD (a, b, c)
parseTriple a b c funcName v = 
  case v of
    Object o -> (,,) <$> o !: fromString a 
                     <*> o !: fromString b 
                     <*> o !: fromString c
    _        -> throwIO . BadJSON $ funcName ++
                ": cannot parse non-object JSON response as a (" ++ a
                ++ ", " ++ b ++ ", " ++ c ++ ") pair"


-- conversion functions between parser result types and WD

apResultToWD :: FromJSON a => AP.Result Value -> WD a
apResultToWD p = case p of
  Done _ res -> fromJSON' res
  Fail _ _ err -> throwIO $ BadJSON err

aesonResultToWD r = case r of
  Success val -> return val
  Error err   -> throwIO $ BadJSON err
