{-# LANGUAGE OverloadedStrings #-}
-- |A collection of convenience functions for using and parsing JSON valus
-- within 'WD'. All monadic parse errors are converted to asynchronous 
-- 'BadJSON' exceptions.
module Test.WebDriver.JSON 
       ( -- * Access a JSON object key
         (!:)
         -- * Conversion from JSON within WD
         -- |Apostrophes are used to disambiguate these functions
         -- from their "Data.Aeson" counterparts.
       , parseJSON', fromJSON'
         -- * Tuple functions
         -- |Convenience functions for working with tuples.
         
         -- ** JSON object constructors
       , single, pair, triple
         -- ** Extracting JSON objects into tuples
       , parsePair, parseTriple 
         -- * Conversion between parser results and WD
         -- |These functions are used to implement the above functions,
         -- and could be used to implement your own JSON convenience functions
       , apResultToWD, aesonResultToWD 
       ) where
import Test.WebDriver.Types

import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Attoparsec.ByteString.Lazy (Result(..))
import qualified Data.Attoparsec.ByteString.Lazy as AP

import Control.Applicative
import Control.Exception.Lifted
import Data.String

-- |Construct a singleton JSON 'object' from a key and value.
single :: ToJSON a => Text -> a -> Value
single a x = object [a .= x]

-- |Construct a 2-element JSON 'object' from a pair of keys and a pair of 
-- values. 
pair :: (ToJSON a, ToJSON b) => (Text,Text) -> (a,b) -> Value
pair (a,b) (x,y) = object [a .= x, b .= y]

-- |Construct a 3-element JSON 'object' from a triple of keys and a triple of
-- values.
triple :: (ToJSON a, ToJSON b, ToJSON c) => 
          (Text,Text,Text) -> (a,b,c) -> Value
triple (a,b,c) (x,y,z) = object [a .= x, b.= y, c .= z]


-- |Parse a lazy 'ByteString' as a top-level JSON 'Value', then convert it to an
-- instance of 'FromJSON'..
parseJSON' :: FromJSON a => ByteString -> WD a
parseJSON' = apResultToWD . AP.parse json

-- |Convert a JSON 'Value' to an instance of 'FromJSON'.
fromJSON' :: FromJSON a => Value -> WD a
fromJSON' = aesonResultToWD . fromJSON



-- |This operator is a wrapper over Aeson's '.:' operator.
(!:) :: FromJSON a => Object -> Text -> WD a
o !: k = aesonResultToWD $ parse (.: k) o


-- |Parse a JSON 'Object' as a pair. The first two string arguments specify the
-- keys to extract from the object. The fourth string is the name of the
-- calling function, for better error reporting.
parsePair :: (FromJSON a, FromJSON b) => 
             String -> String -> String -> Value -> WD (a, b)
parsePair a b funcName v = 
  case v of
    Object o -> (,) <$> o !: fromString a <*> o !: fromString b
    _        -> throwIO . BadJSON $ funcName ++ 
                ": cannot parse non-object JSON response as a (" ++ a  
                ++ ", " ++ b ++ ") pair" ++ ")"


-- |Parse a JSON Object as a triple. The first three string arguments
-- specify the keys to extract from the object. The fourth string is the name 
-- of the calling function, for better error reporting.
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



-- |Convert an attoparsec parser result to 'WD'. 
apResultToWD :: FromJSON a => AP.Result Value -> WD a
apResultToWD p = case p of
  Done _ res -> fromJSON' res
  Fail _ _ err -> throwIO $ BadJSON err

-- |Convert an Aeson parser result to 'WD'.
aesonResultToWD :: Aeson.Result a -> WD a
aesonResultToWD r = case r of
  Success val -> return val
  Error err   -> throwIO $ BadJSON err
