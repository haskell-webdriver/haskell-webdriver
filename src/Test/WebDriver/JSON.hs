{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-deriving-typeable #-}

-- | A collection of convenience functions for using and parsing JSON values
-- within 'WD'. All monadic parse errors are converted to asynchronous
-- 'BadJSON' exceptions.
--
-- These functions are used internally to implement webdriver commands, and may
-- be useful for implementing non-standard commands.
module Test.WebDriver.JSON (
  -- * Access a JSON object key
  (!:)
  , (.:??)

  -- * Conversion from JSON within WD
  -- | Apostrophes are used to disambiguate these functions
  -- from their "Data.Aeson" counterparts.
  , parseJSON'
  , fromJSON'

  -- * Tuple functions
  -- | Convenience functions for working with tuples.
   -- ** JSON object constructors
  , single
  , pair
  , triple
  -- ** Extracting JSON objects into tuples
  , parseTriple

  -- * Conversion from parser results to WD
  -- | These functions are used to implement the other functions
  -- in this module, and could be used to implement other JSON
  -- convenience functions
  , apResultToWD
  , aesonResultToWD

  -- * Parse exception
  , BadJSON(..)

  -- * parsing commands with no return value
  , noReturn
  , ignoreReturn
  , aesonKeyFromText
  , NoReturn(..)

  -- * JSON helpers
  , noObject
  ) where

import Control.Applicative
import Control.Monad (join, void)
import Control.Monad.IO.Class
import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString.Lazy (Result(..))
import qualified Data.Attoparsec.ByteString.Lazy as AP
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.String
import Data.Text (Text)
import Prelude -- hides some "unused import" warnings
import Test.WebDriver.Types (WebDriver)
import Test.WebDriver.Util.Aeson (aesonKeyFromText)
import UnliftIO.Exception

#if MIN_VERSION_aeson(2,2,0)
-- This comes from the attoparsec-aeson package
import Data.Aeson.Parser (json)
#endif

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap          as HM
#else
import qualified Data.HashMap.Strict        as HM
#endif


-- | An error occured when parsing a JSON value.
newtype BadJSON = BadJSON String
  deriving (Eq, Show, Typeable)
instance Exception BadJSON

-- | A type indicating that we expect no return value from the webdriver request.
-- Its FromJSON instance parses successfully for any values that indicate lack of
-- a return value (a notion that varies from server to server).
data NoReturn = NoReturn

instance FromJSON NoReturn where
  parseJSON Null                    = return NoReturn
  parseJSON (Object o) | HM.null o  = return NoReturn
  parseJSON (String "")             = return NoReturn
  parseJSON other                   = typeMismatch "no return value" other

instance ToJSON NoReturn where
  toJSON NoReturn = Aeson.String "<no return>"

-- | Convenience function to handle webdriver commands with no return value.
noReturn :: WebDriver m => m NoReturn -> m ()
noReturn = void

-- | Convenience function to ignore result of a webdriver command.
ignoreReturn :: WebDriver m => m Value -> m ()
ignoreReturn = void

-- | Construct a singleton JSON 'object' from a key and value.
single :: ToJSON a => Text -> a -> Value
single a x = object [aesonKeyFromText a .= x]

-- | Construct a 2-element JSON 'object' from a pair of keys and a pair of
-- values.
pair :: (ToJSON a, ToJSON b) => (Text, Text) -> (a, b) -> Value
pair (a, b) (x,y) = object [aesonKeyFromText a .= x, aesonKeyFromText b .= y]

-- | Construct a 3-element JSON 'object' from a triple of keys and a triple of
-- values.
triple :: (ToJSON a, ToJSON b, ToJSON c) => (Text, Text, Text) -> (a, b, c) -> Value
triple (a, b, c) (x, y, z) = object [aesonKeyFromText a .= x, aesonKeyFromText b.= y, aesonKeyFromText c .= z]

-- | Parse a lazy 'ByteString' as a top-level JSON 'Value', then convert it to an
-- instance of 'FromJSON'..
parseJSON' :: MonadIO m => FromJSON a => ByteString -> m a
parseJSON' = apResultToWD . AP.parse json

-- | Convert a JSON 'Value' to an instance of 'FromJSON'.
fromJSON' :: MonadIO m => FromJSON a => Value -> m a
fromJSON' = aesonResultToWD . fromJSON

-- | This operator is a wrapper over Aeson's '.:' operator.
(!:) :: (MonadIO m, FromJSON a) => Object -> Text -> m a
o !: k = aesonResultToWD $ parse (.: aesonKeyFromText k) o

-- | Due to a breaking change in the '.:?' operator of aeson 0.10 (see <https://github.com/bos/aeson/issues/287>) that was subsequently reverted, this operator
-- was added to provide consistent behavior compatible with all aeson versions. If the field is either missing or `Null`, this operator should return a `Nothing` result.
(.:??) :: FromJSON a => Object -> Text -> Parser (Maybe a)
o .:?? k = fmap join (o .:? aesonKeyFromText k)

-- | Parse a JSON Object as a triple. The first three string arguments
-- specify the keys to extract from the object. The fourth string is the name
-- of the calling function, for better error reporting.
parseTriple :: (
  MonadIO m, FromJSON a, FromJSON b, FromJSON c
  ) => String -> String -> String ->  String -> Value -> m (a, b, c)
parseTriple a b c funcName v =
  case v of
    Object o -> (,,) <$> o !: fromString a
                     <*> o !: fromString b
                     <*> o !: fromString c
    _        -> throwIO . BadJSON $ funcName ++
                ": cannot parse non-object JSON response as a (" ++ a
                ++ ", " ++ b ++ ", " ++ c ++ ") pair"

-- | Convert an attoparsec parser result to 'WD'.
apResultToWD :: (MonadIO m, FromJSON a) => AP.Result Value -> m a
apResultToWD p = case p of
  Done _ res -> fromJSON' res
  Fail _ _ err -> throwIO $ BadJSON err

-- |  Convert an Aeson parser result to 'WD'.
aesonResultToWD :: (MonadIO m) => Aeson.Result a -> m a
aesonResultToWD r = case r of
  Success val -> return val
  Error err -> throwIO $ BadJSON err

-- Selenium 3.x doesn't seem to like receiving Null for click parameter
noObject :: Value
noObject = Object mempty
