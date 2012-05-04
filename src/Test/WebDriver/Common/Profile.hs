{-# LANGUAGE CPP, TypeSynonymInstances, DeriveDataTypeable, FlexibleInstances, 
             GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# OPTIONS_HADDOCK not-home #-}
-- |A type for profile preferences. These preference values are used by both 
-- Firefox and Opera profiles.
module Test.WebDriver.Common.Profile
       ( Profile(..), PreparedProfile(..), ProfilePref(..), ToPref(..)
       , getPref, addPref, deletePref, addExtension, deleteExtension
       , ProfileParseError(..) ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.Number (Number(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Text (Text, pack)
import Data.ByteString (ByteString)

import Data.Fixed
import Data.Ratio
import Data.Int
import Data.Word

import Data.Typeable
import Control.Exception
import Control.Applicative

-- |This structure allows you to construct and manipulate profiles in pure code,
-- deferring execution of IO operations until the profile is \"prepared\". This 
-- type is shared by both Firefox and Opera profile code; when a distinction 
-- must be made, the phantom type parameter is used to differentiate.
data Profile b = Profile 
                 { -- |Location of the profile in the local file system
                   profileDir    :: FilePath
                   -- |A set of filepaths pointing to browser extensions.
                 , profileExts   :: HS.HashSet FilePath
                   -- |A map of Firefox preferences. These are the settings
                   -- found in the profile's prefs.js, and entries found in
                   -- about:config
                 , profilePrefs  :: HM.HashMap Text ProfilePref
                 }
               deriving (Eq, Show)

-- |Represents a profile that has been prepared for 
-- network transmission. The profile cannot be modified in this form.
newtype PreparedProfile b = PreparedProfile ByteString
  deriving (Eq, Show)

instance FromJSON (PreparedProfile s) where
  parseJSON (Object o) = PreparedProfile <$> o .: "zip"
  parseJSON other = typeMismatch "PreparedProfile" other
  
instance ToJSON (PreparedProfile s) where
  toJSON s = object ["zip" .= s]

-- |A profile preference value. This is the subset of JSON values that excludes
-- arrays, objects, and null.
data ProfilePref = PrefInteger !Integer
                 | PrefDouble  !Double
                 | PrefString  !Text
                 | PrefBool    !Bool
                 deriving (Eq, Show)

instance ToJSON ProfilePref where
  toJSON v = case v of
    PrefInteger i -> toJSON i
    PrefDouble d  -> toJSON d
    PrefString s  -> toJSON s
    PrefBool  b   -> toJSON b

instance FromJSON ProfilePref where
  parseJSON (String s) = return $ PrefString s
  parseJSON (Bool b) = return $ PrefBool b
  parseJSON (Number (I i)) = return $ PrefInteger i
  parseJSON (Number (D d)) = return $ PrefDouble d
  parseJSON other = typeMismatch "ProfilePref" other

instance Exception ProfileParseError
-- |An error occured while attempting to parse a profile's preference file.
newtype ProfileParseError = ProfileParseError String
                          deriving  (Eq, Show, Read, Typeable)

-- |A typeclass to convert types to profile preference values
class ToPref a where
  toPref :: a -> ProfilePref

instance ToPref Text where
  toPref = PrefString
  
instance ToPref String where
  toPref = toPref . pack

instance ToPref Bool where
  toPref = PrefBool
  
instance ToPref Integer where
  toPref = PrefInteger

#define I(t) instance ToPref t where toPref = PrefInteger . toInteger

I(Int)
I(Int8)
I(Int16)
I(Int32)
I(Int64)
I(Word)
I(Word8)
I(Word16)
I(Word32)
I(Word64)

instance ToPref Double where
  toPref = PrefDouble

instance ToPref Float where
  toPref = PrefDouble . realToFrac
  
instance (Integral a) => ToPref (Ratio a) where
  toPref = PrefDouble . realToFrac
  
instance (HasResolution r) => ToPref (Fixed r) where
  toPref = PrefDouble . realToFrac


-- |Retrieve a preference from a profile by key name.
getPref :: Text -> Profile b -> Maybe ProfilePref
getPref k (Profile _ _ m) = HM.lookup k m

-- |Add a new preference entry to a profile, overwriting any existing entry
-- with the same key.
addPref :: ToPref a => Text -> a -> Profile b -> Profile b
addPref k v p = asMap p $ HM.insert k (toPref v)

-- |Delete an existing preference entry from a profile. This operation is
-- silent if the preference wasn't found.
deletePref :: Text -> Profile b -> Profile b
deletePref k p = asMap p $ HM.delete k

-- |Add a new extension to the profile. The file path should refer to
-- an .xpi file or an extension directory. This operation has no effect if
-- the same extension has already been added to this profile.
addExtension :: FilePath -> Profile b -> Profile b
addExtension path p = asSet p $ HS.insert path

-- |Delete an existing extension from the profile. The file path should refer
-- to an .xpi file or an extension directory. This operation has no effect if
-- the extension was never added to the profile.
deleteExtension :: FilePath -> Profile b -> Profile b
deleteExtension path p = asSet p $ HS.delete path

asMap :: Profile b
         -> (HM.HashMap Text ProfilePref -> HM.HashMap Text ProfilePref)
         -> Profile b
asMap (Profile p hs hm) f = Profile p hs (f hm)

asSet :: Profile b
         -> (HS.HashSet FilePath -> HS.HashSet FilePath)
         -> Profile b
asSet (Profile p hs hm) f = Profile p (f hs) hm
