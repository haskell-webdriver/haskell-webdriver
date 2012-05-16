{-# LANGUAGE CPP, TypeSynonymInstances, DeriveDataTypeable, FlexibleInstances, 
             GeneralizedNewtypeDeriving, OverloadedStrings, FlexibleContexts #-}
{-# OPTIONS_HADDOCK not-home #-}
-- |A type for profile preferences. These preference values are used by both 
-- Firefox and Opera profiles.
module Test.WebDriver.Common.Profile
       ( -- *Profiles and profile preferences
         Profile(..), PreparedProfile(..), ProfilePref(..), ToPref(..)
         -- * Preferences
       , getPref, addPref, deletePref
         -- * Extensions
       , addExtension, deleteExtension, hasExtension
         -- * Other files
       , addFile, deleteFile, hasFile
         -- *Preparing profiles from disk
       , prepareLoadedProfile_
         -- *Preparing zipped profiles
       , prepareZippedProfile, prepareZipArchive,
         prepareRawZip
         -- *Profile errors
       , ProfileParseError(..) 
       ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.Number (Number(..))
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, pack)
import Data.ByteString (ByteString)
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64 as B64

import Codec.Archive.Zip
import System.FilePath hiding (addExtension, hasExtension)

import Data.Fixed
import Data.Ratio
import Data.Int
import Data.Word
import Data.List
import Data.Maybe
import Data.Typeable

import Control.Exception
import Control.Applicative
import Control.Monad.Base

-- |This structure allows you to construct and manipulate profiles in pure code,
-- deferring execution of IO operations until the profile is \"prepared\". This 
-- type is shared by both Firefox and Opera profile code; when a distinction 
-- must be made, the phantom type parameter is used to differentiate.
data Profile b = Profile 
                 { -- |A set of filepaths pointing to files to place in
                   -- the extension directory. The first tuple element is the source 
                   -- path. The second tuple element is the destination path 
                   -- relative to the extension of the file.
                   profileFiles   :: [(FilePath, FilePath)]
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
  toJSON (PreparedProfile s) = object ["zip" .= s]

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
getPref k (Profile _ m) = HM.lookup k m

-- |Add a new preference entry to a profile, overwriting any existing entry
-- with the same key.
addPref :: ToPref a => Text -> a -> Profile b -> Profile b
addPref k v p = asMap p $ HM.insert k (toPref v)

-- |Delete an existing preference entry from a profile. This operation is
-- silent if the preference wasn't found.
deletePref :: Text -> Profile b -> Profile b
deletePref k p = asMap p $ HM.delete k

-- |Add a file to the profile directory. The first argument is the source
-- of the file on the local filesystem. The second argument is the destination 
-- as a path relative to a profile directory. 
addFile :: FilePath -> FilePath -> Profile b -> Profile b
addFile src dest p = asList p ((src, dest):)

-- |Delete a file from the profile directory. The first argument is the name of
-- file within the profile directory.
deleteFile :: FilePath -> Profile b -> Profile b
deleteFile path prof = asList prof $ filter (\(_,p) -> p == path)

-- |Determines if a profile contains the given file. specified as a path relative to
-- the profile directory.
hasFile :: String -> Profile b -> Bool
hasFile path (Profile files _) = isJust $ find (\(_,d) ->  d == path) files

-- |Add a new extension to the profile. The file path should refer to
-- an .xpi file or an extension directory on the filesystem. If possible,
-- you should avoiding adding the same extension twice to a given profile.
addExtension :: FilePath -> Profile b -> Profile b
addExtension path = addFile path ("extensions" </> name)
  where (_, name) = splitFileName path

-- |Delete an existing extension from the profile. The string parameter 
-- should refer to an .xpi file or directory located within the extensions 
-- directory of the profile. This operation has no effect if the extension was 
-- never added to the profile.
deleteExtension :: String -> Profile b -> Profile b
deleteExtension name = deleteFile ("extensions" </> name)

-- |Determines if a profile contains the given extension. specified as an .xpi file 
-- or directory name
hasExtension :: String -> Profile b -> Bool
hasExtension name prof = hasFile ("extensions" </> name) prof

asMap :: Profile b
         -> (HM.HashMap Text ProfilePref -> HM.HashMap Text ProfilePref)
         -> Profile b
asMap (Profile hs hm) f = Profile hs (f hm)

asList :: Profile b
         -> ([(FilePath, FilePath)] -> [(FilePath, FilePath)])
         -> Profile b
asList (Profile ls hm) f = Profile (f ls) hm


-- |Efficiently load an existing profile from disk and prepare it for network
-- transmission.
prepareLoadedProfile_ :: MonadBase IO m =>
                        FilePath -> m (PreparedProfile a)
prepareLoadedProfile_ path = prepareZipArchive <$>
                             liftBase (addFilesToArchive [OptRecursive] 
                                       emptyArchive [path])

-- |Prepare a zip file of a profile on disk for network transmission.
-- This function is very efficient at loading large profiles from disk.
prepareZippedProfile :: MonadBase IO m => 
                        FilePath -> m (PreparedProfile a)
prepareZippedProfile path = prepareRawZip <$> liftBase (LBS.readFile path)

-- |Prepare a zip archive of a profile for network transmission.
prepareZipArchive :: Archive -> PreparedProfile a
prepareZipArchive = prepareRawZip . fromArchive

-- |Prepare a ByteString of raw zip data for network transmission
prepareRawZip :: LBS.ByteString -> PreparedProfile a
prepareRawZip = PreparedProfile . B64.encode . SBS.concat . LBS.toChunks
