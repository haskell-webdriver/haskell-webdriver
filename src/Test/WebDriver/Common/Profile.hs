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
         -- * Other files and directories
       , addFile, deleteFile, hasFile
         -- * Miscellaneous profile operations
       , unionProfiles, onProfileFiles, onProfilePrefs
         -- *Preparing profiles from disk
       , prepareLoadedProfile_
         -- *Preparing zipped profiles
       , prepareZippedProfile, prepareZipArchive,
         prepareRawZip
         -- *Profile errors
       , ProfileParseError(..)
       ) where

import System.Directory
import System.FilePath hiding (addExtension, hasExtension)
import Codec.Archive.Zip
import Data.Aeson
import Data.Aeson.Types

#if MIN_VERSION_aeson(0,7,0)
import Data.Scientific
#else
import Data.Attoparsec.Number (Number(..))
#endif

import qualified Data.HashMap.Strict as HM
import Data.Text (Text, pack)
import Data.ByteString.Lazy (ByteString)
--import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Text.Lazy.Encoding as TL


import Data.Fixed
import Data.Ratio
import Data.Int
import Data.Word
import Data.Typeable

import Control.Exception
import Control.Applicative
import Control.Monad.Base

-- |This structure allows you to construct and manipulate profiles in pure code,
-- deferring execution of IO operations until the profile is \"prepared\". This
-- type is shared by both Firefox and Opera profiles; when a distinction
-- must be made, the phantom type parameter is used to differentiate.
data Profile b = Profile
                 { -- |A mapping from relative destination filepaths to source
                   -- filepaths found on the filesystem. When the profile is
                   -- prepared, these source filepaths will be moved to their
                   -- destinations within the profile directory.
                   --
                   -- Using the destination path as the key ensures that
                   -- there is one unique source path going to each
                   -- destination path.
                   profileFiles   :: HM.HashMap FilePath FilePath
                   -- |A map of profile preferences. These are the settings
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
  parseJSON v = PreparedProfile . TL.encodeUtf8 <$> parseJSON v

instance ToJSON (PreparedProfile s) where
  toJSON (PreparedProfile s) = toJSON $ TL.decodeUtf8 s

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
#if MIN_VERSION_aeson(0,7,0)
  parseJSON (Number s) | base10Exponent s >= 0 = return $ PrefInteger (coefficient s * 10^(base10Exponent s))
                       | otherwise = return $ PrefDouble $ realToFrac s
#else
  parseJSON (Number (I i)) = return $ PrefInteger i
  parseJSON (Number (D d)) = return $ PrefDouble d
#endif
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
addPref k v p = onProfilePrefs p $ HM.insert k (toPref v)

-- |Delete an existing preference entry from a profile. This operation is
-- silent if the preference wasn't found.
deletePref :: Text -> Profile b -> Profile b
deletePref k p = onProfilePrefs p $ HM.delete k

-- |Add a file to the profile directory. The first argument is the source
-- of the file on the local filesystem. The second argument is the destination
-- as a path relative to a profile directory. Overwrites any file that
-- previously pointed to the same destination
addFile :: FilePath -> FilePath -> Profile b -> Profile b
addFile src dest p = onProfileFiles p $ HM.insert dest src

-- |Delete a file from the profile directory. The first argument is the name of
-- file within the profile directory.
deleteFile :: FilePath -> Profile b -> Profile b
deleteFile path prof = onProfileFiles prof $ HM.delete path

-- |Determines if a profile contains the given file, specified as a path
-- relative to the profile directory.
hasFile :: String -> Profile b -> Bool
hasFile path (Profile files _) = path `HM.member` files

-- |Add a new extension to the profile. The file path should refer to
-- a .xpi file or an extension directory on the filesystem.
addExtension :: FilePath -> Profile b -> Profile b
addExtension path = addFile path ("extensions" </> name)
  where (_, name) = splitFileName path

-- |Delete an existing extension from the profile. The string parameter
-- should refer to an .xpi file or directory located within the extensions
-- directory of the profile. This operation has no effect if the extension was
-- never added to the profile.
deleteExtension :: String -> Profile b -> Profile b
deleteExtension name = deleteFile ("extensions" </> name)

-- |Determines if a profile contains the given extension. specified as an
-- .xpi file or directory name
hasExtension :: String -> Profile b -> Bool
hasExtension name prof = hasFile ("extensions" </> name) prof


-- |Takes the union of two profiles. This is the union of their 'HashMap'
-- fields.
unionProfiles :: Profile b -> Profile b -> Profile b
unionProfiles (Profile f1 p1) (Profile f2 p2)
  = Profile (f1 `HM.union` f2) (p1 `HM.union` p2)

-- |Modifies the 'profilePrefs' field of a profile.
onProfilePrefs :: Profile b
                  -> (HM.HashMap Text ProfilePref
                      -> HM.HashMap Text ProfilePref)
                  -> Profile b
onProfilePrefs (Profile hs hm) f = Profile hs (f hm)

-- |Modifies the 'profileFiles' field of a profile
onProfileFiles :: Profile b
                  -> (HM.HashMap FilePath FilePath
                      -> HM.HashMap FilePath FilePath)
                  -> Profile b
onProfileFiles (Profile ls hm) f = Profile (f ls) hm


-- |Efficiently load an existing profile from disk and prepare it for network
-- transmission.
prepareLoadedProfile_ :: MonadBase IO m =>
                        FilePath -> m (PreparedProfile a)
prepareLoadedProfile_ path = liftBase $ do
  oldWd <- getCurrentDirectory
  setCurrentDirectory path
  prepareZipArchive <$>
    liftBase (addFilesToArchive [OptRecursive]
              emptyArchive ["."])
    <* setCurrentDirectory oldWd

-- |Prepare a zip file of a profile on disk for network transmission.
-- This function is very efficient at loading large profiles from disk.
prepareZippedProfile :: MonadBase IO m =>
                        FilePath -> m (PreparedProfile a)
prepareZippedProfile path = prepareRawZip <$> liftBase (LBS.readFile path)

-- |Prepare a zip archive of a profile for network transmission.
prepareZipArchive :: Archive -> PreparedProfile a
prepareZipArchive = prepareRawZip . fromArchive

-- |Prepare a ByteString of raw zip data for network transmission
prepareRawZip :: ByteString -> PreparedProfile a
prepareRawZip = PreparedProfile . B64.encode
