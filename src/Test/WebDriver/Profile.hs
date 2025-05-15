{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | A type for profile preferences. These preference values are used by both
-- Firefox and Opera profiles.
module Test.WebDriver.Profile (
  -- * Profiles and profile preferences
  Profile(..)
  , PreparedProfile(..)
  , ProfilePref(..)
  , ToPref(..)

  -- * Preferences
  , getPref
  , addPref
  , deletePref

  -- * Extensions
  , addExtension
  , deleteExtension
  , hasExtension

  -- * Other files and directories
  , addFile
  , deleteFile
  , hasFile

  -- * Miscellaneous profile operations
  , unionProfiles
  , onProfileFiles
  , onProfilePrefs

  -- * Preparing zipped profiles
  , prepareZippedProfile
  , prepareZipArchive
  , prepareRawZip

  -- * Profile errors
  , ProfileParseError(..)

  -- * Firefox
  , Firefox
  , defaultFirefoxProfile
  , loadFirefoxProfile
  , prepareFirefoxProfile
  , prepareFirefoxProfileArchive
  , prepareTempFirefoxProfile
  , prepareLoadedFirefoxProfile
  ) where

import Codec.Archive.Zip
import Control.Applicative
import Control.Arrow
import Control.Exception.Safe hiding (try)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (Result(..), encode, fromJSON)
import Data.Aeson.Parser (jstring, value')
import Data.Aeson.Types (FromJSON, ToJSON, Value(..), parseJSON, toJSON, typeMismatch)
import Data.Attoparsec.ByteString.Char8 as AP
import Data.ByteString as BS (readFile)
import qualified Data.ByteString.Base64.Lazy as B64
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Fixed
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.Ratio
import Data.Text (Text, pack)
import qualified Data.Text.Lazy.Encoding as TL
import Data.Word
import Prelude -- hides some "unused import" warnings
import System.Directory
import System.FilePath ((</>), (<.>), splitFileName)

#if MIN_VERSION_aeson(0,7,0)
import Data.Scientific
#else
import Data.Attoparsec.Number (Number(..))
#endif


-- | This structure allows you to construct and manipulate profiles in pure code,
-- deferring execution of IO operations until the profile is \"prepared\". This
-- type is shared by both Firefox and Opera profiles; when a distinction
-- must be made, the phantom type parameter is used to differentiate.
data Profile b = Profile {
  -- | A mapping from relative destination filepaths to source
  -- filepaths found on the filesystem. When the profile is
  -- prepared, these source filepaths will be moved to their
  -- destinations within the profile directory.
  --
  -- Using the destination path as the key ensures that
  -- there is one unique source path going to each
  -- destination path.
  profileFiles   :: HM.HashMap FilePath FilePath
  -- | A map of profile preferences. These are the settings
  -- found in the profile's prefs.js, and entries found in
  -- about:config
  , profilePrefs  :: HM.HashMap Text ProfilePref
  }
  deriving (Eq, Show)

-- | Represents a profile that has been prepared for
-- network transmission. The profile cannot be modified in this form.
newtype PreparedProfile b = PreparedProfile ByteString
  deriving (Eq, Show)

instance FromJSON (PreparedProfile s) where
  parseJSON v = PreparedProfile . TL.encodeUtf8 <$> parseJSON v

instance ToJSON (PreparedProfile s) where
  toJSON (PreparedProfile s) = toJSON $ TL.decodeUtf8 s

-- | A profile preference value. This is the subset of JSON values that excludes
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
-- | An error occured while attempting to parse a profile's preference file.
newtype ProfileParseError = ProfileParseError String
  deriving  (Eq, Show, Read, Typeable)

-- | A typeclass to convert types to profile preference values
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

instance ToPref ProfilePref where
  toPref = id

-- | Retrieve a preference from a profile by key name.
getPref :: Text -> Profile b -> Maybe ProfilePref
getPref k (Profile _ m) = HM.lookup k m

-- | Add a new preference entry to a profile, overwriting any existing entry
-- with the same key.
addPref :: ToPref a => Text -> a -> Profile b -> Profile b
addPref k v p = onProfilePrefs p $ HM.insert k (toPref v)

-- | Delete an existing preference entry from a profile. This operation is
-- silent if the preference wasn't found.
deletePref :: Text -> Profile b -> Profile b
deletePref k p = onProfilePrefs p $ HM.delete k

-- | Add a file to the profile directory. The first argument is the source
-- of the file on the local filesystem. The second argument is the destination
-- as a path relative to a profile directory. Overwrites any file that
-- previously pointed to the same destination
addFile :: FilePath -> FilePath -> Profile b -> Profile b
addFile src dest p = onProfileFiles p $ HM.insert dest src

-- | Delete a file from the profile directory. The first argument is the name of
-- file within the profile directory.
deleteFile :: FilePath -> Profile b -> Profile b
deleteFile path prof = onProfileFiles prof $ HM.delete path

-- | Determines if a profile contains the given file, specified as a path
-- relative to the profile directory.
hasFile :: String -> Profile b -> Bool
hasFile path (Profile files _) = path `HM.member` files

-- | Add a new extension to the profile. The file path should refer to
-- a .xpi file or an extension directory on the filesystem.
addExtension :: FilePath -> Profile b -> Profile b
addExtension path = addFile path ("extensions" </> name)
  where (_, name) = splitFileName path

-- | Delete an existing extension from the profile. The string parameter
-- should refer to an .xpi file or directory located within the extensions
-- directory of the profile. This operation has no effect if the extension was
-- never added to the profile.
deleteExtension :: String -> Profile b -> Profile b
deleteExtension name = deleteFile ("extensions" </> name)

-- | Determines if a profile contains the given extension. specified as an
-- .xpi file or directory name
hasExtension :: String -> Profile b -> Bool
hasExtension name prof = hasFile ("extensions" </> name) prof


-- | Takes the union of two profiles. This is the union of their 'HashMap'
-- fields.
unionProfiles :: Profile b -> Profile b -> Profile b
unionProfiles (Profile f1 p1) (Profile f2 p2)
  = Profile (f1 `HM.union` f2) (p1 `HM.union` p2)

-- | Modifies the 'profilePrefs' field of a profile.
onProfilePrefs ::
  Profile b
  -> (HM.HashMap Text ProfilePref -> HM.HashMap Text ProfilePref)
  -> Profile b
onProfilePrefs (Profile hs hm) f = Profile hs (f hm)

-- | Modifies the 'profileFiles' field of a profile
onProfileFiles ::
  Profile b
  -> (HM.HashMap FilePath FilePath -> HM.HashMap FilePath FilePath)
  -> Profile b
onProfileFiles (Profile ls hm) f = Profile (f ls) hm

-- | Prepare a zip file of a profile on disk for network transmission.
-- This function is very efficient at loading large profiles from disk.
prepareZippedProfile :: MonadIO m => FilePath -> m (PreparedProfile a)
prepareZippedProfile path = prepareRawZip <$> liftIO (LBS.readFile path)

-- | Prepare a zip archive of a profile for network transmission.
prepareZipArchive :: Archive -> PreparedProfile a
prepareZipArchive = prepareRawZip . fromArchive

-- | Prepare a ByteString of raw zip data for network transmission
prepareRawZip :: ByteString -> PreparedProfile a
prepareRawZip = PreparedProfile . B64.encode

-- * Firefox


-- | Phantom type used in the parameters of 'Profile' and 'PreparedProfile'
data Firefox

-- | Default Firefox Profile, used when no profile is supplied.
defaultFirefoxProfile :: Profile Firefox
defaultFirefoxProfile =
  Profile HM.empty
  $ HM.fromList [("app.update.auto", PrefBool False)
                ,("app.update.enabled", PrefBool False)
                ,("browser.startup.page" , PrefInteger 0)
                ,("browser.download.manager.showWhenStarting", PrefBool False)
                ,("browser.EULA.override", PrefBool True)
                ,("browser.EULA.3.accepted", PrefBool True)
                ,("browser.link.open_external", PrefInteger 2)
                ,("browser.link.open_newwindow", PrefInteger 2)
                ,("browser.offline", PrefBool False)
                ,("browser.safebrowsing.enabled", PrefBool False)
                ,("browser.search.update", PrefBool False)
                ,("browser.sessionstore.resume_from_crash", PrefBool False)
                ,("browser.shell.checkDefaultBrowser", PrefBool False)
                ,("browser.tabs.warnOnClose", PrefBool False)
                ,("browser.tabs.warnOnOpen", PrefBool False)
                ,("browser.startup.page", PrefInteger 0)
                ,("browser.safebrowsing.malware.enabled", PrefBool False)
                ,("startup.homepage_welcome_url", PrefString "about:blank")
                ,("devtools.errorconsole.enabled", PrefBool True)
                ,("focusmanager.testmode", PrefBool True)
                ,("dom.disable_open_during_load", PrefBool False)
                ,("extensions.autoDisableScopes" , PrefInteger 10)
                ,("extensions.logging.enabled", PrefBool True)
                ,("extensions.update.enabled", PrefBool False)
                ,("extensions.update.notifyUser", PrefBool False)
                ,("network.manage-offline-status", PrefBool False)
                ,("network.http.max-connections-per-server", PrefInteger 10)
                ,("network.http.phishy-userpass-length", PrefInteger 255)
                ,("offline-apps.allow_by_default", PrefBool True)
                ,("prompts.tab_modal.enabled", PrefBool False)
                ,("security.fileuri.origin_policy", PrefInteger 3)
                ,("security.fileuri.strict_origin_policy", PrefBool False)
                ,("security.warn_entering_secure", PrefBool False)
                ,("security.warn_submit_insecure", PrefBool False)
                ,("security.warn_entering_secure.show_once", PrefBool False)
                ,("security.warn_entering_weak", PrefBool False)
                ,("security.warn_entering_weak.show_once", PrefBool False)
                ,("security.warn_leaving_secure", PrefBool False)
                ,("security.warn_leaving_secure.show_once", PrefBool False)
                ,("security.warn_submit_insecure", PrefBool False)
                ,("security.warn_viewing_mixed", PrefBool False)
                ,("security.warn_viewing_mixed.show_once", PrefBool False)
                ,("signon.rememberSignons", PrefBool False)
                ,("toolkit.networkmanager.disable", PrefBool True)
                ,("toolkit.telemetry.enabled", PrefBool False)
                ,("toolkit.telemetry.prompted", PrefInteger 2)
                ,("toolkit.telemetry.rejected", PrefBool True)
                ,("javascript.options.showInConsole", PrefBool True)
                ,("browser.dom.window.dump.enabled", PrefBool True)
                ,("webdriver_accept_untrusted_certs", PrefBool True)
                ,("webdriver_enable_native_events", native_events)
                ,("webdriver_assume_untrusted_issuer", PrefBool True)
                ,("dom.max_script_run_time", PrefInteger 30)
                ]
    where
#ifdef darwin_HOST_OS
      native_events = PrefBool False
#else
      native_events = PrefBool True
#endif


-- | Load an existing profile from the file system. Any prepared changes made to
-- the 'Profile' will have no effect to the profile on disk.
--
-- To make automated browser run smoothly, preferences found in
-- 'defaultFirefoxProfile' are automatically merged into the preferences of the on-disk
-- profile. The on-disk profile's preference will override those found in the
-- default profile.
loadFirefoxProfile :: MonadIO m => FilePath -> m (Profile Firefox)
loadFirefoxProfile path = do
  unionProfiles defaultFirefoxProfile <$> (Profile <$> getFiles <*> getPrefs)
  where
    userPrefFile = path </> "prefs" <.> "js"

    getFiles = HM.fromList . map (id &&& (path </>)) . filter isNotIgnored
               <$> liftIO (getDirectoryContents path)
      where isNotIgnored = (`notElem`
                         [".", "..", "OfflineCache", "Cache"
                         ,"parent.lock", ".parentlock", ".lock"
                         ,userPrefFile])

    getPrefs = liftIO $ do
       prefFileExists <- doesFileExist userPrefFile
       if prefFileExists
        then HM.fromList <$> (parsePrefs =<< BS.readFile userPrefFile)
        else return HM.empty
      where parsePrefs s = either (throwIO . ProfileParseError) return
                           $ AP.parseOnly prefsParser s

-- | Prepare a firefox profile for network transmission.
-- Internally, this function constructs a Firefox profile as a zip archive,
-- then base64 encodes the zipped data.
prepareFirefoxProfile :: forall m. (MonadIO m) => Profile Firefox -> m (PreparedProfile Firefox)
prepareFirefoxProfile ffProfile = prepareZipArchive <$> prepareFirefoxProfileArchive ffProfile

-- | Prepare a firefox profile for network transmission.
-- Internally, this function constructs a Firefox profile as a zip archive,
-- then base64 encodes the zipped data.
prepareFirefoxProfileArchive :: forall m. (MonadIO m) => Profile Firefox -> m Archive
prepareFirefoxProfileArchive (Profile {profileFiles = files, profilePrefs = prefs}) = do
  let baseArchive = emptyArchive
                  & addEntryToArchive (toEntry ("user" <.> "js") 0 userJsContents)

  liftIO $ foldM addProfileFile baseArchive (HM.toList files)

  where
    userJsContents = LBS.concat
        . map (\(k, v) -> LBS.concat [ "user_pref(", encode k,
                                       ", ", encode v, ");\n"])
        . HM.toList $ prefs

    addProfileFile :: Archive -> (FilePath, FilePath) -> IO Archive
    addProfileFile archive (dest, pathOnDisk) = doesPathExist pathOnDisk >>= \case
      False -> throwIO $ userError ("Path didn't exist: " ++ show pathOnDisk)
      True -> doesDirectoryExist pathOnDisk >>= \case
        False -> do
          bytes <- LBS.readFile pathOnDisk
          return $ archive
                 & addEntryToArchive (toEntry dest 0 bytes)
        True -> do
          contents <- listDirectory pathOnDisk
          foldM addProfileFile archive [(dest </> x, pathOnDisk </> x) | x <- contents]

-- | Apply a function on a default profile, and
-- prepare the result. The Profile passed to the handler function is
-- the default profile used by sessions when Nothing is specified
prepareTempFirefoxProfile :: (MonadIO m) => (Profile Firefox -> Profile Firefox) -> m (PreparedProfile Firefox)
prepareTempFirefoxProfile f = prepareFirefoxProfile . f $ defaultFirefoxProfile

-- | Convenience function to load an existing Firefox profile from disk, apply
-- a handler function, and then prepare the result for network transmission.
--
-- NOTE: like 'prepareFirefoxProfile', the same caveat about large profiles applies.
prepareLoadedFirefoxProfile :: (
  MonadIO m
  )
  => FilePath
  -> (Profile Firefox -> Profile Firefox)
  -> m (PreparedProfile Firefox)
prepareLoadedFirefoxProfile path f = liftM f (loadFirefoxProfile path) >>= prepareFirefoxProfile

-- firefox prefs.js parser

prefsParser :: AP.Parser [(Text, ProfilePref)]
prefsParser = AP.many1 $ do
  void . padSpaces $ AP.string "user_pref("
  k <- prefKey <?> "preference key"
  void . padSpaces $ AP.char ','
  v <- prefVal <?> "preference value"
  void . padSpaces $ AP.string ");"
  return (k,v)
  where
    prefKey = jstring
    prefVal = do
      v <- value'
      case fromJSON v of
        Error str -> fail str
        Success p -> return p

    padSpaces p = spaces >> p <* spaces
    spaces = many (AP.endOfLine <|> void AP.space <|> void comment)
      where
        comment = inlineComment <|> lineComment
        lineComment = AP.char '#' *> AP.manyTill AP.anyChar AP.endOfLine
        inlineComment = AP.string "/*" *> AP.manyTill AP.anyChar (AP.string "*/")
