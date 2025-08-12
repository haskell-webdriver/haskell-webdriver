{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-deriving-typeable #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | A type for profile preferences. These preference values are used by both
-- Firefox and Opera profiles.
module Test.WebDriver.Profile (
  -- * Profiles and profile preferences
  Profile(..)
  , ProfilePref(..)
  , ToPref(..)

  -- * Preferences
  , getPref
  , addPref
  , deletePref

  -- * Extensions
  -- , addExtension
  -- , deleteExtension
  , hasExtension

  -- * Miscellaneous profile operations
  , unionProfiles
  , onProfileFiles
  , onProfilePrefs

  -- * Profile errors
  , ProfileParseError(..)

  -- * Firefox
  , Firefox
  , defaultFirefoxProfile
  , loadFirefoxProfile
  , saveFirefoxProfile
  , firefoxProfileToArchive
  ) where

import Codec.Archive.Zip
import Control.Applicative
import Control.Exception.Safe hiding (try)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (Result(..), encode, fromJSON)
import qualified Data.Aeson as A
import Data.Aeson.Parser (jstring, value')
import Data.Aeson.Types (FromJSON, ToJSON, Value(..), parseJSON, toJSON, typeMismatch)
import Data.Attoparsec.ByteString.Char8 as AP
import Data.ByteString as BS (readFile)
import qualified Data.ByteString.Base64.Lazy as B64L
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Fixed
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import Data.Int
import qualified Data.List as L
import Data.Maybe
import Data.Ratio
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Word
import Prelude -- hides some "unused import" warnings
import System.Directory
import System.FilePath ((</>), (<.>), takeFileName)

#if MIN_VERSION_aeson(0,7,0)
import Data.Scientific
#else
import Data.Attoparsec.Number (Number(..))
#endif


-- | This structure allows you to construct and manipulate profiles. This type
-- is shared by both Firefox and Opera profiles; when a distinction must be
-- made, the phantom type parameter is used to differentiate.
data Profile b = Profile {
  -- | A mapping from relative destination filepaths to source contents.
  profileFiles   :: HM.HashMap FilePath ByteString
  -- | A map of profile preferences. These are the settings found in the
  -- profile's prefs.js, and entries found in about:config
  , profilePrefs  :: HM.HashMap Text ProfilePref
  }
  deriving (Eq, Show)

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
  parseJSON (Number s)
    | base10Exponent s >= 0 = return $ PrefInteger (coefficient s * 10^(base10Exponent s))
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

-- | Determines if a profile contains the given file, specified as a path
-- relative to the profile directory.
hasFile :: String -> Profile b -> Bool
hasFile path (Profile files _) = path `HM.member` files

-- | Determines if a profile contains the given extension. specified as an
-- .xpi file or directory name
hasExtension :: String -> Profile b -> Bool
hasExtension name = hasFile ("extensions" </> name)

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
  -> (HM.HashMap FilePath ByteString -> HM.HashMap FilePath ByteString)
  -> Profile b
onProfileFiles (Profile ls hm) f = Profile (f ls) hm

-- -- | Prepare a zip file of a profile on disk for network transmission.
-- -- This function is very efficient at loading large profiles from disk.
-- prepareZippedProfile :: MonadIO m => FilePath -> m (PreparedProfile a)
-- prepareZippedProfile path = prepareRawZip <$> liftIO (LBS.readFile path)

-- -- | Prepare a zip archive of a profile for network transmission.
-- prepareZipArchive :: Archive -> PreparedProfile a
-- prepareZipArchive = prepareRawZip . fromArchive

-- -- | Prepare a 'ByteString' of raw zip data for network transmission.
-- prepareRawZip :: ByteString -> PreparedProfile a
-- prepareRawZip = PreparedProfile . B64.encode

-- * Firefox


-- | Phantom type used in the parameters of 'Profile' and 'PreparedProfile'
data Firefox

-- | Default Firefox Profile, used when no profile is supplied.
defaultFirefoxProfile :: Profile Firefox
defaultFirefoxProfile = Profile HM.empty $ HM.fromList [
  ("app.update.auto", PrefBool False)
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
loadFirefoxProfile :: forall m. MonadIO m => FilePath -> m (Profile Firefox)
loadFirefoxProfile path = unionProfiles defaultFirefoxProfile <$> (Profile <$> getFiles path <*> getPrefs)
  where
    userPrefFile = path </> "prefs" <.> "js"

    getFiles :: FilePath -> m (HM.HashMap FilePath ByteString)
    getFiles dir = liftIO $ do
      allFiles <- getAllFiles dir
      foldM (\acc p -> HM.insert p <$> LBS.readFile p <*> pure acc) HM.empty $
        filter ((`notElem` filesToIgnore) . takeFileName) allFiles

    getAllFiles :: FilePath -> IO [FilePath]
    getAllFiles dir = do
      exists <- doesDirectoryExist dir
      if not exists then return [] else do
        contents <- map (dir </>) <$> listDirectory dir
        files <- filterM doesFileExist contents
        dirs <- filterM doesDirectoryExist contents
        subFiles <- concat <$> mapM getAllFiles dirs
        return (files ++ subFiles)

    filesToIgnore = [".", "..", "OfflineCache", "Cache", "parent.lock", ".parentlock", ".lock", userPrefFile]

    getPrefs = liftIO $ do
       doesFileExist userPrefFile >>= \case
         True -> HM.fromList <$> (parsePrefs =<< BS.readFile userPrefFile)
         False -> return HM.empty
      where
        parsePrefs s = either (throwIO . ProfileParseError) return
                     $ AP.parseOnly prefsParser s

-- | Save a Firefox profile to a destination directory. This directory should
-- already exist.
saveFirefoxProfile :: MonadIO m => Profile Firefox -> FilePath -> m ()
saveFirefoxProfile (firefoxProfileToArchive -> archive) dest = liftIO $ flip extractFilesFromArchive archive [
  OptRecursive
  , OptDestination dest
  ]

-- | Prepare a Firefox profile for network transmission.
firefoxProfileToArchive :: Profile Firefox -> Archive
firefoxProfileToArchive (Profile {profileFiles = files, profilePrefs = prefs}) =
  foldl addProfileFile baseArchive (HM.toList files)

  where
    baseArchive = emptyArchive
                & addEntryToArchive (toEntry ("user" <.> "js") 0 userJsContents)

    userJsContents = prefs
                   & HM.toList
                   & map (\(k, v) -> LBS.concat [ "user_pref(", encode k, ", ", encode v, ");\n"])
                   & LBS.concat

    addProfileFile :: Archive -> (FilePath, ByteString) -> Archive
    addProfileFile archive (dest, bytes) = addEntryToArchive (toEntry dest 0 bytes) archive

instance ToJSON (Profile Firefox) where
  toJSON prof = firefoxProfileToArchive prof
              & fromArchive
              & B64L.encode
              & LBS.toStrict
              & T.decodeUtf8
              & A.String

instance FromJSON (Profile Firefox) where
  parseJSON (String t) = case B64L.decode (TL.encodeUtf8 (TL.fromStrict t)) of
    Left err -> fail ("Couldn't decode Firefox profile archive: " <> err)
    Right bytes -> case toArchiveOrFail bytes of
      Left err -> fail ("Couldn't unzip Firefox profile archive: " <> err)
      Right archive -> return $ Profile {
        profileFiles = HM.fromList [(eRelativePath, fromEntry e) | e@(Entry {..}) <- otherFiles]
        , profilePrefs = prefs
        }
        where
          prefs = case L.find (\(Entry {..}) -> eRelativePath == "prefs.js") (zEntries archive) of
            Nothing -> mempty
            Just entry -> case AP.parseOnly prefsParser (BL.toStrict $ fromEntry entry) of
              Left _err -> mempty
              Right p -> HM.fromList p

          otherFiles = [e | e@(Entry {..}) <- zEntries archive, eRelativePath /= "prefs.js"]
  parseJSON other = typeMismatch "Profile Firefox" other

-- Firefox prefs.js parser

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
