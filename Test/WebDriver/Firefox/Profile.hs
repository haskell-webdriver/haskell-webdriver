{-# LANGUAGE CPP, TypeSynonymInstances, OverloadedStrings,
             GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
-- |A module for working with Firefox profiles. Firefox profiles are manipulated
-- in pure code and then \"prepared\" for network transmission. 
module Test.WebDriver.Firefox.Profile 
       ( -- * Profiles
         FirefoxProfile(..), PreparedFirefoxProfile
         -- * Preferences
       , FirefoxPref(..), ToFirefox(..)
       , addPref, getPref, deletePref
         -- * Extensions
       , addExtension, deleteExtension
         -- * Loading and preparing profiles
       , loadProfile, prepareProfile
       , prepareTempProfile, prepareLoadedProfile
       ) where
import Data.Aeson
import Data.Attoparsec.Text as AP
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Text (Text, pack)
import Data.Text.IO as TIO
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Base64 as B64

import Data.Fixed
import Data.Ratio
import Data.Int
import Data.Word
import Data.Char

import System.IO
import System.FilePath hiding (hasExtension, addExtension)
import System.Directory
import System.IO.Temp
import Codec.Archive.Zip
import Distribution.Simple.Utils
import Distribution.Verbosity

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Exception.Lifted
import Data.Typeable

-- |A pure representation of a FirefoxProfile. This structure allows you to
-- construct and manipulate Firefox profiles in pure code, deferring execution
-- of IO operations until the profile is \"prepared\" using either
-- 'prepareProfile' or one of the wrapper functions 'prepareTempProfile' and 
-- 'prepareLoadedProfile'.
data FirefoxProfile = FirefoxProfile 
                      { -- |Location of the profile in the file system
                        profileDir    :: FilePath
                        -- |A set of filepaths pointing to Firefox extensions.
                        -- These paths can either refer to an .xpi file
                        -- or an extension directory
                      , profileExts   :: HS.HashSet FilePath
                        -- |A map of Firefox preferences. These are the settings
                        -- found in the profile's prefs.js, and entries found in
                        -- about:config
                      , profilePrefs  :: HM.HashMap Text FirefoxPref
                      }
                    deriving (Eq, Show)

-- |A Firefox preference. This is the subset of JSON values that excludes
-- arrays and objects.
data FirefoxPref = PrefInteger !Integer
                 | PrefDouble  !Double
                 | PrefString  !Text
                 | PrefBool    !Bool
                   deriving (Eq, Show)

instance ToJSON FirefoxPref where
  toJSON v = case v of
    PrefInteger i -> toJSON i
    PrefDouble d  -> toJSON d
    PrefString s  -> toJSON s
    PrefBool  b   -> toJSON b


-- |Represents a Firefox profile that has been prepared for 
-- network transmission. The profile cannot be modified in this form.
newtype PreparedFirefoxProfile = PreparedFirefoxProfile ByteString
  deriving (Eq, Show, ToJSON, FromJSON)


instance Exception ProfileParseError
-- |An error occured while attempting to parse a profile's prefs.js file 
newtype ProfileParseError = ProfileParseError String
                          deriving  (Eq, Show, Read, Typeable)

-- |A typeclass to convert types to Firefox preference values
class ToFirefox a where
  toFirefox :: a -> FirefoxPref

instance ToFirefox Text where
  toFirefox = PrefString
  
instance ToFirefox String where
  toFirefox = toFirefox . pack

instance ToFirefox Bool where
  toFirefox = PrefBool
  
instance ToFirefox Integer where
  toFirefox = PrefInteger

#define I(t) instance ToFirefox t where toFirefox = PrefInteger . toInteger

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

instance ToFirefox Double where
  toFirefox = PrefDouble

instance ToFirefox Float where
  toFirefox = PrefDouble . realToFrac
  
instance (Integral a) => ToFirefox (Ratio a) where
  toFirefox = PrefDouble . realToFrac
  
instance (HasResolution r) => ToFirefox (Fixed r) where
  toFirefox = PrefDouble . realToFrac

-- |Retrieve a preference from a profile by key name.
getPref :: Text -> FirefoxProfile -> Maybe FirefoxPref
getPref k (FirefoxProfile _ _ m) = HM.lookup k m

-- |Add a new preference entry to a profile, overwriting any existing entry
-- with the same key.
addPref :: ToFirefox a => Text -> a -> FirefoxProfile -> FirefoxProfile
addPref k v p = asMap p $ HM.insert k (toFirefox v)

-- |Delete an existing preference entry from a profile. This operation is
-- silent if the preference wasn't found.
deletePref :: Text -> FirefoxProfile -> FirefoxProfile
deletePref k p = asMap p $ HM.delete k

-- |Add a new extension to the profile. The file path should refer to
-- an .xpi file or an extension directory. This operation has no effect if
-- the same extension has already been added to this profile.
addExtension :: FilePath -> FirefoxProfile -> FirefoxProfile
addExtension path p = asSet p $ HS.insert path

-- |Delete an existing extension from the profile. The file path should refer
-- to an .xpi file or an extension directory. This operation has no effect if
-- the extension was never added to the profile.
deleteExtension :: FilePath -> FirefoxProfile -> FirefoxProfile
deleteExtension path p = asSet p $ HS.delete path

asMap :: FirefoxProfile
         -> (HM.HashMap Text FirefoxPref -> HM.HashMap Text FirefoxPref)
         -> FirefoxProfile
asMap (FirefoxProfile p hs hm) f = FirefoxProfile p hs (f hm)

asSet :: FirefoxProfile
         -> (HS.HashSet FilePath -> HS.HashSet FilePath)
         -> FirefoxProfile
asSet (FirefoxProfile p hs hm) f = FirefoxProfile p (f hs) hm


tempProfile :: MonadIO m => m FirefoxProfile
tempProfile = liftIO $ defaultProfile <$> mkTemp

-- |Load an existing profile from the file system. Any prepared changes made to
-- the FirefoxProfile will have no effect to the profile on disk.
loadProfile :: MonadIO m => FilePath -> m FirefoxProfile
loadProfile path = liftIO $ do
  FirefoxProfile{ profileDir = d } <- tempProfile
  FirefoxProfile <$> pure d <*> getExtensions <*> getPrefs
  where
    extD = path </> "extensions"
    userPref = path </> "prefs" <.> "js"
    getExtensions = HS.fromList . filter (`elem` [".",".."]) 
                    <$> getDirectoryContents extD
    getPrefs = HM.fromList <$> (parsePrefs =<< TIO.readFile userPref)
    
    parsePrefs s = either (throwIO . ProfileParseError)
                          return 
                   $ parseOnly prefsParser s

-- |Prepare a FirefoxProfile for network transmission.
--
-- Internally, this function constructs a Firefox profile within a temp 
-- directory, archives it as a zip file, and then base64 encodes the zipped 
-- data. The temporary directory is deleted afterwards
prepareProfile :: MonadIO m => FirefoxProfile -> m PreparedFirefoxProfile
prepareProfile FirefoxProfile {profileDir = d, profileExts = s, 
                               profilePrefs = m} 
  = liftIO $ do 
      createDirectoryIfMissing False extensionD
      extPaths <- mapM canonicalizePath . HS.toList $ s
      forM_ extPaths installExtension
      withFile userPrefs WriteMode writeUserPrefs
      prof <-  PreparedFirefoxProfile . B64.encode . SBS.concat . LBS.toChunks 
               . fromArchive 
               <$> addFilesToArchive [OptRecursive] emptyArchive [d]
      removeDirectoryRecursive d
      return prof
  where
    extensionD = d </> "extensions"
    userPrefs  = d </> "prefs" <.> "js"
    
    installExtension ePath = 
      case splitExtension ePath of
           (_,".xpi") -> installOrdinaryFile silent ePath dest
           _          -> installDirectoryContents silent ePath dest
      where
        dest = extensionD </> eFile
        (_,eFile) = splitFileName ePath
      
    writeUserPrefs h =
      forM_ (HM.toList m) $ \(k, v) ->
        LBS.hPut h . LBS.concat
          $ [ "user_pref(", encode k, ", ", encode v, ");\n"]
  

-- |Apply a function on an automatically generated default profile, and
-- prepare the result. The FirefoxProfile passed to the handler function is
-- the same one used by sessions when no Firefox profile is specified
prepareTempProfile :: MonadIO m => 
                     (FirefoxProfile -> FirefoxProfile) 
                     -> m PreparedFirefoxProfile
prepareTempProfile f = liftM f tempProfile >>= prepareProfile

-- |Convenience function to load an existing Firefox profile from disk, apply
-- a handler function, and then prepare the result for network transmission.
prepareLoadedProfile :: MonadIO m =>
                        FilePath
                        -> (FirefoxProfile -> FirefoxProfile)
                        -> m PreparedFirefoxProfile
prepareLoadedProfile path f = liftM f (loadProfile path) >>= prepareProfile

defaultProfile :: FilePath -> FirefoxProfile
defaultProfile d = 
  FirefoxProfile d HS.empty
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

mkTemp :: IO FilePath
mkTemp = do 
  d <- getTemporaryDirectory
  createTempDirectory d ""
  
-- firefox prefs.js parser

prefsParser = many prefLine

prefLine = do 
  padSpaces $ string "user_pref("
  k <- prefKey
  padSpaces $ char ','
  v <- prefVal
  padSpaces $  string ");"
  endOfLine
  return (k,v)
  where
    spaces = AP.takeWhile isSpace
    padSpaces p = spaces >> p >> spaces

prefKey = str
prefVal = boolVal <|> stringVal <|> intVal <|> doubleVal
  where
    boolVal   = boolTrue <|> boolFalse     
    boolTrue  = string "true"  >> return (PrefBool True)
    boolFalse = string "false" >> return (PrefBool False)
    stringVal = PrefString <$> str
    intVal    = PrefInteger <$> signed decimal
    doubleVal = PrefDouble <$> double
    
str = char '"' >> AP.takeWhile (not . (=='"')) <* char '"'