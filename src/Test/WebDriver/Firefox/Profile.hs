{-# LANGUAGE CPP, OverloadedStrings, FlexibleContexts, 
             GeneralizedNewtypeDeriving, EmptyDataDecls #-}
-- |A module for working with Firefox profiles. Firefox profiles are manipulated
-- in pure code and then \"prepared\" for network transmission. 
module Test.WebDriver.Firefox.Profile 
       ( -- * Profiles
         Firefox(..), Profile(..), PreparedProfile
         -- * Preferences
       , ProfilePref(..), ToPref(..)
       , addPref, getPref, deletePref
         -- * Extensions
       , addExtension, deleteExtension
         -- * Loading and preparing profiles
       , loadProfile, prepareProfile
       , prepareTempProfile, prepareLoadedProfile
       ) where
import Test.WebDriver.Common.Profile
import Test.WebDriver.JSON (fromJSON')

import Data.Aeson
import Data.Aeson.Parser (jstring, value')
import Data.Attoparsec.Char8 as AP
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Text (Text, pack)
import Data.ByteString as BS (ByteString, readFile)
import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Base64 as B64

import System.IO
import System.FilePath hiding (hasExtension, addExtension)
import System.Directory
import System.IO.Temp
import Codec.Archive.Zip
import Distribution.Simple.Utils
import Distribution.Verbosity

import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Applicative
import Control.Exception.Lifted
import Data.Typeable


-- |Phantom type used in the parameters of 'Profile' and 'PreparedProfile'
data Firefox

tempProfile :: MonadBase IO m => m (Profile Firefox)
tempProfile = liftBase $ defaultProfile <$> mkTemp

-- |Load an existing profile from the file system. Any prepared changes made to
-- the 'Profile' will have no effect to the profile on disk.
loadProfile :: MonadBaseControl IO m => FilePath -> m (Profile Firefox)
loadProfile path = liftBase $ do
  Profile{ profileDir = d } <- tempProfile
  Profile <$> pure d <*> getExtensions <*> getPrefs
  where
    extD = path </> "extensions"
    userPref = path </> "prefs" <.> "js"
    getExtensions = HS.fromList . filter (`elem` [".",".."]) 
                    <$> getDirectoryContents extD
    getPrefs = HM.fromList <$> (parsePrefs =<< BS.readFile userPref)
    
    parsePrefs s = either (throwIO . ProfileParseError)
                          return
                   $ parseOnly prefsParser s

-- |Prepare a firefox profile for network transmission.
-- Internally, this function constructs a Firefox profile within a temp 
-- directory, archives it as a zip file, and then base64 encodes the zipped 
-- data. The temporary directory is deleted afterwards
prepareProfile :: MonadBase IO m => 
                  Profile Firefox -> m (PreparedProfile Firefox)
prepareProfile Profile {profileDir = d, profileExts = s, 
                        profilePrefs = m} 
  = liftBase $ do 
      createDirectoryIfMissing False extensionD
      extPaths <- mapM canonicalizePath . HS.toList $ s
      forM_ extPaths installExtension
      withFile userPrefs WriteMode writeUserPrefs
      prof <- PreparedProfile . B64.encode . SBS.concat . LBS.toChunks 
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
-- prepare the result. The Profile passed to the handler function is
-- the default profile used by sessions when Nothing is specified
prepareTempProfile :: MonadBase IO m => 
                     (Profile Firefox -> Profile Firefox) 
                     -> m (PreparedProfile Firefox)
prepareTempProfile f = liftM f tempProfile >>= prepareProfile

-- |Convenience function to load an existing Firefox profile from disk, apply
-- a handler function, and then prepare the result for network transmission.
prepareLoadedProfile :: MonadBaseControl IO m =>
                        FilePath
                        -> (Profile Firefox -> Profile Firefox)
                        -> m (PreparedProfile Firefox)
prepareLoadedProfile path f = liftM f (loadProfile path) >>= prepareProfile

defaultProfile :: FilePath -> Profile Firefox
defaultProfile d = 
  Profile d HS.empty
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

prefsParser :: Parser [(Text, ProfilePref)]
prefsParser = many $ do 
  padSpaces $ string "user_pref("
  k <- prefKey <?> "preference key"
  padSpaces $ char ','
  v <- prefVal <?> "preference value"
  padSpaces $ string ");"
  endOfLine
  return (k,v)
  where
    prefKey = jstring
    prefVal = do 
      v <- value'
      case fromJSON v of
        Error str -> fail str
        Success p -> return p
    spaces = AP.takeWhile isSpace
    padSpaces p = spaces >> p <* spaces