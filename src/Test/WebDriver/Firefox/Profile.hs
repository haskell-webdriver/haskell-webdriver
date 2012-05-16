{-# LANGUAGE CPP, OverloadedStrings, FlexibleContexts, 
             GeneralizedNewtypeDeriving, EmptyDataDecls, 
             ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-} -- suppress warnings from attoparsec
-- |A module for working with Firefox profiles. Firefox profiles are manipulated
-- in pure code and then \"prepared\" for network transmission. 
module Test.WebDriver.Firefox.Profile 
       ( -- * Profiles
         Firefox, Profile(..), PreparedProfile
       , defaultProfile
         -- * Preferences
       , ProfilePref(..), ToPref(..)
       , addPref, getPref, deletePref
         -- * Extensions
       , addExtension, deleteExtension, hasExtension
         -- * Other Files
       , addFile, removeFile, hasFile
         -- * Loading and preparing profiles
       , prepareProfile, prepareTempProfile
         -- ** Preparing profiles from disk
       , loadProfile, prepareLoadedProfile, prepareLoadedProfile_
         -- ** Preparing zip archives
       , prepareZippedProfile, prepareZipArchive, prepareRawZip
         -- ** Preferences parsing error
       , ProfileParseError(..)
       ) where
import Test.WebDriver.Common.Profile
import Data.Aeson
import Data.Aeson.Parser (jstring, value')
import Data.Attoparsec.Char8 as AP
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.ByteString as BS (readFile)
import qualified Data.ByteString.Lazy.Char8 as LBS

import System.FilePath hiding (addExtension, hasExtension)
import System.Directory
import System.IO.Temp (createTempDirectory)
import qualified System.File.Tree as FS
import Codec.Archive.Zip

import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Applicative
import Data.List
import Control.Exception.Lifted hiding (try)

import Prelude hiding (catch)

-- |Phantom type used in the parameters of 'Profile' and 'PreparedProfile'
data Firefox

defaultProfile :: Profile Firefox
defaultProfile = 
  Profile []
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


-- |Load an existing profile from the file system. Any prepared changes made to
-- the 'Profile' will have no effect to the profile on disk.
loadProfile :: MonadBaseControl IO m => FilePath -> m (Profile Firefox)
loadProfile path = liftBase $ do
  Profile <$> getFiles <*> getPrefs
  where
    getFiles = do
      d <- FS.getDirectory path
      case FS.filter (not.isIgnored) [d] of
        [t] -> return $ FS.zipWithDest (,) "" t
        _   -> return []
      where isIgnored p = "Cache/" `isInfixOf` p
                          || "OfflineCache/" `isInfixOf` p
                          || "lock" `isSuffixOf` p
    
    userPref = path </> "prefs" <.> "js"
    getPrefs = HM.fromList <$> (parsePrefs =<< BS.readFile userPref)
    parsePrefs s = either (throwIO . ProfileParseError)
                          return
                   $ parseOnly prefsParser s

-- |Prepare a firefox profile for network transmission.
-- Internally, this function constructs a Firefox profile within a temp 
-- directory, archives it as a zip file, and then base64 encodes the zipped 
-- data. The temporary directory is deleted afterwards.
--
-- NOTE: because this function has to copy the profile files into a 
-- a temp directory before zip archiving them, this operation is likely to be slow
-- for large profiles. In such a case, consider using 'prepareLoadedProfile_' or 
-- 'prepareZippedProfile' instead.
prepareProfile :: MonadBaseControl IO m => 
                  Profile Firefox -> m (PreparedProfile Firefox)
prepareProfile Profile {profileFiles = files, profilePrefs = prefs} 
  = liftBase $ do 
      tmpdir <- mkTemp
      mapM_ (installPath tmpdir) files
      archive <- addUserPrefs <$> addFilesToArchive [OptRecursive] 
                                                    emptyArchive [tmpdir]
      removeDirectoryRecursive tmpdir
      return . prepareZipArchive $ archive
  where
    installPath tmp (src, d) = do
      let dest = tmp </> d
      isDir <- doesDirectoryExist src
      if isDir
        then createDirectoryIfMissing True dest `catch` ignoreIOException
        else do
          let dir = takeDirectory dest
          when (not . null $ dir) $
            createDirectoryIfMissing True dir `catch` ignoreIOException
          copyFile src dest `catch` ignoreIOException
    
    ignoreIOException :: IOException -> IO ()
    ignoreIOException = print 
    
    addUserPrefs = addEntryToArchive e
      where
        e = toEntry ("user" <.> "js") 0
            . LBS.concat
            . map (\(k, v) -> LBS.concat [ "user_pref(", encode k, 
                                           ", ", encode v, ");\n"]) 
            . HM.toList $ prefs  

-- |Apply a function on a default profile, and
-- prepare the result. The Profile passed to the handler function is
-- the default profile used by sessions when Nothing is specified
prepareTempProfile :: MonadBaseControl IO m => 
                     (Profile Firefox -> Profile Firefox) 
                     -> m (PreparedProfile Firefox)
prepareTempProfile f = prepareProfile . f $ defaultProfile

-- |Convenience function to load an existing Firefox profile from disk, apply
-- a handler function, and then prepare the result for network transmission.
--
-- NOTE: like 'prepareProfile', the same caveat about large profiles applies.
prepareLoadedProfile :: MonadBaseControl IO m =>
                        FilePath
                        -> (Profile Firefox -> Profile Firefox)
                        -> m (PreparedProfile Firefox)
prepareLoadedProfile path f = liftM f (loadProfile path) >>= prepareProfile
  
-- firefox prefs.js parser

prefsParser :: Parser [(Text, ProfilePref)]
prefsParser = many1 $ do 
  padSpaces $ string "user_pref("
  k <- prefKey <?> "preference key"
  padSpaces $ char ','
  v <- prefVal <?> "preference value"
  padSpaces $ string ");"
  return (k,v)
  where
    prefKey = jstring
    prefVal = do 
      v <- value'
      case fromJSON v of
        Error str -> fail str
        Success p -> return p
    
    padSpaces p = spaces >> p <* spaces
    spaces = many (endOfLine <|> void space <|> void comment)      
      where
        comment = inlineComment <|> lineComment
        lineComment = char '#' *> manyTill anyChar endOfLine
        inlineComment = string "/*" *> manyTill anyChar (string "*/")
        
        
mkTemp :: IO FilePath
mkTemp = do
  d <- getTemporaryDirectory
  createTempDirectory d ""