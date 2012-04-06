{-# LANGUAGE CPP, TypeSynonymInstances, OverloadedStrings,
             GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Test.WebDriver.Firefox.Profile 
       ( FirefoxProfile(..), PreparedFirefoxProfile
       , FirefoxPref(..), ToFirefox(..)
                          
       ,addPref, getPref, deletePref
       ,hasExtension, addExtension, deleteExtension
                          
       ,loadProfile, prepareProfile 
       ,prepareTempProfile, prepareLoadedProfile
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
import Control.Exception (Exception)
import Control.Exception.Lifted
import Data.Typeable

data FirefoxProfile = FirefoxProfile 
                      { profileDir    :: FilePath
                      , profileExts   :: HS.HashSet FilePath
                      , profilePrefs  :: HM.HashMap Text FirefoxPref
                      }
                    deriving (Eq, Show)

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


newtype PreparedFirefoxProfile = PreparedFirefoxProfile ByteString
  deriving (Eq, Show, ToJSON, FromJSON)


newtype ProfileParseError = ProfileParseError String
                          deriving  (Eq, Show, Read, Typeable)
instance Exception ProfileParseError

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

getPref :: Text -> FirefoxProfile -> Maybe FirefoxPref
getPref k (FirefoxProfile _ _ m) = HM.lookup k m

addPref :: ToFirefox a => Text -> a -> FirefoxProfile -> FirefoxProfile
addPref k v p = asMap p $ HM.insert k (toFirefox v)

deletePref :: Text -> FirefoxProfile -> FirefoxProfile
deletePref k p = asMap p $ HM.delete k

hasExtension :: FilePath -> FirefoxProfile -> Bool
hasExtension p (FirefoxProfile _ hs _) = p `HS.member` hs

addExtension :: FilePath -> FirefoxProfile -> FirefoxProfile
addExtension path p = asSet p $ HS.insert path

deleteExtension :: FilePath -> FirefoxProfile -> FirefoxProfile
deleteExtension path p = asSet p $ HS.delete path

asMap (FirefoxProfile p hs hm) f = FirefoxProfile p hs (f hm)

asSet (FirefoxProfile p hs hm) f = FirefoxProfile p (f hs) hm



tempProfile :: MonadIO m => m FirefoxProfile
tempProfile = liftIO $ defaultProfile <$> mkTemp

loadProfile :: MonadIO m => FilePath -> m FirefoxProfile
loadProfile path = liftIO $  FirefoxProfile 
                   <$> pure path <*> getExtensions <*> getPrefs
  where
    extD = path </> "extensions"
    userPref = path </> "prefs" <.> "js"
    getExtensions = HS.fromList . filter (`elem` [".",".."]) 
                    <$> getDirectoryContents extD
    getPrefs = HM.fromList <$> (parsePrefs =<< TIO.readFile userPref)
    
    parsePrefs s = either (throwIO . ProfileParseError)
                          return 
                   $ parseOnly prefsParser s

prepareProfile :: MonadIO m => FirefoxProfile -> m PreparedFirefoxProfile
prepareProfile FirefoxProfile {profileDir = d, profileExts = s, 
                               profilePrefs = m} 
  = liftIO $ do 
      createDirectoryIfMissing False extensionD
      forM_ (HS.toList s) installExtension
      withFile userPrefs WriteMode $ writeUserPrefs
      PreparedFirefoxProfile . B64.encode . SBS.concat . LBS.toChunks 
        . fromArchive 
        <$> addFilesToArchive [OptRecursive] emptyArchive [d]
        
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
  

prepareTempProfile :: MonadIO m => 
                     (FirefoxProfile -> FirefoxProfile) 
                     -> m PreparedFirefoxProfile
prepareTempProfile f = do 
  p <- liftM f tempProfile 
  p' <- prepareProfile p
  liftIO $ removeDirectoryRecursive (profileDir p)
  return p'
             
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