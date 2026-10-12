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
  , parseFirefoxPrefs
  ) where

import Codec.Archive.Zip
import Control.Applicative
import Control.Exception.Safe hiding (try)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (Result(..), encode, fromJSON)
import qualified Data.Aeson as A
import Data.Aeson.Types (FromJSON, ToJSON, Value(..), parseJSON, toJSON, typeMismatch)
import Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (chr, digitToInt, isHexDigit)
import Data.Fixed
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.Maybe
import Data.Ratio
import Data.Text (Text, pack, singleton)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Word
import Prelude -- hides some "unused import" warnings
import System.Directory
import System.FilePath ((</>), (<.>))

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


-- | The files Firefox reads preferences from, in the order it reads them: later files
-- override earlier ones.
firefoxPrefFiles :: [FilePath]
firefoxPrefFiles = ["prefs.js", "user.js"]

-- | Load an existing profile from the file system. Any prepared changes made to
-- the 'Profile' will have no effect to the profile on disk.
--
-- Preferences are read from the profile's @prefs.js@ and @user.js@ (the latter taking
-- precedence, as in Firefox) and merged over 'defaultFirefoxProfile', so that preferences
-- found on disk override the defaults. Every other regular file below the directory is
-- stored under its path relative to the directory, except for lock files and cache
-- directories.
--
-- Throws an 'IOException' if the directory can't be read and a 'ProfileParseError' if a
-- preferences file can't be parsed.
loadFirefoxProfile :: MonadIO m => FilePath -> m (Profile Firefox)
loadFirefoxProfile dir = liftIO $ do
  paths <- listFiles ""
  -- Files are read one at a time and strictly, so the number of open handles stays bounded.
  files <- forM (filter (`notElem` firefoxPrefFiles) paths) $ \rel -> do
    bytes <- BS.readFile (dir </> rel)
    return (rel, BL.fromStrict bytes)
  prefs <- foldM readPrefs HM.empty (filter (`elem` paths) firefoxPrefFiles)
  return $ Profile (HM.fromList files) prefs `unionProfiles` defaultFirefoxProfile
  where
    -- Regular files below the profile directory, as paths relative to it.
    listFiles rel = do
      names <- listDirectory (dir </> rel)
      fmap concat $ forM (filter (`notElem` ignored) names) $ \name -> do
        let rel' = rel </> name
        isDir <- doesDirectoryExist (dir </> rel')
        if isDir then listFiles rel' else do
          isFile <- doesFileExist (dir </> rel')
          return [rel' | isFile]

    ignored = ["parent.lock", ".parentlock", "lock", ".lock", "Cache", "OfflineCache"]

    readPrefs acc name = do
      let path = dir </> name
      ps <- either throwIO return . parseFirefoxPrefs path =<< BS.readFile path
      -- HM.union is left-biased, so this file's preferences win over the earlier ones.
      return (ps `HM.union` acc)

-- | Save a Firefox profile to a destination directory. This directory should
-- already exist.
saveFirefoxProfile :: MonadIO m => Profile Firefox -> FilePath -> m ()
saveFirefoxProfile (firefoxProfileToArchive -> archive) dest = liftIO $ flip extractFilesFromArchive archive [
  OptRecursive
  , OptDestination dest
  ]

-- | Prepare a Firefox profile for network transmission. The preferences are written to a
-- @user.js@ entry, which Firefox reads after @prefs.js@.
firefoxProfileToArchive :: Profile Firefox -> Archive
firefoxProfileToArchive (Profile {profileFiles = files, profilePrefs = prefs}) =
  foldl addProfileFile baseArchive (HM.toList files)

  where
    baseArchive = emptyArchive
                & addEntryToArchive (toEntry ("user" <.> "js") 0 userJsContents)

    userJsContents = prefs
                   & HM.toList
                   & map (\(k, v) -> LBS.concat [ "user_pref(", quoted k, ", ", value v, ");\n"])
                   & LBS.concat

    value (PrefString s) = quoted s
    value v = encode v

    -- Quoted the way Firefox's own prefs writer does it: only \n, \r, \\ and \" are escaped.
    -- aeson's encoding would also produce \t and \uXXXX, which Firefox's parser rejects.
    quoted s = LBS.concat ["\"", TL.encodeUtf8 (TL.concatMap escape (TL.fromStrict s)), "\""]
      where
        escape '\n' = "\\n"
        escape '\r' = "\\r"
        escape '\\' = "\\\\"
        escape '"' = "\\\""
        escape c = TL.singleton c

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
      Right archive -> do
        prefs <- foldM (readPrefs archive) HM.empty firefoxPrefFiles
        return $ Profile {
          profileFiles = HM.fromList [(eRelativePath, fromEntry e) | e@(Entry {..}) <- zEntries archive, eRelativePath `notElem` firefoxPrefFiles]
          , profilePrefs = prefs
          }
    where
      readPrefs archive acc name = case findEntryByPath name archive of
        Nothing -> return acc
        Just entry -> case parseFirefoxPrefs name (BL.toStrict (fromEntry entry)) of
          Left (ProfileParseError err) -> fail err
          Right ps -> return (ps `HM.union` acc)
  parseJSON other = typeMismatch "Profile Firefox" other

-- * Firefox prefs.js parser

-- | Parse the contents of a Firefox preferences file (@prefs.js@ or @user.js@): a sequence of
-- @user_pref("name", value);@ statements, with @\/\/@ and @#@ line comments and C-style block
-- comments allowed between them. Later entries for the same name override earlier ones, as in
-- Firefox.
--
-- The 'FilePath' is only used to label error messages.
parseFirefoxPrefs :: FilePath -> BS.ByteString -> Either ProfileParseError (HM.HashMap Text ProfilePref)
parseFirefoxPrefs path contents = case parseComplete prefsParser contents of
  AP.Done _ prefs -> Right (HM.fromList prefs)
  AP.Fail rest ctxs msg -> Left $ ProfileParseError $ mconcat [
    path, ":", show (lineNumber rest), ": prefs parse error: "
    -- Labels are outermost first, so the head is the one attached to the construct being parsed.
    , case ctxs of { (ctx:_) -> ctx; [] -> msg }
    , " near ", show (BS8.unpack (BS8.take snippetLength (BS8.takeWhile (\c -> c /= '\n' && c /= '\r') rest)))
    ]
  AP.Partial _ -> Left $ ProfileParseError (path <> ": prefs parse error: unexpected end of input")
  where
    -- Pref lines can be thousands of characters long (JSON blobs), so cap the quoted snippet.
    snippetLength = 60

    -- Like `parseOnly`, but keeps the unconsumed input for the error's line number and
    -- snippet. Feeding an empty chunk marks the end of input.
    parseComplete p s = AP.parse p s `AP.feed` BS.empty

    lineNumber rest = 1 + BS8.count '\n' (BS.take (BS.length contents - BS.length rest) contents)

prefsParser :: AP.Parser [(Text, ProfilePref)]
prefsParser = skipSpaceAndComments *> statements
  where
    -- Explicit recursion instead of `many statement <* AP.endOfInput` to avoid unhelpful
    -- "end of input" error message when parsing a statement fails
    statements = (AP.endOfInput *> pure [])
             <|> ((:) <$> statement <*> statements)

    statement = do
      symbol "user_pref"
      symbol "("
      k <- lexeme prefString <?> "expected quoted preference name"
      symbol ","
      v <- lexeme prefValue <?> "expected preference value (string, integer or boolean)"
      symbol ")"
      symbol ";"
      return (k, v)

    -- A token and the space and comments after it.
    lexeme p = p <* skipSpaceAndComments
    symbol s = void (lexeme (AP.string s)) <?> ("expected " <> BS8.unpack s)

    -- Firefox's grammar: a string, true, false, or a signed integer. Decimals are accepted too,
    -- since that is how firefoxProfileToArchive writes a PrefDouble.
    prefValue = AP.choice [
      PrefString <$> prefString
      , PrefBool True <$ AP.string "true"
      , PrefBool False <$ AP.string "false"
      , AP.scientific >>= numberPref
      ]
      where
        numberPref s = case fromJSON (Number s) of
          Success p -> return p
          Error err -> fail err

    -- A string literal in Firefox's grammar: single or double quoted, with the escapes
    -- \" \' \\ \n \r, \xNN for a raw byte and \uNNNN for a UTF-16 code unit.
    prefString = do
      quote <- AP.satisfy (\c -> c == '"' || c == '\'')
      pieces <- AP.manyTill (plain quote <|> escaped) (AP.char quote)
      either (const (fail "string is not valid UTF-8")) return (T.decodeUtf8' (BS.concat pieces))
      where
        plain quote = AP.takeWhile1 (\c -> c /= quote && c /= '\\' && c /= '\n' && c /= '\r')
        escaped = AP.char '\\' *> AP.choice [
          "\"" <$ AP.char '"'
          , "'" <$ AP.char '\''
          , "\\" <$ AP.char '\\'
          , "\n" <$ AP.char 'n'
          , "\r" <$ AP.char 'r'
          , BS.singleton . fromIntegral <$> (AP.char 'x' *> hex 2)
          , T.encodeUtf8 . singleton . chr <$> (AP.char 'u' *> (hex 4 >>= codeUnit))
          ]
        codeUnit u
          | isHighSurrogate u = do
              l <- AP.string "\\u" *> hex 4
              if isLowSurrogate l
                then return (0x10000 + (u - 0xD800) * 0x400 + (l - 0xDC00))
                else fail "expected low surrogate after high surrogate"
          | isLowSurrogate u = fail "expected high surrogate before low surrogate"
          | otherwise = return u
        isHighSurrogate u = u >= 0xD800 && u <= 0xDBFF
        isLowSurrogate u = u >= 0xDC00 && u <= 0xDFFF
        hex n = foldl (\acc d -> acc * 16 + digitToInt d) 0 <$> AP.count n (AP.satisfy isHexDigit)

    skipSpaceAndComments = AP.skipMany (void (AP.takeWhile1 AP.isSpace) <|> comment)
    comment = lineComment "//" <|> lineComment "#" <|> blockComment
    lineComment start = AP.string start *> AP.skipWhile (\c -> c /= '\n' && c /= '\r')
    -- Both delimiters must stay on one line: this module is preprocessed by CPP.
    blockComment = (AP.string "/*" *> void (AP.manyTill AP.anyChar (AP.string "*/"))) <?> "unterminated block comment"
