{-# LANGUAGE OverloadedStrings, RecordWildCards, ConstraintKinds, DataKinds, PolyKinds, TypeFamilies, 
             FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction,
             GADTs, TypeSynonymInstances, TemplateHaskell, FunctionalDependencies, TypeOperators, UndecidableInstances,
             TupleSections, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.WebDriver.Capabilities where

import Test.WebDriver.Firefox.Profile
import Test.WebDriver.Chrome.Extension
import Test.WebDriver.JSON

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch, Pair)
import qualified Data.HashMap.Strict as HM (keys)
import Text.Read (readMaybe)

import Data.Text as T (Text, toLower, toUpper, unpack, pack)
import Data.Default.Class (Default(..))
import Data.Word (Word16)
import Data.Maybe (isNothing)
import Data.String (IsString(..))
import Data.Char as C (toLower, toUpper)

import qualified Data.Typeable as T (Proxy(..))
import Data.Vinyl
import Data.Vinyl.TypeLevel
import qualified Data.Vinyl.Functor as F
import Data.Singletons
import Data.Singletons.TH (genSingletons)
import GHC.Exts (Constraint)

import Control.Monad
import Control.Applicative
import Control.Lens.TH
import Control.Exception.Lifted (throw)

import Prelude -- hides some "unused import" warnings

type Capabilities ckind fields = Rec (CapabilityField ckind) fields

data CapabilityKind = Requested | Resultant
  deriving (Eq, Ord, Bounded, Enum, Show, Read)

data CapabilityName =
    SpecificationLevel
  | Browser
  | Version -- |browser version (deprecated in w3c)
  | BrowserVersion
  | Platform
  | PlatformVersion
  | Proxy
  | JavascriptEnabled
  | TakesScreenshot
  | TakesElementScreenshot
  | HandlesAlerts
  | DatabaseEnabled
  | LocationContextEnabled
  | ApplicationCacheEnabled
  | BrowserConnectionEnabled
  | CSSSelectorsEnabled
  | WebStorageEnabled
  | Rotatable
  | AcceptSSLCerts
  | NativeEvents
  | UnexpectedAlertBehavior
  | RawCapability
  deriving (Eq, Ord, Bounded, Enum, Show, Read)

-- |List of fields common protocol fields
type CommonFields = 
  [ 'Browser
  , 'Platform
  , Proxy
  , AcceptSSLCerts
  , TakesScreenshot
  , RawCapability
  ]

-- |Fields used by the legacy wire protocol
type LegacyWireProtocol =
  CommonFields ++ 
 [ Version
 , JavascriptEnabled
 , HandlesAlerts
 , DatabaseEnabled
 , LocationContextEnabled
 , ApplicationCacheEnabled
 , BrowserConnectionEnabled
 , CSSSelectorsEnabled
 , WebStorageEnabled
 , Rotatable
 , NativeEvents
 , 'UnexpectedAlertBehavior
 ]

-- |Fields used by the W3C protocol specification
type W3C =
  CommonFields ++
  [ SpecificationLevel
  , BrowserVersion
  , PlatformVersion
  , AcceptSSLCerts
  , TakesScreenshot
  , TakesElementScreenshot
  ]

-- |Associates capability field names with their types
type family CapabilityFamily (name :: CapabilityName) where
  CapabilityFamily 'Browser                  = Browser
  CapabilityFamily BrowserVersion            = String
  CapabilityFamily 'Platform                 = Platform
  CapabilityFamily PlatformVersion           = String
  CapabilityFamily Proxy                     = ProxyType
  CapabilityFamily AcceptSSLCerts            = Bool
  CapabilityFamily TakesScreenshot           = Bool
  CapabilityFamily TakesElementScreenshot    = Bool
  CapabilityFamily RawCapability             = Value
  -- |legacy wire protocol fields below
  CapabilityFamily JavascriptEnabled         = Bool
  CapabilityFamily HandlesAlerts             = Bool
  CapabilityFamily DatabaseEnabled           = Bool
  CapabilityFamily LocationContextEnabled    = Bool
  CapabilityFamily ApplicationCacheEnabled   = Bool
  CapabilityFamily BrowserConnectionEnabled  = Bool
  CapabilityFamily CSSSelectorsEnabled       = Bool
  CapabilityFamily WebStorageEnabled         = Bool
  CapabilityFamily Rotatable                 = Bool
  CapabilityFamily NativeEvents              = Bool
  CapabilityFamily 'UnexpectedAlertBehavior  = UnexpectedAlertBehavior
  CapabilityFamily Version                   = String


data CapabilityField (ckind :: CapabilityKind) (name :: CapabilityName) =
 CapabilityKey ckind name := Capability ckind name
infix 4 :=

type family CapabilityKey (ckind :: CapabilityKind) (name :: CapabilityName) where
  CapabilityKey Requested RawCapability = Text
  CapabilityKey Requested name = Sing name
  CapabilityKey Resultant name = T.Proxy name

--newtype RawKey = RawKey Text deriving (IsString, ToJSON, FromJSON)

data Capability (ckind :: CapabilityKind) (name :: CapabilityName) where
  -- |The actual value of a capability server-side
  Actual :: CapabilityFamily name -> Capability Resultant name
  -- |A desired capability requested by the client
  Desired  :: CapabilityFamily name -> Capability Requested name
  -- |A required capability requested by the client
  Required :: CapabilityFamily name -> Capability Requested name
  -- |Unspecified capability. Assume default value.
  Unspecified :: Capability ckind name

capToMaybe :: Capability ckind name -> Maybe (CapabilityFamily name)
capToMaybe c = case c of
  Required v -> Just v
  Desired v -> Just v
  Actual v -> Just v
  Unspecified -> Nothing

isRequired :: Capability Requested name -> Bool
isRequired (Required _) = True
isRequired _ = False

isDesired :: Capability Requested name -> Bool
isDesired (Desired _) = True
isDesired _ = False

isActual :: Capability Resultant name -> Bool
isActual (Actual _) = True
isActual _ = False

isUnspecified :: Capability ckind name -> Bool
isUnspecified Unspecified = True
isUnspecified _ = False

requestedToResultant :: Capability Requested name -> Capability Resultant name
requestedToResultant c = case c of
  Required v -> Actual v
  Desired v -> Actual v
  Unspecified -> Unspecified

actualToRequired :: Capability Resultant name -> Capability Requested name
actualToRequired Unspecified = Unspecified
actualToRequired (Actual c) = Required c

actualToDesired :: Capability Resultant name -> Capability Requested name
actualToDesired Unspecified = Unspecified
actualToDesired (Actual c) = Desired c


-- Capability instances

instance Default (Capability ctype name) where
  def = Unspecified

instance ToJSON (CapabilityFamily name) => ToJSON (Capability ckind name) where
  toJSON = toJSON . capToMaybe

instance FromJSON (CapabilityFamily name) => FromJSON (Capability Resultant name) where
  parseJSON Null = return Unspecified
  parseJSON v = Actual <$> parseJSON v

-- Capabilities instances

instance (KeysHaveText names, CapsAreParseable names) => ToJSON (Capabilities Requested names) where
  toJSON caps = toJSON . object $ zip names values
    where
      names = getKeysAsText caps
      values = recordToList 
             . rmap (\(F.Compose (Dict v)) -> F.Const $ toJSON v)
             . reifyConstraint (T.Proxy :: T.Proxy ToJSON)
             $ getCapValues caps

class ParseCapabilities fs where
  parseField :: Value -> Parser (Capabilities Resultant fs)

instance ParseCapabilities '[] where
  parseField _ = return RNil

instance (FromJSON (CapabilityFamily f), ParseCapabilities fs) => ParseCapabilities (f ': fs) where
  parseField (Object o) = (:&) <$> parseFromKeys (HM.keys o) <*> parseField (Object o)
    where
      parseFromKeys = maybe (return (T.Proxy := Unspecified )) mkField . tryAllKeys
      mkField (key, name) = (T.Proxy :=) . Actual <$> (parseJSON =<< (o .:? key .!= Null))
      tryAllKeys = foldr mplus Nothing . map tryKey
      tryKey k = (k ,) <$> keyToCapName k

  parseField other = typeMismatch "Capabilities" other

instance ParseCapabilities fields => FromJSON (Capabilities Resultant fields) where
  parseJSON = parseField

{-
instance FromJSON [Capability Resultant AdditionalCapKeyValuePair] where
  parseJSON (Object o) = mkPairs . filterKnownKeys $ HM.keys o
    where
      filterKnownKeys = filter (isNothing . keyToCapName)
      mkPairs = mapM (\k -> Actual . (k ,) <$> o .: k)
  parseJSON other = typeMismatch "AdditionalCaps" other
-}

keyToCapName :: Text -> Maybe CapabilityName
keyToCapName = readMaybe . normalize . T.unpack
  where
    normalize s = case s of
      "cssSelectorEnabled" -> "CSSSelectorEnabled"
      "AcceptSslCerts" -> "AcceptSSLCerts"
      (c : cs) -> C.toUpper c : cs
      [] -> []


class KeyToText key where
  keyToText ::  key -> Text


instance KeyToText Text where
  keyToText  = id

instance KeyToText (Sing (name :: CapabilityName)) where
  keyToText  = fromString . normalize . show . fromSing
    where
      normalize s = case s of
       "CSSSelectorsEnabled" -> "cssSelectorsEnabled"
       "AcceptSSLCerts" -> "acceptSslCerts"
       (c : cs) -> C.toLower c : cs
       [] -> []

instance KeyToText (CapabilityKey ckind name) => KeyToText (CapabilityField ckind name) where
  keyToText (k := _) = keyToText k

{-
class GetCapsKeys names where
  getCapsKeys :: Capabilities Requested names -> [Text]

instance GetCapsKeys '[] where
  getCapsKeys RNil = []

instance GetCapsKeys ns => GetCapsKeys (RawCapability ': ns) where
  getCapsKeys ((k := _) :& fs) = k : getCapsKeys fs

instance (Show (CapabilityKey Requested n), GetCapsKeys ns) => GetCapsKeys (n ': ns) where
  getCapsKeys ((k := _) :& fs) = keyToText k : getCapsKeys fs
    where 
      keyToText = fromString . normalize . show
      normalize s = case s of
       "CSSSelectorsEnabled" -> "cssSelectorsEnabled"
       "AcceptSSLCerts" -> "acceptSslCerts"
       (c : cs) -> C.toLower c : cs
       [] -> []
-}

getKeysAsText :: KeysHaveText names => Capabilities Requested names -> [Text]
getKeysAsText = recordToList
              . getTextFromKeys

getCapValues :: Capabilities ctype names -> Rec (Capability ctype) names
getCapValues = rmap (\(_ := v) -> v)


getTextFromKeys :: KeysHaveText names => Capabilities Requested names -> Rec (F.Const Text) names
getTextFromKeys = rmap (\(F.Compose (Dict f)) -> F.Const $ keyToText f)
                  . reifyConstraint (T.Proxy :: T.Proxy KeyToText)


emptyCaps :: Capabilities ctype '[]
emptyCaps = RNil

type CapsAll ckind fields c = RecAll (Capability ckind) fields c

type FieldsAll ckind fields c = RecAll (CapabilityField ckind) fields c

type KeysHaveText names = FieldsAll Requested names KeyToText

type CapsAreParseable names = CapsAll Requested names ToJSON

type family CapabilityNamesOf t :: [CapabilityName]

class GetCapabilities t (ckind :: CapabilityKind) where
  getCaps :: t -> Capabilities ckind (CapabilityNamesOf t)

class SetCapabilities t (ckind :: CapabilityKind) where
  setCaps :: t -> Capabilities ckind (CapabilityNamesOf t) -> Capabilities ckind (CapabilityNamesOf t)

--instance (fields :: [CapabilityName]) ⊆ ('[] :: [CapabilityName]) where


-- |This constructor simultaneously specifies which browser the session will
-- use, while also providing browser-specific configuration. Default
-- configuration is provided for each browser by 'firefox', 'chrome', 'opera',
-- 'ie', etc.
--
-- This library uses 'firefox' as its 'Default' browser configuration, when no
-- browser choice is specified.
data Browser = 
               Firefox { -- |The firefox profile to use. If Nothing,
                         -- a default temporary profile is automatically created
                         -- and used.
                         ffProfile :: Maybe (PreparedProfile Firefox)
                         -- |Firefox logging preference
                       , ffLogPref :: LogLevel
                         -- |Server-side path to Firefox binary. If Nothing,
                         -- use a sensible system-based default.
                       , ffBinary :: Maybe FilePath
                       }
             | Chrome { -- |Version of the Chrome Webdriver server server to use
                        --
                        -- for more information on chromedriver see
                        -- <http://code.google.com/p/selenium/wiki/ChromeDriver>
                        chromeDriverVersion :: Maybe String
                        -- |Server-side path to Chrome binary. If Nothing,
                        -- use a sensible system-based default.
                      , chromeBinary :: Maybe FilePath
                        -- |A list of command-line options to pass to the
                        -- Chrome binary.
                      , chromeOptions :: [String]
                        -- |A list of extensions to use.
                      , chromeExtensions :: [ChromeExtension]
                      }
             | IE { -- |Whether to skip the protected mode check. If set, tests
                    -- may become flaky, unresponsive, or browsers may hang. If
                    -- not set, and protected mode settings are not the same for
                    -- all zones, an exception will be thrown on driver
                    -- construction.
                    ieIgnoreProtectedModeSettings :: Bool
                    -- |Indicates whether to skip the check that the browser's zoom
                    -- level is set to 100%. Value is set to false by default.
                  , ieIgnoreZoomSetting :: Bool
                    -- |Allows the user to specify the initial URL loaded when IE
                    -- starts. Intended to be used with ignoreProtectedModeSettings
                    -- to allow the user to initialize IE in the proper Protected Mode
                    -- zone. Using this capability may cause browser instability or
                    -- flaky and unresponsive code. Only \"best effort\" support is
                    -- provided when using this capability.
                  , ieInitialBrowserUrl :: Maybe Text
                    -- |Allows the user to specify whether elements are scrolled into
                    -- the viewport for interaction to align with the top or bottom
                    -- of the viewport. The default value is to align with the top of
                    -- the viewport.
                  , ieElementScrollBehavior :: IEElementScrollBehavior
                    -- |Determines whether persistent hovering is enabled (true by
                    -- default). Persistent hovering is achieved by continuously firing
                    -- mouse over events at the last location the mouse cursor has been
                    -- moved to.
                  , ieEnablePersistentHover :: Bool
                    -- |Determines whether the driver should attempt to remove obsolete
                    -- elements from the element cache on page navigation (true by
                    -- default). This is to help manage the IE driver's memory footprint
                    -- , removing references to invalid elements.
                  , ieEnableElementCacheCleanup :: Bool
                    -- |Determines whether to require that the IE window have focus
                    -- before performing any user interaction operations (mouse or
                    -- keyboard events). This capability is false by default, but
                    -- delivers much more accurate native events interactions.
                  , ieRequireWindowFocus :: Bool
                    -- |The timeout, in milliseconds, that the driver will attempt to
                    -- locate and attach to a newly opened instance of Internet Explorer
                    -- . The default is zero, which indicates waiting indefinitely.
                  , ieBrowserAttachTimeout :: Integer
                    -- |The path to file where server should write log messages to.
                    -- By default it writes to stdout.
                  , ieLogFile :: Maybe FilePath
                    -- |The log level used by the server. Defaults to 'IELogFatal'
                  , ieLogLevel :: IELogLevel
                    -- |The address of the host adapter on which the server will listen
                    -- for commands.
                  , ieHost :: Maybe Text
                    -- |The path to the directory used to extract supporting files used
                    -- by the server. Defaults to the TEMP directory if not specified.
                  , ieExtractPath :: Maybe Text
                    -- |Suppresses diagnostic output when the server is started.
                  , ieSilent :: Bool
                    -- |Forces launching Internet Explorer using the CreateProcess API.
                    -- If this option is not specified, IE is launched using the
                    -- IELaunchURL, if it is available. For IE 8 and above, this option
                    -- requires the TabProcGrowth registry value to be set to 0.
                  , ieForceCreateProcess :: Bool
                    -- |Specifies command-line switches with which to launch Internet
                    -- Explorer. This is only valid when used with the
                    -- forceCreateProcess.
                  , ieSwitches :: Maybe Text
                  }
             | Opera { -- |Server-side path to the Opera binary
                       operaBinary    :: Maybe FilePath
                     --, operaNoRestart :: Maybe Bool
                       -- |Which Opera product we're using, e.g. \"desktop\",
                       -- \"core\"
                     , operaProduct   :: Maybe String
                       -- |Whether the Opera instance should stay open after
                       -- we close the session. If false, closing the session
                       -- closes the browser.
                     , operaDetach    :: Bool
                       -- |Whether to auto-start the Opera binary. If false,
                       -- OperaDriver will wait for a connection from the
                       -- browser. By default this is True.
                     , operaAutoStart :: Bool
                       -- |Whether to use Opera's alternative implicit wait
                       -- implementation. It will use an in-browser heuristic
                       -- to guess when a page has finished loading. This
                       -- feature is experimental, and disabled by default.
                     , operaIdle      :: Bool
                       -- |(*nix only) which X display to use.
                     , operaDisplay   :: Maybe Int
                     --, operaProfile   :: Maybe (PreparedProfile Opera)
                       -- |Path to the launcher binary to use. The launcher
                       -- is a gateway between OperaDriver and the Opera
                       -- browser. If Nothing, OperaDriver will use the
                       -- launcher supplied with the package.
                     , operaLauncher  :: Maybe FilePath
                       -- |The port we should use to connect to Opera. If Just 0
                       -- , use a random port. If Nothing, use the default
                       -- Opera port. The default 'opera' constructor uses
                       -- Just 0, since Nothing is likely to cause "address
                       -- already in use" errors.
                     , operaPort      :: Maybe Word16
                       -- |The host Opera should connect to. Unless you're
                       -- starting Opera manually you won't need this.
                     , operaHost      :: Maybe String
                       -- |Command-line arguments to pass to Opera.
                     , operaOptions   :: Maybe String
                       -- |Where to send the log output. If Nothing, logging is
                       -- disabled.
                     , operaLogFile   :: Maybe FilePath
                       -- |Log level preference. Defaults to 'LogInfo'
                     , operaLogPref   :: LogLevel
                     }
             | HTMLUnit
             | IPhone
             | IPad
             | Android
             -- |some other browser, specified by a string name
             | OtherBrowser Text
             deriving (Eq, Show)

instance Default Browser where
  def = firefox


instance ToJSON Browser where
  toJSON Firefox {} = String "firefox"
  toJSON Chrome {} = String "chrome"
  toJSON Opera {} = String "opera"
  toJSON IE {} = String "internet explorer"
  toJSON (OtherBrowser b) = String b
  toJSON b = String . T.toLower . fromString . show $ b

instance FromJSON Browser where
  parseJSON (String jStr) = case T.toLower jStr of
    "firefox"           -> return firefox
    "chrome"            -> return chrome
    "internet explorer" -> return ie
    "opera"             -> return opera
    -- "safari"            -> return safari
    "iphone"            -> return iPhone
    "ipad"              -> return iPad
    "android"           -> return android
    "htmlunit"          -> return htmlUnit
    other               -> return (OtherBrowser other)
  parseJSON v = typeMismatch "Browser" v


-- |Default Firefox settings. All Maybe fields are set to Nothing. ffLogPref
-- is set to 'LogInfo'.
firefox :: Browser
firefox = Firefox Nothing def Nothing

-- |Default Chrome settings. All Maybe fields are set to Nothing, no options are
-- specified, and no extensions are used.
chrome :: Browser
chrome = Chrome Nothing Nothing [] []

-- |Default IE settings. See the 'IE' constructor for more details on
-- individual defaults
ie :: Browser
ie = IE { ieIgnoreProtectedModeSettings = True
        , ieIgnoreZoomSetting = False
        , ieInitialBrowserUrl = Nothing
        , ieElementScrollBehavior = def
        , ieEnablePersistentHover = True
        , ieEnableElementCacheCleanup = True
        , ieRequireWindowFocus = False
        , ieBrowserAttachTimeout = 0
        , ieLogFile = Nothing
        , ieLogLevel = def
        , ieHost = Nothing
        , ieExtractPath = Nothing
        , ieSilent = False
        , ieForceCreateProcess = False
        , ieSwitches = Nothing
        }

-- |Default Opera settings. See the 'Opera' constructor for more details on
-- individual defaults.
opera :: Browser
opera = Opera { operaBinary = Nothing
              --, operaNoRestart = Nothing
              , operaProduct = Nothing
              , operaDetach = False
              , operaAutoStart = True
              , operaDisplay = Nothing
              , operaIdle = False
--              , operaProfile = Nothing
              , operaLauncher = Nothing
              , operaHost = Nothing
              , operaPort = Just 0
              , operaOptions = Nothing
              , operaLogFile = Nothing
              , operaLogPref = def
              }

--safari :: Browser
--safari = Safari

htmlUnit :: Browser
htmlUnit = HTMLUnit

iPhone :: Browser
iPhone = IPhone

iPad :: Browser
iPad = IPad

android :: Browser
android = Android

-- |Represents platform options supported by WebDriver. The value Any represents
-- no preference.
data Platform = Windows | XP | Vista | Mac | Linux | Unix | Any | OtherPlatform Text
              deriving (Eq, Show, Ord)

instance ToJSON Platform where
  toJSON (OtherPlatform txt) = String txt
  toJSON p = String . T.toUpper . fromString . show $ p

instance FromJSON Platform where
  parseJSON (String jStr) = case T.toLower jStr of
    "windows" -> return Windows
    "xp"      -> return XP
    "vista"   -> return Vista
    "mac"     -> return Mac
    "linux"   -> return Linux
    "unix"    -> return Unix
    "any"     -> return Any
    other     -> return $ OtherPlatform other
  parseJSON v = typeMismatch "Platform" v

-- |Available settings for the proxy 'Capabilities' field
data ProxyType = NoProxy
               | UseSystemSettings
               | AutoDetect
                 -- |Use a proxy auto-config file specified by URL
               | PAC { autoConfigUrl :: String }
                 -- |Manually specify proxy hosts as hostname:port strings.
                 -- Note that behavior is undefined for empty strings.
               | Manual { ftpProxy  :: String
                        , sslProxy  :: String
                        , httpProxy :: String
                        }
               deriving (Eq, Show)

instance FromJSON ProxyType where
  parseJSON (Object obj) = do
    pTyp <- f "proxyType"
    case T.toLower pTyp of
      "direct" -> return NoProxy
      "system" -> return UseSystemSettings
      "pac"    -> PAC <$> f "autoConfigUrl"
      "manual" -> Manual <$> f "ftpProxy"
                         <*> f "sslProxy"
                         <*> f "httpProxy"
      _ -> fail $ "Invalid ProxyType " ++ show pTyp
    where
      f :: FromJSON a => Text -> Parser a
      f = (obj .:)
  parseJSON v = typeMismatch "ProxyType" v

instance ToJSON ProxyType where
  toJSON pt = object $ case pt of
    NoProxy ->
      ["proxyType" .= ("DIRECT" :: String)]
    UseSystemSettings ->
      ["proxyType" .= ("SYSTEM" :: String)]
    AutoDetect ->
      ["proxyType" .= ("AUTODETECT" :: String)]
    PAC{autoConfigUrl = url} ->
      ["proxyType" .= ("PAC" :: String)
      ,"autoConfigUrl" .= url
      ]
    Manual{ftpProxy = ftp, sslProxy = ssl, httpProxy = http} ->
      ["proxyType" .= ("MANUAL" :: String)
      ,"ftpProxy"  .= ftp
      ,"sslProxy"  .= ssl
      ,"httpProxy" .= http
      ]

data UnexpectedAlertBehavior = AcceptAlert | DismissAlert | IgnoreAlert
                              deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance ToJSON UnexpectedAlertBehavior where
  toJSON AcceptAlert  = String "accept"
  toJSON DismissAlert = String "dismiss"
  toJSON IgnoreAlert  = String "ignore"

instance FromJSON UnexpectedAlertBehavior where
  parseJSON (String s) =
    return $ case s of
      "accept"  -> AcceptAlert
      "dismiss" -> DismissAlert
      "ignore"  -> IgnoreAlert
      err       -> throw . BadJSON
                   $ "Invalid string value for UnexpectedAlertBehavior: " ++ show err
  parseJSON v = typeMismatch "UnexpectedAlertBehavior" v

-- |Indicates a log verbosity level. Used in 'Firefox' and 'Opera' configuration.
data LogLevel = LogOff | LogSevere | LogWarning | LogInfo | LogConfig
              | LogFine | LogFiner | LogFinest | LogAll
             deriving (Eq, Show, Read, Ord, Bounded, Enum)

instance Default LogLevel where
  def = LogInfo

instance ToJSON LogLevel where
  toJSON p= String $ case p of
    LogOff -> "OFF"
    LogSevere -> "SEVERE"
    LogWarning -> "WARNING"
    LogInfo -> "INFO"
    LogConfig -> "CONFIG"
    LogFine -> "FINE"
    LogFiner -> "FINER"
    LogFinest -> "FINEST"
    LogAll -> "ALL"

instance FromJSON LogLevel where
  parseJSON (String s) = return $ case s of
    "OFF" -> LogOff
    "SEVERE" -> LogSevere
    "WARNING" -> LogWarning
    "INFO" -> LogInfo
    "CONFIG" -> LogConfig
    "FINE" -> LogFine
    "FINER" -> LogFiner
    "FINEST" -> LogFinest
    "ALL" -> LogAll
    _ -> throw . BadJSON $ "Invalid logging preference: " ++ show s
  parseJSON other = typeMismatch "LogLevel" other


-- |Logging levels for Internet Explorer
data IELogLevel = IELogTrace | IELogDebug | IELogInfo | IELogWarn | IELogError
                | IELogFatal
                deriving (Eq, Show, Read, Ord, Bounded, Enum)

instance Default IELogLevel where
  def = IELogFatal


instance ToJSON IELogLevel where
  toJSON p= String $ case p of
    IELogTrace -> "TRACE"
    IELogDebug -> "DEBUG"
    IELogInfo -> "INFO"
    IELogWarn -> "WARN"
    IELogError -> "ERROR"
    IELogFatal -> "FATAL"

instance FromJSON IELogLevel where
  parseJSON (String s) = return $ case s of
    "TRACE" -> IELogTrace
    "DEBIG" -> IELogDebug
    "INFO"  -> IELogInfo
    "WARN"  -> IELogWarn
    "ERROR" -> IELogError
    "FATAL" -> IELogFatal
    _ -> throw . BadJSON $ "Invalid logging preference: " ++ show s
  parseJSON other = typeMismatch "IELogLevel" other

-- |Specifies how elements scroll into the viewport. (see 'ieElementScrollBehavior')
data IEElementScrollBehavior = AlignTop | AlignBottom
                             deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Default IEElementScrollBehavior where
  def = AlignTop

instance ToJSON IEElementScrollBehavior where
  toJSON AlignTop    = toJSON (0 :: Int)
  toJSON AlignBottom = toJSON (1 :: Int)

instance FromJSON IEElementScrollBehavior where
  parseJSON v = do
    n <- parseJSON v
    case n :: Integer of
      0 -> return AlignTop
      1 -> return AlignBottom
      _ -> fail $ "Invalid integer for IEElementScrollBehavior: " ++ show n

genSingletons([''CapabilityName])
--makeLenses ''CapabilityField

specificationLevel = SSpecificationLevel
browser = SBrowser 
version = SVersion 
browserVersion = SBrowserVersion 
platform = SPlatform 
platformVersion = SPlatformVersion 
proxy = SProxy 
acceptSSLCerts = SAcceptSSLCerts 
takesScreenshot = STakesScreenshot 
takesElementScreenshot = STakesElementScreenshot 
javascriptEnabled = SJavascriptEnabled 
handlesAlerts = SHandlesAlerts 
databaseEnabled = SDatabaseEnabled 
locationContextEnabled = SLocationContextEnabled 
applicationCacheEnabled = SApplicationCacheEnabled 
browserConnectionEnabled = SBrowserConnectionEnabled 
cssSelectorsEnabled = SCSSSelectorsEnabled 
webStorageEnabled = SWebStorageEnabled 
rotatable = SRotatable 
nativeEvents = SNativeEvents 
unexpectedAlertBehavior = SUnexpectedAlertBehavior
