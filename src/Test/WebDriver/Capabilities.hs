{-# LANGUAGE OverloadedStrings, RecordWildCards, ConstraintKinds, DataKinds, PolyKinds, TypeFamilies, 
             FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction,
             GADTs, TypeSynonymInstances, TemplateHaskell, FunctionalDependencies, TypeOperators, UndecidableInstances,
             TupleSections, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{- | For advanced session configuration, the webdriver protocol specifies so-called capabilities, which allow you to describe desired/required configuration parameters.

  Encoding webdriver capabilities into staticly typed data constructors and records while also providing JSON serializing and parsing is a rather tedious effort. 
  Previously hs-webdriver verisons used vanilla Haskell 98 records, but as the webdriver protocol changed and with the advent of W3C WebDriver and Selenium 4.0, 
  it quickly became evident that vanilla records were not ideal for working with the demands of simultaneously providing backwards compatibility over a range of protocol versions
  and doing so in a flexible yet type-safe manner.

  We now use the very powerful vinyl records library to work with capabilities. This allows us to not only support differing versions of the WebDriver protocol easily,
  but it allows us to make stronger static guarantees about the consistency of capabilities, thanks to vinyl's structural typing characteristics.

  The trade-off to this switch is that to a Haskell beginner vinyl can seem daunting due to the vast number of type-level definitions and GHC type extensions, but
  the end result is rather simple to understand:

-} 
module Test.WebDriver.Capabilities 
  ( -- * Capabilities: a vinyl record type
    Capabilities, nullCaps
    -- ** Type-level tags (using GHC DataKinds extension) 
    -- |Most capability-related types accept two phantom type parameters of kind 'CapabilityKind' and 'CapabilityName' (or a list of these 'CapabilityName' types)
    --
    -- 'CapabilityKind' is documented here because it is small and helpful in understanding the API.
    --
    --  'CapabilityName', however, is very verbose not something you interact with directly in most cases, so it is documented further down.
  , CapabilityKind(..)
    -- * CapabilityField: a key-value pair
  , CapabilityField(..), (=:), (&), KeySpecifier(..)
    -- * Capability keys
  , CapabilityKey
    -- ** Key specifier values
    -- |Instead of working with 'CapabilityKey' directly, you typically want to use these singleton types
    -- which have a 'KeySpecifier' instance associated with them.
  , specificationLevel, browser, browserVersion, version,
    -- * CapabilityFamily: A type family from fields names to values
  , CapabilityFamily
    -- * Capability values
  , Capability(..)
    -- ** Capability predicates
  , isRequired, isDesired, isActual, isUnspecified
    -- ** Capability constructor conversions
  , actualToRequired, actualToDesired, requestedToResultant, forceRequired, forceDesired, splitRequestedCaps
    -- ** Conversion to other types
  , capToMaybe, requestedCapToEither
    -- * Browser-specific capabilities
  , BrowserType(..)
    -- ** Browser defaults
  , firefox, chrome, ie, opera, iPhone, iPad, android
    -- ** Other browser capability types
  , PlatformType(..), ProxyType(..), UnexpectedAlertBehavior(..), LogLevel(..), IELogLevel(..), IEElementScrollBehavior(..)
    -- * CapabilityName: a type-level field name tag
  , CapabilityName(..), 
    -- ** CapabilityName lists
  , CommonFields, W3C, LegacyWireProtocol,
    -- * Utilities
    -- ** Capabilities processing
  , splitRequestedCaps
    -- ** JSON parsing
  , keyToText, getKeysAsText, getTextFromKeys, getCapValues
    -- * vinyl re-exports and related functions
    -- ** lens functions
      rget, rput, rsubset, rcast, rreplace
    -- ** record manipulation 
  , Rec(..), rappend, (<+>), rmap, (<<$>>), (<<&&>>), recordToList
    -- ** Type-level logic
    (∈), (⊆), (≅), (<:), (:~:), (++), RecAll, reifyConstraint
    -- ** Additional type-level helpers for Capabilities
  , CapsAll, FieldsAll, KeysHaveText, CapsAreParseable
  ) where

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
import Data.Maybe (isNothing, catMaybes)
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

-- |A heterogeneously typed collection holding WebDriver Capabilities indexed by type-level keys.
--
-- This is a convenient type synonym for the underling vinyl record.
type Capabilities ckind names = Rec (CapabilityField ckind) names

-- |A data kind used to tag the 'Capabilities' record and all constituent key/value types, indicating a purpose in the webdriver protocol.
--
-- A 'Requested' tag means the record holds 'Required'/'Desired' capabilities of the client-side session.
--
-- A 'Resultant' tag means the record holds 'Actual' capabilities of a server-side session.
data CapabilityKind = Requested | Resultant
  deriving (Eq, Ord, Bounded, Enum, Show, Read)

-- |A data kind representing type-level tags for each possible record field 
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


-- |List of fields that are common to all webdriver protocols
type CommonFields =
  [ 'Browser
  , 'Platform
  , Proxy
  , AcceptSSLCerts
  , TakesScreenshot
  , RawCapability
  ]
-- |List of fields used by the legacy wire protocol (deprcated by the 'W3C' spec)
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

-- |Fields used by the W3C webdriver protocol (available in Selenium versions >= 4.0)
type W3C =
  CommonFields ++
  [ SpecificationLevel
  , BrowserVersion
  , PlatformVersion
  , AcceptSSLCerts
  , TakesElementScreenshot
  ]

-- |This type family provides a mapping between capability field names and the types of their associated values.
type family CapabilityFamily (name :: CapabilityName) where
  CapabilityFamily Browser                   = BrowserType
  CapabilityFamily BrowserVersion            = String
  CapabilityFamily Platform                  = PlatformType
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


-- |A 'Capabilities' record is a heterogeneous collection
-- of these tagged key-value pairs.
data CapabilityField (ckind :: CapabilityKind) (name :: CapabilityName) =
 CapabilityField (CapabilityKey ckind name) (Capability ckind name)

-- |Provides information about the key of a record field. This is an opaque type
-- and not intended to be worked with directly in most cases.
data CapabilityKey (ckind :: CapabilityKind) (name :: CapabilityName) where
  RawKey :: Text -> CapabilityKey ckind RawCapability
  RequestKey :: Sing name -> CapabilityKey Requested name
  ResultKey :: CapabilityKey Resultant name 

-- |The value of a capability, tagged by two data kinds. The set of permitted constructors depends
-- on the 'CapabilityKind' tag, while the constructor argument's type depends
-- on the 'CapabilityName' tag.
--
-- In short: the 'Requested' tag permits the 'Required' and 'Desired' constructors,
-- while the 'Resultant' tag permits the 'Actual' constructor.
-- In either case, the 'Unspecified' constructor is always allowed.
--
-- To see how the 'CapabilityName' tag resolves the input argument, refer to
-- the definition of 'CapabilityFamily'.
data Capability (ckind :: CapabilityKind) (name :: CapabilityName) where
  -- |The actual value of a capability server-side
  Actual :: CapabilityFamily name -> Capability Resultant name
  -- |A desired capability requested by the client
  Desired  :: CapabilityFamily name -> Capability Requested name
  -- |A required capability requested by the client
  Required :: CapabilityFamily name -> Capability Requested name
  -- |Unspecified capability. Assume default value.
  Unspecified :: Capability ckind name

-- |Converts a capability into a 'Maybe' value.
capToMaybe :: Capability ckind name -> Maybe (CapabilityFamily name)
capToMaybe c = case c of
  Required v -> Just v
  Desired v -> Just v
  Actual v -> Just v
  Unspecified -> Nothing

-- |Convert a 'Requested' capability into a 'Maybe' 'Either'. 'Left' indicates 'Required', 'Right' indicates 'Desired'
requestedCapToEither :: Capability Requested name -> Either (CapabilityFamily name) (CapabilityFamily name)
requestedCapToEither c = case c of
  Required v -> Just (Left v)
  Desired v -> Just (Right v)
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

-- splits a requested capability record into (required, desired). Each subrecord has the same number of fields, but fields
-- that were not the correct required/desired constructor are converted to 'Unspecified'
splitRequestedCaps :: Capabilities Requested names -> (Capabilities Requested names, Capabilities Requested names)
splitRequestedCaps RNil = (RNil, RNil)
splitRequestedCaps (f@(CapabilityField k v) :& cs) = (r :& otherRequired, d :& otherDesired)
  where
    (r, d) = case v of
      Required _ -> (f, noF)
      Desired  _ -> (noF, f)
      Unspecified -> (noF, noF)
    noF = CapabilityField k Unspecified
    (otherRequired, otherDesired) = splitRequestedCaps cs

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

-- |Forces an existing requested capability to be 'Required' if it was previously 'Desired'. Has no effect otherwise.
forceRequired :: Capability Requested name -> Capability Requested name
forceRequired (Desired v) = Required v
forceRequired c = c

forceDesired :: Capability Requested name -> Capability Requested name
forceDesired (Required v) = Desired v
forceDesired c = c

keyToCapName :: Text -> Maybe CapabilityName
keyToCapName = readMaybe . normalize . T.unpack
  where
    normalize s = case s of
      "browserName" -> "Browser"
      "cssSelectorEnabled" -> "CSSSelectorEnabled"
      "AcceptSslCerts" -> "AcceptSSLCerts"
      (c : cs) -> C.toUpper c : cs
      [] -> []

class KeyToText key where
  -- |Converts a CapabilityKey or CapabilityField to a 'Text' value indicating its JSON key name.
  --
  -- Caveat: this operation is not always possible, in particular when dealing with 'Resultant' capabilities.
  -- You will get a compile error (undefined instance) when attempting to 
  -- call this function on keys where the operation is not permitted. The complexities of
  -- type-level programming make defining this function for all keys difficult, but the eventual
  -- goal is to support this in all cases.
  --
  -- It SHOULD work in all 'Requested' capabilities cases. If it does not this is an oversight and you should report a bug.
  keyToText ::  key -> Text

instance KeyToText (CapabilityKey Requested name) where
  keyToText (RawKey t) = t
  keyToText (RequestKey k) = fromString . normalize . show . fromSing $ k
    where
      normalize s = case s of
        "Browser" -> "browserName"
        "CSSSelectorsEnabled" -> "cssSelectorsEnabled"
        "AcceptSSLCerts" -> "acceptSslCerts"
        (c : cs) -> C.toLower c : cs
        [] -> []

instance KeyToText (CapabilityKey Resultant RawCapability)
  keyToText (RawKey t) = t

instance KeyToText (CapabilityKey ckind name) => KeyToText (CapabilityField ckind name) where
  keyToText (CapabilityField k _) = keyToText k

getKeysAsText :: KeysHaveText names => Capabilities Requested names -> [Text]
getKeysAsText = recordToList
              . getTextFromKeys

getCapValues :: Capabilities ctype names -> Rec (Capability ctype) names
getCapValues = rmap (\(CapabilityField _ v) -> v)

getTextFromKeys :: KeysHaveText names => Capabilities Requested names -> Rec (F.Const Text) names
getTextFromKeys = rmap (\(F.Compose (Dict f)) -> F.Const $ keyToText f)
                  . reifyConstraint (T.Proxy :: T.Proxy KeyToText)

emptyCaps :: Capabilities ctype '[]
emptyCaps = RNil

-- |This constructor simultaneously specifies which browser the session will
-- use, while also providing browser-specific configuration. Default
-- configuration is provided for each browser by 'firefox', 'chrome', 'opera',
-- 'ie', etc.
--
-- This library uses 'firefox' as its 'Default' browser configuration, when no
-- browser choice is specified.
data BrowserType = 
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

instance Default BrowserType where
  def = firefox



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
      removeNulls = filter (\(k, v) -> case v of Null -> False; _ -> True)
      names = getKeysAsText caps
      values = 
               catMaybes -- remove unspecified values
             . recordToList 
             . rmap (\(F.Compose (Dict v)) -> 
                F.Const $ case v of
                  Unspecified -> Nothing
                  _           -> Just $ toJSON v)
             . reifyConstraint (T.Proxy :: T.Proxy ToJSON)
             $ getCapValues caps
      serialize Unspecified = Nothing
      serialize v = Just (toJSON v)

class ParseCapabilities fs where
  parseFields :: Value -> Parser (Capabilities Resultant fs)

instance ParseCapabilities '[] where
  parseFields _ = return RNil

instance (FromJSON (CapabilityFamily f), ParseCapabilities fs) => ParseCapabilities (f ': fs) where
  parseFields (Object o) = (:&) <$> parseFromKeys (HM.keys o) <*> parseFields (Object o)
    where
      parseFromKeys = maybe (return (CapabilityField ResultKey Unspecified )) mkField . tryAllKeys
      mkField (key, name) = (CapabilityField ResultKey ) . Actual <$> (parseJSON =<< (o .:? key .!= Null))
      tryAllKeys = foldr mplus Nothing . map tryKey
      tryKey k = (k ,) <$> keyToCapName k

  parseFields other = typeMismatch "Capabilities" other

instance ParseCapabilities names => FromJSON (Capabilities Resultant names) where
  parseJSON = parseFields


type family CapabilityNamesOf t :: [CapabilityName]

class GetCapabilities t (ckind :: CapabilityKind) where
  getCaps :: t -> Capabilities ckind (CapabilityNamesOf t)

class SetCapabilities t (ckind :: CapabilityKind) where
  setCaps :: t -> Capabilities ckind (CapabilityNamesOf t) -> Capabilities ckind (CapabilityNamesOf t)

-- |A short-hand for RecAll over the 'Capability' functor
type CapsAll ckind names c = RecAll (Capability ckind) names c

-- |A short=hand for 'RecAll' over the 'CapabilityField' functor
type FieldsAll ckind names c = RecAll (CapabilityField ckind) names c

-- |Shorthand to indicate that all names in a capability record can be converted to 'Text' strings
type KeysHaveText names = FieldsAll Requested names KeyToText

-- |Shorthand to indicate that all 'Capability' values in a record are parseable
type CapsAreParseable names = CapsAll Requested names ToJSON

instance ToJSON BrowserType where
  toJSON Firefox {} = String "firefox"
  toJSON Chrome {} = String "chrome"
  toJSON Opera {} = String "opera"
  toJSON IE {} = String "internet explorer"
  toJSON (OtherBrowser b) = String b
  toJSON b = String . T.toLower . fromString . show $ b

instance FromJSON BrowserType where
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
firefox :: BrowserType
firefox = Firefox Nothing def Nothing

-- |Default Chrome settings. All Maybe fields are set to Nothing, no options are
-- specified, and no extensions are used.
chrome :: BrowserType
chrome = Chrome Nothing Nothing [] []

-- |Default IE settings. See the 'IE' constructor for more details on
-- individual defaults
ie :: BrowserType
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
opera :: BrowserType
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

htmlUnit :: BrowserType
htmlUnit = HTMLUnit

iPhone :: BrowserType
iPhone = IPhone

iPad :: BrowserType
iPad = IPad

android :: BrowserType
android = Android

-- |Represents platform options supported by WebDriver. The value Any represents
-- no preference.
data PlatformType = Windows | XP | Vista | Mac | Linux | Unix | Any | OtherPlatform Text
              deriving (Eq, Show, Ord)

instance ToJSON PlatformType where
  toJSON (OtherPlatform txt) = String txt
  toJSON p = String . T.toUpper . fromString . show $ p

instance FromJSON PlatformType where
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

-- |Overloadable class of types that can be converted to a 'CapabilityKey'
--
-- Provided below is a list of auto-generated singleton types that can be used
-- as key specifiers for each 'CapabilityName'. In addition, 'String' and 'Text' values can be used to
-- represent raw JSON keys, which can be useful if using non-standard
-- webdriver implementations.
class KeySpecifier s ckind name | s -> name where
  fromKeySpecifier :: s -> CapabilityKey ckind name

instance {-# OVERLAPPING #-} KeySpecifier String ckind RawCapability
  fromKeySpecifier = RawKey . fromString

instance {-# OVERLAPPING #-} KeySpecifier Text ckind RawCapability where
  fromKeySpecifier = RawKey

instance KeySpecifier (Sing name) Requested name where
  fromKeySpecifier = RequestKey

instance KeySpecifier (Sing name) Resultant name where
  fromKeySpecifier _ = ResultKey

instance KeySpecifier (T.Proxy name) Resultant name where
  fromKeySpecifier _ = ResultKey

instance KeySpecifier (CapabilityKey ckind name) ckind name where
  fromKeySpecifier = id

-- |Creates a 'CapabilityField' from a key value pair. You can think of this as the \"=\" or \":\" in a table/object of a dynamically typed language, however
-- instead of using dynamic typing we use type inference to infer and statically resolve types at runtime.
--
-- Details: Using the 'KeySpecifier' class allows the type constructors the promoted data kinds 'CapabilityKind' and 'CapabilityName' to
-- be determined automatically, in addition it allows us to overload over things like 'String' keys for setting raw JSON values.
(=:) :: KeySpecifier s ckind name => s -> Capability ckind name -> CapabilityField ckind name
k =: v = CapabilityField (fromKeySpecifier k) v
infix 4 =:

-- |Appends a 'CapabilityField' to a 'Capabilities' record.
-- If you're familiar with vinyl, you may notice that this is equivalent to the ':&' operator, but with a restricted type.
--
-- Example:
--
-- > browser := chrome & platform := Windows & "raw JSON key" := (toJSON "raw value") & nullCaps
--
-- Notice the 'nullCaps' to terminate the chain at the end. This is analogous to ':' and '[]' in lists.
(&) :: CapabilityField ckind name -> Capabilities ckind (names) -> Capabilities
(&) = (:&)
infixr 9 &

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
