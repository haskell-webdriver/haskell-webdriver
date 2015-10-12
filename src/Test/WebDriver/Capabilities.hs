{-# LANGUAGE OverloadedStrings, RecordWildCards, ConstraintKinds, FlexibleContexts, UndecidableInstances, GADTs,  TypeFamilies, ScopedTypeVariables
  #-}
module Test.WebDriver.Capabilities where

import Test.WebDriver.JSON
import Test.WebDriver.Browser

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch, Pair)
import qualified Data.HashMap.Strict as HM (delete, toList)

import Data.Text as T (Text, toLower, toUpper)
import Data.Default (Default(..))
import Data.Maybe (fromMaybe, catMaybes)
import Data.String (fromString)

import Control.Applicative
import Control.Exception.Lifted (throw)

type CapabilitiesOf t = Capabilities (BrowserTagOf t)

instance BrowserTag t => HasBrowserTag (Capabilities t) where
  type BrowserTagOf (Capabilities t) = t

-- |A typeclass for readable 'Capabilities'
class GetCapabilities t where
  getCaps :: t -> Capabilities (BrowserTagOf t)

-- |A typeclass for writable 'Capabilities'
class SetCapabilities t where
  setCaps :: Capabilities (BrowserTagOf t) -> t -> t

-- |Read/write 'Capabilities'
type HasCapabilities t = (GetCapabilities t, SetCapabilities t)

-- |Modifies the 'wdCapabilities' field of a 'WDConfig' by applying the given function. Overloaded to work with any 'HasCapabilities' instance.
modifyCaps :: HasCapabilities t => (CapabilitiesOf t -> CapabilitiesOf t) -> t -> t
modifyCaps f c = setCaps (f (getCaps c)) c

-- |A helper function for setting the 'browser' capability of a 'HasCapabilities' instance
useBrowser :: HasCapabilities t => BrowserOf t -> t -> t
useBrowser b = modifyCaps $ \c -> c { browser = b }

-- |A helper function for setting the 'version' capability of a 'HasCapabilities' instance
useVersion :: HasCapabilities t => String -> t -> t
useVersion v = modifyCaps $ \c -> c { version = Just v }

-- |A helper function for setting the 'platform' capability of a 'HasCapabilities' instance 
usePlatform :: HasCapabilities t => Platform -> t -> t
usePlatform p = modifyCaps $ \c -> c { platform = p }

-- |A helper function for setting the 'useProxy' capability of a 'HasCapabilities' instance
useProxy :: HasCapabilities t => ProxyType -> t -> t
useProxy p = modifyCaps $ \c -> c { proxy = p }


{- |A structure describing the capabilities of a session. This record
serves dual roles.

* It's used to specify the desired capabilities for a session before
it's created. In this usage, fields that are set to Nothing indicate
that we have no preference for that capability.

* When received from the server , it's used to
describe the actual capabilities given to us by the WebDriver
server. Here a value of Nothing indicates that the server doesn't
support the capability. Thus, for Maybe Bool fields, both Nothing and
Just False indicate a lack of support for the desired capability.
-}
data Capabilities b =
  Capabilities { -- |Browser choice and browser specific settings.
                 browser                  :: Browser b
                 -- |Browser version to use.
               , version                  :: Maybe String
                 -- |Platform on which the browser should run.
               , platform                 :: Platform
                 -- |Proxy configuration settings.
               , proxy                    :: ProxyType
                 -- |Whether the session supports executing JavaScript via
                 -- 'executeJS' and 'asyncJS'.
               , javascriptEnabled        :: Maybe Bool
                 -- |Whether the session supports taking screenshots of the
                 -- current page with the 'screenshot' command
               , takesScreenshot          :: Maybe Bool
                 -- |Whether the session can interact with modal popups,
                 -- such as window.alert and window.confirm via
                 -- 'acceptAlerts', 'dismissAlerts', etc.
               , handlesAlerts            :: Maybe Bool
                 -- |Whether the session can interact with database storage.
               , databaseEnabled          :: Maybe Bool
                 -- |Whether the session can set and query the browser's
                 -- location context with 'setLocation' and 'getLocation'.
               , locationContextEnabled   :: Maybe Bool
                 -- |Whether the session can interact with the application cache
                 -- .
               , applicationCacheEnabled  :: Maybe Bool
                 -- |Whether the session can query for the browser's
                 -- connectivity and disable it if desired
               , browserConnectionEnabled :: Maybe Bool
                 -- |Whether the session supports CSS selectors when searching
                 -- for elements.
               , cssSelectorsEnabled      :: Maybe Bool
                 -- |Whether Web Storage ('getKey', 'setKey', etc) support is
                 -- enabled
               , webStorageEnabled        :: Maybe Bool
                 -- |Whether the session can rotate the current page's current
                 -- layout between 'Portrait' and 'Landscape' orientations.
               , rotatable                :: Maybe Bool
                 -- |Whether the session should accept all SSL certs by default
               , acceptSSLCerts           :: Maybe Bool
                 -- |Whether the session is capable of generating native OS
                 -- events when simulating user input.
               , nativeEvents             :: Maybe Bool
                 -- |How the session should handle unexpected alerts.
               , unexpectedAlertBehavior :: Maybe UnexpectedAlertBehavior
                 -- |A list of ('Text', 'Value') pairs specifying additional non-standard capabilities.
               , additionalCaps           :: [Pair]
               } deriving (Eq, Show)

instance Default (Browser b) => Default (Capabilities b) where
  def = Capabilities { browser = def
                     , version = Nothing
                     , platform = Any
                     , javascriptEnabled = Nothing
                     , takesScreenshot = Nothing
                     , handlesAlerts = Nothing
                     , databaseEnabled = Nothing
                     , locationContextEnabled = Nothing
                     , applicationCacheEnabled = Nothing
                     , browserConnectionEnabled = Nothing
                     , cssSelectorsEnabled = Nothing
                     , webStorageEnabled = Nothing
                     , rotatable = Nothing
                     , acceptSSLCerts = Nothing
                     , nativeEvents = Nothing
                     , proxy = UseSystemSettings
                     , unexpectedAlertBehavior = Nothing
                     , additionalCaps = []
                     }

-- |Default capabilities. This is the same as the 'Default' instance, but with
-- less polymorphism. All 'Maybe' capabilities are set to 'Nothing' (no preference).
defaultCaps :: Default (Browser b) => Capabilities b
defaultCaps = def

-- |Same as 'defaultCaps', but with all 'Maybe' 'Bool' capabilities set to
-- 'Just' 'True'.
allCaps :: Default (Browser b) => Capabilities b
allCaps = defaultCaps { javascriptEnabled = Just True
                      , takesScreenshot = Just True
                      , handlesAlerts = Just True
                      , databaseEnabled = Just True
                      , locationContextEnabled = Just True
                      , applicationCacheEnabled = Just True
                      , browserConnectionEnabled = Just True
                      , cssSelectorsEnabled = Just True
                      , webStorageEnabled = Just True
                      , rotatable = Just True
                      , acceptSSLCerts = Just True
                      , nativeEvents = Just True
                      }

instance (ToJSON (Browser b)) => ToJSON (Capabilities b) where
  toJSON Capabilities{..} =
    object $ filter (\p -> snd p /= Null)
           $ [ "browserName" .= browser
             , "version" .= version
             , "platform" .= platform
             , "proxy" .= proxy
             , "javascriptEnabled" .= javascriptEnabled
             , "takesScreenshot" .= takesScreenshot
             , "handlesAlerts" .= handlesAlerts
             , "databaseEnabled" .= databaseEnabled
             , "locationContextEnabled" .= locationContextEnabled
             , "applicationCacheEnabled" .= applicationCacheEnabled
             , "browserConnectionEnabled" .= browserConnectionEnabled
             , "cssSelectorsEnabled" .= cssSelectorsEnabled
             , "webStorageEnabled" .= webStorageEnabled
             , "rotatable" .= rotatable
             , "acceptSslCerts" .= acceptSSLCerts
             , "nativeEvents" .= nativeEvents
             , "unexpectedAlertBehavior" .= unexpectedAlertBehavior
             ]
    ++ browserInfo
    ++ additionalCaps
    where
      browserInfo = case browser of
        Firefox {..}
          -> ["firefox_profile" .= ffProfile
             ,"loggingPrefs" .= object ["driver" .= ffLogPref]
             ,"firefox_binary" .= ffBinary
             ]
        Chrome {..}
          -> catMaybes [ opt "chrome.chromedriverVersion" chromeDriverVersion ]
             ++ [ "chromeOptions" .= object (catMaybes
                  [ opt "binary" chromeBinary
                  ] ++
                  [ "args"       .= chromeOptions
                  , "extensions" .= chromeExtensions
                  ]
                )]
        IE {..}
          -> ["ignoreProtectedModeSettings" .= ieIgnoreProtectedModeSettings
             ,"ignoreZoomSetting" .= ieIgnoreZoomSetting
             ,"initialBrowserUrl" .= ieInitialBrowserUrl
             ,"elementScrollBehavior" .= ieElementScrollBehavior
             ,"enablePersistentHover" .= ieEnablePersistentHover
             ,"enableElementCacheCleanup" .= ieEnableElementCacheCleanup
             ,"requireWindowFocus" .= ieRequireWindowFocus
             ,"browserAttachTimeout" .= ieBrowserAttachTimeout
             ,"logFile" .= ieLogFile
             ,"logLevel" .= ieLogLevel
             ,"host" .= ieHost
             ,"extractPath" .= ieExtractPath
             ,"silent" .= ieSilent
             ,"forceCreateProcess" .= ieForceCreateProcess
             ,"internetExplorerSwitches" .= ieSwitches
             ]
        Opera{..}
          -> catMaybes [ opt "opera.binary" operaBinary
                       , opt "opera.display" operaDisplay
                       , opt "opera.product" operaProduct
                       , opt "opera.launcher" operaLauncher
                       , opt "opera.host" operaHost
                       , opt "opera.logging.file" operaLogFile
                       ]
             ++ ["opera.detatch" .= operaDetach
                ,"opera.no_quit" .= operaDetach --backwards compatability
                ,"opera.autostart" .= operaAutoStart
                , "opera.idle" .= operaIdle
                -- ,"opera.profile" .= operaProfile
                ,"opera.port" .= fromMaybe (-1) operaPort
                 --note: consider replacing operaOptions with a list of options
                ,"opera.arguments" .= operaOptions
                ,"opera.logging.level" .= operaLogPref
                ]
        _ -> []

        where
          opt k = fmap (k .=)


instance (BrowserTag b, FromJSON (Browser b)) => FromJSON (Capabilities b) where
  parseJSON (Object o) = 
    Capabilities <$> parseJSON (Object o)  -- parse browser specific capabilities
                 <*> opt "version" Nothing
                 <*> req "platform"
                 <*> opt "proxy" NoProxy
                 <*> b "javascriptEnabled"
                 <*> b "takesScreenshot"
                 <*> b "handlesAlerts"
                 <*> b "databaseEnabled"
                 <*> b "locationContextEnabled"
                 <*> b "applicationCacheEnabled"
                 <*> b "browserConnectionEnabled"
                 <*> b "cssSelectorEnabled"
                 <*> b "webStorageEnabled"
                 <*> b "rotatable"
                 <*> b "acceptSslCerts"
                 <*> b "nativeEvents"
                 <*> opt "unexpectedAlertBehaviour" Nothing
                 <*> pure additionalCapabilities

    where --some helpful JSON accessor shorthands
      req :: FromJSON a => Text -> Parser a
      req = (o .:)            -- required field
      opt :: FromJSON a => Text -> a -> Parser a
      opt k d = o .:?? k .!= d -- optional field
      b :: Text -> Parser (Maybe Bool)
      b k = opt k Nothing     -- Maybe Bool field

      -- produce additionalCaps by removing known capabilities from the JSON object
      additionalCapabilities :: [Pair]
      additionalCapabilities = HM.toList . foldr HM.delete o $ knownCapabilities

      knownCapabilities :: [Text]
      knownCapabilities =
        ["browserName", "version", "platform", "proxy"
        ,"javascriptEnabled", "takesScreenshot", "handlesAlerts"
        ,"databaseEnabled", "locationContextEnabled"
        ,"applicationCacheEnabled", "browserConnectionEnabled"
        , "cssSelectorEnabled","webStorageEnabled", "rotatable"
        , "acceptSslCerts", "nativeEvents", "unexpectedBrowserBehaviour"]
        ++ browserCapabilities (undefined :: b)

  parseJSON v = typeMismatch "Capabilities" v


-- |Represents platform options supported by WebDriver. The value Any represents
-- no preference.
data Platform = Windows | XP | Vista | Mac | Linux | Unix | Any
              deriving (Eq, Show, Ord, Bounded, Enum)

instance ToJSON Platform where
  toJSON = String . toUpper . fromString . show

instance FromJSON Platform where
  parseJSON (String jStr) = case toLower jStr of
    "windows" -> return Windows
    "xp"      -> return XP
    "vista"   -> return Vista
    "mac"     -> return Mac
    "linux"   -> return Linux
    "unix"    -> return Unix
    "any"     -> return Any
    err -> fail $ "Invalid Platform string " ++ show err
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
    case toLower pTyp of
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
