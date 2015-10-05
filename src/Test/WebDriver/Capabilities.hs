{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts, UndecidableInstances, GADTs
  #-}
module Test.WebDriver.Capabilities where

import Test.WebDriver.Firefox.Profile
import Test.WebDriver.Chrome.Extension
import Test.WebDriver.JSON
import Test.WebDriver.Browser

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch, Pair)
import qualified Data.HashMap.Strict as HM (delete, toList)

import Data.Text (Text, toLower, toUpper)
import Data.Default (Default(..))
import Data.Word (Word16)
import Data.Maybe (fromMaybe, catMaybes)
import Data.String (fromString)

import Control.Applicative
import Control.Exception.Lifted (throw)

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

instance ToJSON (Capabilities b) where
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


instance FromJSON (Capabilities b) where
  parseJSON (Object o) = do
    browserName <- req "browserName"
    browser <- getBrowserCaps browserName
    Capabilities <$> return browser
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
                 <*> pure (additionalCapabilities browser)

    where --some helpful JSON accessor shorthands
          req :: FromJSON a => Text -> Parser a
          req = (o .:)            -- required field
          opt :: FromJSON a => Text -> a -> Parser a
          opt k d = o .:?? k .!= d -- optional field
          b :: Text -> Parser (Maybe Bool)
          b k = opt k Nothing     -- Maybe Bool field

          -- produce additionalCaps by removing known capabilities from the JSON object
          additionalCapabilities = HM.toList . foldr HM.delete o . knownCapabilities

          knownCapabilities browser =
            ["browserName", "version", "platform", "proxy"
            ,"javascriptEnabled", "takesScreenshot", "handlesAlerts"
            ,"databaseEnabled", "locationContextEnabled"
            ,"applicationCacheEnabled", "browserConnectionEnabled"
            , "cssSelectorEnabled","webStorageEnabled", "rotatable"
            , "acceptSslCerts", "nativeEvents", "unexpectedBrowserBehaviour"]
            ++ case browser of
              Firefox {} -> ["firefox_profile", "loggingPrefs", "firefox_binary"]
              Chrome {} -> ["chrome.chromedriverVersion", "chrome.extensions", "chrome.switches", "chrome.extensions"]
              IE {} -> ["ignoreProtectedModeSettings", "ignoreZoomSettings", "initialBrowserUrl", "elementScrollBehavior"
                       ,"enablePersistentHover", "enableElementCacheCleanup", "requireWindowFocus", "browserAttachTimeout"
                       ,"logFile", "logLevel", "host", "extractPath", "silent", "forceCreateProcess", "internetExplorerSwitches"]
              Opera {} -> ["opera.binary", "opera.product", "opera.no_quit", "opera.autostart", "opera.idle", "opera.display"
                          ,"opera.launcher", "opera.port", "opera.host", "opera.arguments", "opera.logging.file", "opera.logging.level"]
              _ -> []
          getBrowserCaps :: Text -> Parser (Browser b)
          getBrowserCaps browser =
            case toLower browser of
              "firefox" -> Firefox <$> opt "firefox_profile" Nothing
                                   <*> opt "loggingPrefs" def
                                   <*> opt "firefox_binary" Nothing
              "chrome" -> Chrome <$> opt "chrome.chromedriverVersion" Nothing
                                 <*> opt "chrome.extensions" Nothing
                                 <*> opt "chrome.switches" []
                                 <*> opt "chrome.extensions" []
              "internet explorer" 
                -> IE <$> opt "ignoreProtectedModeSettings" True
                      <*> opt "ignoreZoomSettings" False
                      <*> opt "initialBrowserUrl" Nothing
                      <*> opt "elementScrollBehavior" def
                      <*> opt "enablePersistentHover" True
                      <*> opt "enableElementCacheCleanup" True
                      <*> opt "requireWindowFocus" False
                      <*> opt "browserAttachTimeout" 0
                      <*> opt "logFile" Nothing
                      <*> opt "logLevel" def
                      <*> opt "host" Nothing
                      <*> opt "extractPath" Nothing
                      <*> opt "silent" False
                      <*> opt "forceCreateProcess" False
                      <*> opt "internetExplorerSwitches" Nothing
              "opera" -> Opera <$> opt "opera.binary" Nothing
                               <*> opt "opera.product" Nothing
                               <*> opt "opera.no_quit" False
                               <*> opt "opera.autostart" True
                               <*> opt "opera.idle" False
                               <*> opt "opera.display" Nothing
                               <*> opt "opera.launcher" Nothing
                               <*> opt "opera.port" (Just 0)
                               <*> opt "opera.host" Nothing
                               <*> opt "opera.arguments" Nothing
                               <*> opt "opera.logging.file" Nothing
                               <*> opt "opera.logging.level" def
              other -> parseJSON (String other)

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
