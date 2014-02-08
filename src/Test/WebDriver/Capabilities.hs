{-# LANGUAGE OverloadedStrings, RecordWildCards
  #-}
module Test.WebDriver.Capabilities where

import Test.WebDriver.Firefox.Profile
import Test.WebDriver.Chrome.Extension
import Test.WebDriver.JSON

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
data Capabilities =
  Capabilities { -- |Browser choice and browser specific settings.
                 browser                  :: Browser
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

instance Default Capabilities where
  def = Capabilities { browser = firefox
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
-- less polymorphism. By default, we use 'firefox' of an unspecified 'version'
-- with default system-wide 'proxy' settings on whatever 'platform' is available
-- . All 'Maybe' capabilities are set to 'Nothing' (no preference).
defaultCaps :: Capabilities
defaultCaps = def

-- |Same as 'defaultCaps', but with all 'Maybe' 'Bool' capabilities set to
-- 'Just' 'True'.
allCaps :: Capabilities
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


instance ToJSON Capabilities where
  toJSON Capabilities{..} =
    object $ [ "browserName" .= browser
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
          -> catMaybes [ opt "chrome.chromedriverVersion" chromeDriverVersion
                       , opt "chrome.binary" chromeBinary
                       ]
             ++ ["chrome.switches" .= chromeOptions
                ,"chrome.extensions" .= chromeExtensions
                ]
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


instance FromJSON Capabilities where
  parseJSON (Object o) = do
    browser <- req "browserName"
    Capabilities <$> getBrowserCaps browser
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
          opt k d = o .:? k .!= d -- optional field
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
          getBrowserCaps browser =
            case browser of 
              Firefox {} -> Firefox <$> opt "firefox_profile" Nothing
                                    <*> opt "loggingPrefs" def
                                    <*> opt "firefox_binary" Nothing
              Chrome {} -> Chrome <$> opt "chrome.chromedriverVersion" Nothing
                                  <*> opt "chrome.extensions" Nothing
                                  <*> opt "chrome.switches" []
                                  <*> opt "chrome.extensions" []
              IE {} -> IE <$> opt "ignoreProtectedModeSettings" True
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
              Opera {} -> Opera <$> opt "opera.binary" Nothing
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
              _ -> return browser
                              
  parseJSON v = typeMismatch "Capabilities" v

-- |This constructor simultaneously specifies which browser the session will
-- use, while also providing browser-specific configuration. Default
-- configuration is provided for each browser by 'firefox', 'chrome', 'opera',
-- 'ie', etc.
--
-- This library uses 'firefox' as its 'Default' browser configuration, when no
-- browser choice is specified.
data Browser = Firefox { -- |The firefox profile to use. If Nothing,
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
             | Browser Text
             deriving (Eq, Show)

instance Default Browser where
  def = firefox


instance ToJSON Browser where
  toJSON Firefox {}  = String "firefox"
  toJSON Chrome {}   = String "chrome"
  toJSON Opera {}    = String "opera"
  toJSON IE {}       = String "internet explorer"
  toJSON (Browser b) = String b
  toJSON b           = String . toLower . fromString . show $ b

instance FromJSON Browser where
  parseJSON (String jStr) = case toLower jStr of
    "firefox"           -> return firefox
    "chrome"            -> return chrome
    "internet explorer" -> return ie
    "opera"             -> return opera
    -- "safari"            -> return safari
    "iphone"            -> return iPhone
    "ipad"              -> return iPad
    "android"           -> return android
    "htmlunit"          -> return htmlUnit
    other               -> return (Browser other)
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
    
