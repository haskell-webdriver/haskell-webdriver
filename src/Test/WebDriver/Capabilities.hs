{-# LANGUAGE OverloadedStrings, RecordWildCards
  #-}
module Test.WebDriver.Capabilities where

import Test.WebDriver.Firefox.Profile
import Test.WebDriver.Chrome.Extension
import Test.WebDriver.JSON

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)

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
                     }

-- |Default capabilities. This is the same as the 'Default' instance, but with 
-- less polymorphism. By default, we use 'firefox' of an unspecified 'version' 
-- with default system-wide proxy settings on whatever 'platform' is available.
-- All Maybe Bool capabilities are set to Nothing (no preference).
defaultCaps :: Capabilities
defaultCaps = def

-- |Same as 'defaultCaps', but with all Maybe Bool capabilities set to 
-- Just True.
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
             ] ++ browserInfo
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
          -> ["IgnoreProtectedModeSettings" .= ignoreProtectedModeSettings
             --,"useLegacyInternalServer" .= u
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
  parseJSON (Object o) = Capabilities <$> req "browserName"
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
    where req :: FromJSON a => Text -> Parser a
          req = (o .:)            -- required field
          opt :: FromJSON a => Text -> a -> Parser a
          opt k d = o .:? k .!= d -- optional field
          b :: Text -> Parser (Maybe Bool)
          b k = opt k Nothing     -- Maybe Bool field
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
                       , ffLogPref :: LogPref
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
                    ignoreProtectedModeSettings :: Bool
                  --, useLegacyInternalServer     :: Bool
                  }
             | Opera { -- |Server-side path to the Opera binary
                       operaBinary    :: Maybe FilePath
                     --, operaNoRestart :: Maybe Bool 
                       -- |Which Opera product 'were using, e.g. "desktop",
                       -- "core"
                     , operaProduct   :: Maybe String
                       -- |Whether the Opera instance should stay open after
                       -- we close the session. If False, closing the session
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
                     , operaOptions   :: String
                       -- |Where to send the log output. If Nothing, logging is 
                       -- disabled.
                     , operaLogFile   :: Maybe FilePath
                       -- |Log level preference. Defaults to 'LogInfo'
                     , operaLogPref   :: LogPref
                     }
             | HTMLUnit
             | IPhone 
             | IPad
             | Android
             deriving (Eq, Show)

instance Default Browser where
  def = firefox


instance ToJSON Browser where
  toJSON Firefox {} = String "firefox"
  toJSON Chrome {}  = String "chrome"
  toJSON Opera {}   = String "opera"
  toJSON IE {}      = String "internet explorer"
  toJSON b = String . toLower . fromString . show $ b

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
    err  -> fail $ "Invalid Browser string " ++ show err
  parseJSON v = typeMismatch "Browser" v


-- |Default Firefox settings. All Maybe fields are set to Nothing. ffLogPref
-- is set to 'LogInfo'.
firefox :: Browser
firefox = Firefox Nothing def Nothing

-- |Default Chrome settings. All Maybe fields are set to Nothing, no options are
-- specified, and no extensions are used.
chrome :: Browser
chrome = Chrome Nothing Nothing [] []

-- |Default IE settings. 'ignoreProtectedModeSettings' is set to True.
ie :: Browser
ie = IE True

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
              , operaOptions = []
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

-- |Indicates log verbosity. Used in 'Firefox' and 'Opera' configuration.
data LogPref = LogOff | LogSevere | LogWarning | LogInfo | LogConfig 
             | LogFine | LogFiner | LogFinest | LogAll
             deriving (Eq, Show, Ord, Bounded, Enum)

instance Default LogPref where
  def = LogInfo

instance ToJSON LogPref where
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
    
instance FromJSON LogPref where
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
  parseJSON other = typeMismatch "LogPref" other