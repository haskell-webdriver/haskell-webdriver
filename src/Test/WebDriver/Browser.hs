{-# LANGUAGE OverloadedStrings, RecordWildCards, EmptyDataDecls, GADTs, FlexibleInstances, StandaloneDeriving, 
    TypeFamilies, FlexibleContexts, ConstraintKinds, UndecidableInstances, ScopedTypeVariables
  #-}
module Test.WebDriver.Browser where

import Test.WebDriver.Common.Profile (PreparedProfile)
import Test.WebDriver.Chrome.Extension (ChromeExtension)
import Test.WebDriver.JSON

import Data.Default
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Text as T (Text, toLower, unpack)
import Data.String (fromString)
import Data.Word (Word16)

import Control.Applicative
import Control.Monad (when)
import Control.Exception.Lifted (throw)

-- GADT Browser tags
data Firefox
data Chrome
data IE
data Opera
data HTMLUnit
data IPhone
data IPad
data Android

class BrowserTag t where
  expectedBrowserName :: t -> Text
  browserCapabilities :: t -> [Text]
  parseBrowserJSON :: Object -> Parser (Browser t)

instance BrowserTag Firefox  where
  expectedBrowserName _ = "firefox"
  browserCapabilities _ = ["firefox_profile", "loggingPrefs", "firefox_binary"]
  parseBrowserJSON o = 
    Firefox <$> opt "firefox_profile" Nothing
            <*> opt "loggingPrefs" def
            <*> opt "firefox_binary" Nothing
    where
      opt :: FromJSON a => Text -> a -> Parser a
      opt k d = o .:?? k .!= d -- optional field

instance BrowserTag Chrome  where
  expectedBrowserName _ = "chrome"
  browserCapabilities _ = ["chrome.chromedriverVersion", "chrome.extensions", "chrome.switches", "chrome.extensions"]
  parseBrowserJSON o =
    Chrome <$> opt "chrome.chromedriverVersion" Nothing
           <*> opt "chrome.extensions" Nothing
           <*> opt "chrome.switches" []
           <*> opt "chrome.extensions" []
    where
      opt :: FromJSON a => Text -> a -> Parser a
      opt k d = o .:?? k .!= d -- optional field

instance BrowserTag IE  where
  expectedBrowserName _ = "internet explorer"
  browserCapabilities _ = ["ignoreProtectedModeSettings", "ignoreZoomSettings", "initialBrowserUrl", "elementScrollBehavior"
                          ,"enablePersistentHover", "enableElementCacheCleanup", "requireWindowFocus", "browserAttachTimeout"
                          ,"logFile", "logLevel", "host", "extractPath", "silent", "forceCreateProcess", "internetExplorerSwitches"]
  parseBrowserJSON o =
    IE <$> opt "ignoreProtectedModeSettings" True
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
    where
      opt :: FromJSON a => Text -> a -> Parser a
      opt k d = o .:?? k .!= d -- optional field

instance BrowserTag Opera  where
  expectedBrowserName _ = "opera"
  browserCapabilities _ = ["opera.binary", "opera.product", "opera.no_quit", "opera.autostart", "opera.idle", "opera.display"
                          ,"opera.launcher", "opera.port", "opera.host", "opera.arguments", "opera.logging.file", "opera.logging.level"]
  parseBrowserJSON o =
    Opera <$> opt "opera.binary" Nothing
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
    where
      opt :: FromJSON a => Text -> a -> Parser a
      opt k d = o .:?? k .!= d -- optional field

instance BrowserTag HTMLUnit where
  expectedBrowserName _ = "htmlunit"
  browserCapabilities _ = []
  parseBrowserJSON _ = return HTMLUnit

instance BrowserTag IPhone  where
  expectedBrowserName _ = "iphone" 
  browserCapabilities _ = []
  parseBrowserJSON _ = return IPhone

instance BrowserTag IPad  where
  expectedBrowserName _ = "ipad"
  browserCapabilities _ = []
  parseBrowserJSON _ = return IPad

instance BrowserTag Android  where
  expectedBrowserName _ = "Android"
  browserCapabilities _ = []
  parseBrowserJSON _ = return Android

class BrowserTag (BrowserTagOf t) => HasBrowserTag t where
  type BrowserTagOf t


instance BrowserTag t => HasBrowserTag (Browser t) where
  type BrowserTagOf (Browser t) = t

browserTagOf :: HasBrowserTag t => t -> BrowserTagOf t
browserTagOf = undefined

type BrowserOf t = Browser (BrowserTagOf t)

-- |This constructor simultaneously specifies which browser the session will
-- use, while also providing browser-specific configuration. Default
-- configuration is provided for each browser by 'firefox', 'chrome', 'opera',
-- 'ie', etc.
--
-- This library uses 'firefox' as its 'Default' browser configuration, when no
-- browser choice is specified.
data Browser b where
    Firefox :: { -- |The firefox profile to use. If Nothing,
              -- a default temporary profile is automatically created
              -- and used.
                ffProfile :: Maybe (PreparedProfile Firefox)
              -- |Firefox logging preference
              , ffLogPref :: LogLevel
              -- |Server-side path to Firefox binary. If Nothing,
              -- use a sensible system-based default.
              , ffBinary :: Maybe FilePath
    } -> Browser Firefox
    Chrome :: { -- |Version of the Chrome Webdriver server server to use
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
    } -> Browser Chrome
    IE :: { -- |Whether to skip the protected mode check. If set, tests
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
    } -> Browser IE
    Opera :: { -- |Server-side path to the Opera binary
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
    } -> Browser Opera
    HTMLUnit :: Browser HTMLUnit
    IPhone :: Browser IPhone
    IPad :: Browser IPad
    Android :: Browser Android
    -- |some other browser, specified by a string name
    Browser :: Text -> Browser b

deriving instance Show (Browser b)
deriving instance Eq (Browser b)

instance Default (Browser Firefox) where
  def = firefox

instance Default (Browser Chrome) where
  def = chrome

instance Default (Browser IE) where
  def = ie

instance Default (Browser Opera) where
  def = opera

instance Default (Browser Android) where
  def = android

instance Default (Browser HTMLUnit) where
  def = htmlUnit

instance Default (Browser IPad) where
  def = iPad

instance Default (Browser IPhone) where
  def = iPhone



instance ToJSON (Browser b) where
  toJSON Firefox {}  = String "firefox"
  toJSON Chrome {}   = String "chrome"
  toJSON Opera {}    = String "opera"
  toJSON IE {}       = String "internet explorer"
  toJSON (Browser b) = String b
  toJSON b           = String . toLower . fromString . show $ b


instance BrowserTag b => FromJSON (Browser b) where
  parseJSON (Object o) = do
    browserName <- o .: "browserName" -- TODO: check that browser name matches our browser tag
    when (toLower browserName /= toLower expected) $
      fail $ "Expected browser " ++ T.unpack expected ++ " but got " ++ T.unpack browserName
    parseBrowserJSON o
    where
      expected = expectedBrowserName (undefined :: b)

  parseJSON v = typeMismatch ("Browser " ++ T.unpack expected) v
    where
        expected = expectedBrowserName (undefined :: b)

-- |Default Firefox settings. All Maybe fields are set to Nothing. ffLogPref
-- is set to 'LogInfo'.
firefox :: Browser Firefox
firefox = Firefox Nothing def Nothing

-- |Default Chrome settings. All Maybe fields are set to Nothing, no options are
-- specified, and no extensions are used.
chrome :: Browser Chrome
chrome = Chrome Nothing Nothing [] []

-- |Default IE settings. See the 'IE' constructor for more details on
-- individual defaults
ie :: Browser IE
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
opera :: Browser Opera
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

htmlUnit :: Browser HTMLUnit
htmlUnit = HTMLUnit

iPhone :: Browser IPhone
iPhone = IPhone

iPad :: Browser IPad
iPad = IPad

android :: Browser Android
android = Android



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
