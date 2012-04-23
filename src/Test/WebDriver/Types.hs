{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable,
    TemplateHaskell, OverloadedStrings, ExistentialQuantification, 
    MultiParamTypeClasses, TypeFamilies, 
    RecordWildCards
  #-}
{-# OPTIONS_HADDOCK not-home #-}
module Test.WebDriver.Types 
       ( -- * WebDriver sessions
         WD(..), WDSession(..), defaultSession, SessionId(..)
         -- * Capabilities and configuration
       , Capabilities(..), defaultCaps, allCaps
       , Platform(..), ProxyType(..)
         -- ** Browser-specific configuration
       , Browser(..), 
         -- ** Default settings for browsers
         firefox, chrome, ie, opera, iPhone, iPad, android
       , LogPref(..)
         -- * WebDriver objects and command-specific types
       , Element(..)
       , WindowHandle(..), currentWindow
       , Selector(..)
       , JSArg(..)
       , FrameSelector(..)
       , Cookie(..), mkCookie
       , Orientation(..)
       , MouseButton(..)
       , WebStorageType(..)
         -- * Exceptions
       , InvalidURL(..), NoSessionId(..), BadJSON(..)
       , HTTPStatusUnknown(..), HTTPConnError(..)
       , UnknownCommand(..), ServerError(..)
       , FailedCommand(..), FailedCommandType(..)
       , FailedCommandInfo(..), StackFrame(..)
       , mkFailedCommandInfo, failedCommand
       ) where

import Test.WebDriver.Firefox.Profile
import Test.WebDriver.Chrome.Extension

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Network.Stream (ConnError)

import Data.Text as Text (toLower, toUpper, unpack)
import Data.Text (Text)
import Data.ByteString (ByteString)

import Control.Exception.Lifted
import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Typeable
import Data.List
import Data.Maybe
import Data.Word
import Data.String
import Text.Show
import Data.Default
import qualified Data.Char as C


{- |A monadic interface to the WebDriver server. This monad is a simple, strict 
layer over 'IO', threading session information between sequential commands
-}
newtype WD a = WD (StateT WDSession IO a)
  deriving (Functor, Monad, MonadState WDSession, MonadIO
           ,Applicative)

instance MonadBase IO WD where
  liftBase = WD . liftBase

instance MonadBaseControl IO WD where
  data StM WD a = StWD {unStWD :: StM (StateT WDSession IO) a}
  
  liftBaseWith f = WD $  
    liftBaseWith $ \runInBase ->
    f (\(WD sT) -> liftM StWD . runInBase $ sT)

  restoreM = WD . restoreM . unStWD

{- |An opaque identifier for a WebDriver session. These handles are produced by 
the server on session creation, and act to identify a session in progress. -} 
newtype SessionId = SessionId Text
                  deriving (Eq, Ord, Show, Read, 
                            FromJSON, ToJSON)


{- |An opaque identifier for a browser window -}
newtype WindowHandle = WindowHandle Text
                     deriving (Eq, Ord, Show, Read, 
                               FromJSON, ToJSON)
{- |An opaque identifier for a web page element. -}
newtype Element = Element Text
                  deriving (Eq, Ord, Show, Read)

-- |A special 'WindowHandle' that always refers to the currently focused window.
currentWindow :: WindowHandle
currentWindow = WindowHandle "current"

-- |Specifies the frame used by 'Test.WebDriver.Commands.focusFrame'
data FrameSelector = WithIndex Integer       
                     -- |focus on a frame by name or ID
                   | WithName Text
                     -- |focus on a frame Element
                   | WithElement Element
                     -- |focus on the first frame, or the main document
                     -- if iframes are used.
                   | DefaultFrame
                   deriving (Eq, Show, Read)

{- |Information about a WebDriver session. This structure is passed
implicitly through all 'WD' computations, and is also used to configure the 'WD'
monad before execution. -}
data WDSession = WDSession { 
                             -- |Host name of the WebDriver server for this 
                             -- session
                             wdHost   :: String
                             -- |Port number of the server
                           , wdPort   :: Word16
                             -- |An opaque reference identifying the session to
                             -- use with 'WD' commands.
                             -- A value of Nothing indicates that a session 
                             -- hasn't been created yet.
                             -- Sessions can be created within 'WD' via
                             -- 'Test.WebDriver.createSession', or created
                             -- and closed automatically with 
                             -- 'Test.WebDriver.runSession'
                           , wdSessId :: Maybe SessionId 
                           } deriving (Eq, Show)

instance Default WDSession where
  def = WDSession { wdHost   = "127.0.0.1"
                  , wdPort   = 4444
                  , wdSessId = Nothing
                  }

{- |A default session connects to localhost on port 4444, and hasn't been 
created yet. This value is the same as 'def' but with a more specific type. -}
defaultSession :: WDSession
defaultSession = def


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
      

-- This constructor simultaneously specifies which browser this session will 
-- use, while also providing browser-specific configuration. Default
-- configuration is provided for each browser by 'firefox', 'chrome', 'opera',
-- 'ie', etc.
--
-- The 'Default' instance uses 'firefox' as the default Browser.
-- When no browser is specified, that default is used.
data Browser = Firefox { -- |The firefox profile to use. If Nothing,
                         -- a default temporary profile is automatically created
                         -- and used.
                         ffProfile :: Maybe PreparedFirefoxProfile
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
                     , operaNoRestart :: Maybe Bool 
                     , operaProduct   :: Maybe String
                     , operaNoQuit    :: Bool
                     , operaAutoStart :: Bool
                     , operaDisplay   :: Maybe Int
                     , operaIdle      :: Bool
                     , operaProfile   :: Maybe String --PreparedOperaProfile
                     , operaLauncher  :: Maybe FilePath
                     , operaPort      :: Maybe Word16
                     , operaHost      :: Maybe String
                     , operaOptions   :: [String]
                     , operaLogFile   :: Maybe FilePath
                     , operaLogPref   :: LogPref
                     }
             -- | Safari
             | HTMLUnit
             | IPhone 
             | IPad
             | Android
             deriving (Eq, Show)
instance Default Browser where
  def = firefox

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

opera :: Browser
opera = Opera { operaBinary = Nothing
              , operaNoRestart = Nothing
              , operaProduct = Nothing
              , operaNoQuit = False
              , operaAutoStart = True
              , operaDisplay = Nothing
              , operaIdle = False
              , operaProfile = Nothing
              , operaLauncher = Nothing
              , operaHost = Nothing
              , operaPort = Nothing
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

-- |Indicates log verbosity. Used in 'Firefox' and 'Opera' configuration.
data LogPref = LogOff | LogSevere | LogWarning | LogInfo | LogConfig 
             | LogFine | LogFiner | LogFinest | LogAll
             deriving (Eq, Show, Ord, Bounded, Enum)

instance Default LogPref where
  def = LogInfo

instance Exception InvalidURL
-- |An invalid URL was given
newtype InvalidURL = InvalidURL String 
                deriving (Eq, Show, Typeable)

instance Exception NoSessionId
-- |A command requiring a session ID was attempted when no session ID was 
-- available.
newtype NoSessionId = NoSessionId String 
                 deriving (Eq, Show, Typeable)

instance Exception BadJSON
-- |An error occured when parsing a JSON value.
newtype BadJSON = BadJSON String 
             deriving (Eq, Show, Typeable)

instance Exception HTTPStatusUnknown
-- |An unexpected HTTP status was sent by the server.
data HTTPStatusUnknown = HTTPStatusUnknown (Int, Int, Int) String
                       deriving (Eq, Show, Typeable)

instance Exception HTTPConnError
-- |HTTP connection errors.
newtype HTTPConnError = HTTPConnError ConnError
                     deriving (Eq, Show, Typeable)

instance Exception UnknownCommand
-- |A command was sent to the WebDriver server that it didn't recognize.
newtype UnknownCommand = UnknownCommand String 
                    deriving (Eq, Show, Typeable)

instance Exception ServerError
-- |A server-side exception occured
newtype ServerError = ServerError String
                      deriving (Eq, Show, Typeable)

instance Exception FailedCommand
-- |This exception encapsulates a broad variety of exceptions that can
-- occur when a command fails.
data FailedCommand = FailedCommand FailedCommandType FailedCommandInfo
                   deriving (Eq, Show, Typeable)

-- |The type of failed command exception that occured.
data FailedCommandType = NoSuchElement
                       | NoSuchFrame
                       | UnknownFrame
                       | StaleElementReference
                       | ElementNotVisible
                       | InvalidElementState
                       | UnknownError
                       | ElementIsNotSelectable
                       | JavascriptError
                       | XPathLookupError
                       | Timeout
                       | NoSuchWindow
                       | InvalidCookieDomain
                       | UnableToSetCookie
                       | UnexpectedAlertOpen
                       | NoAlertOpen
                       | ScriptTimeout
                       | InvalidElementCoordinates
                       | IMENotAvailable
                       | IMEEngineActivationFailed
                       | InvalidSelector
                       | MoveTargetOutOfBounds
                       | InvalidXPathSelector
                       | InvalidXPathSelectorReturnType
                       | MethodNotAllowed
                       deriving (Eq, Ord, Enum, Bounded, Show)

-- |Detailed information about the failed command provided by the server.
data FailedCommandInfo = 
  FailedCommandInfo { -- |The error message.
                      errMsg    :: String
                      -- |The session associated with 
                      -- the exception.
                    , errSess :: WDSession 
                      -- |A screen shot of the focused window
                      -- when the exception occured,
                      -- if provided.
                    , errScreen :: Maybe ByteString
                      -- |The "class" in which the exception
                      -- was raised, if provided.
                    , errClass  :: Maybe String
                      -- |A stack trace of the exception.
                    , errStack  :: [StackFrame]
                    }
  deriving (Eq)

-- |Constructs a FailedCommandInfo from only an error message.
mkFailedCommandInfo :: String -> WD FailedCommandInfo
mkFailedCommandInfo m = do
  sess <- get
  return $ FailedCommandInfo {errMsg = m , errSess = sess , errScreen = Nothing
                             , errClass  = Nothing , errStack  = [] }

-- |Convenience function to throw a 'FailedCommand' locally with no server-side 
-- info present.
failedCommand :: FailedCommandType -> String -> WD a
failedCommand t m = throwIO . FailedCommand t =<< mkFailedCommandInfo m

-- |An individual stack frame from the stack trace provided by the server 
-- during a FailedCommand.
data StackFrame = StackFrame { sfFileName   :: String
                             , sfClassName  :: String
                             , sfMethodName :: String
                             , sfLineNumber :: Word
                             }
                deriving (Eq)

-- |Cookies are delicious delicacies. When sending cookies to the server, a value
-- of Nothing indicates that the server should use a default value. When receiving
-- cookies from the server, a value of Nothing indicates that the server is unable
-- to specify the value.
data Cookie = Cookie { cookName   :: Text
                     , cookValue  :: Text          -- ^ 
                     , cookPath   :: Maybe Text    -- ^path of this cookie.
                                                   -- if Nothing, defaults to /
                     , cookDomain :: Maybe Text    -- ^domain of this cookie.
                                                   -- if Nothing, the current pages
                                                   -- domain is used
                     , cookSecure :: Maybe Bool    -- ^Is this cookie secure?
                     , cookExpiry :: Maybe Integer -- ^Expiry date expressed as
                                                   -- seconds since the Unix epoch
                                                   -- Nothing indicates that the 
                                                   -- cookie never expires
                     } deriving (Eq, Show)              

-- |Creates a Cookie with only a name and value specified. All other
-- fields are set to Nothing, which tells the server to use default values.
mkCookie :: Text -> Text -> Cookie
mkCookie name value = Cookie { cookName = name, cookValue = value,
                               cookPath = Nothing, cookDomain = Nothing,
                               cookSecure = Nothing, cookExpiry = Nothing
                             }

-- |Specifies element(s) within a DOM tree using various selection methods.
data Selector = ById Text  
              | ByName Text
              | ByClass Text -- ^ (Note: multiple classes are not  
                             -- allowed. For more control, use 'ByCSS')
              | ByTag Text            
              | ByLinkText Text       
              | ByPartialLinkText Text
              | ByCSS Text
              | ByXPath Text
              deriving (Eq, Show, Ord)

-- |An existential wrapper for any 'ToJSON' instance. This allows us to pass
-- parameters of many different types to Javascript code.
data JSArg = forall a. ToJSON a => JSArg a

-- |A screen orientation
data Orientation = Landscape | Portrait
                 deriving (Eq, Show, Ord, Bounded, Enum)

-- |A mouse button
data MouseButton = LeftButton | MiddleButton | RightButton
                 deriving (Eq, Show, Ord, Bounded, Enum)


-- |An HTML 5 storage type
data WebStorageType = LocalStorage | SessionStorage 
                    deriving (Eq, Show, Ord, Bounded, Enum)

instance Show FailedCommandInfo where
  show i = showChar '\n' 
           . showString "Session: " . sess 
           . showChar '\n'
           . showString className . showString ": " . showString (errMsg i)
           . showChar '\n'
           . foldl (\f s-> f . showString "  " . shows s) id (errStack i)
           $ ""
    where
      className = fromMaybe "<unknown exception>" . errClass $ i
      
      sess = showString sessId . showString " at " 
             . showString host . showChar ':' . shows port
        where
          sessId = case msid of
            Just (SessionId sid) -> unpack sid
            Nothing -> "<no session id>"
          WDSession {wdHost = host, wdPort = port, wdSessId = msid } = errSess i

instance Show StackFrame where
  show f = showString (sfClassName f) . showChar '.' 
           . showString (sfMethodName f) . showChar ' '
           . showParen True ( showString (sfFileName f) . showChar ':'
                              . shows (sfLineNumber f))
           $ "\n"

instance FromJSON Element where
  parseJSON (Object o) = Element <$> o .: "ELEMENT"
  parseJSON v = typeMismatch "Element" v
  
instance ToJSON Element where
  toJSON (Element e) = object ["ELEMENT" .= e]

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
             ,"loggingPrefs" .= ffLogPref
             ,"firefox_binary" .= ffBinary
             ]
        Chrome {..}
          -> ["chrome.chromedriverVersion" .= chromeDriverVersion
             ,"chrome.binary" .= chromeBinary
             ,"chrome.switches" .= chromeOptions
             ,"chrome.extensions" .= chromeBinary
             ]       
        IE {..}
          -> ["IgnoreProtectedModeSettings" .= ignoreProtectedModeSettings
             --,"useLegacyInternalServer" .= u
             ]
        Opera{..}
          -> ["opera.binary" .= operaBinary
             ,"opera.guess_binary_path" .= isNothing operaBinary
             ,"opera.no_restart" .= operaNoRestart
             ,"opera.product" .= operaProduct
             ,"opera.no_quit" .= operaNoQuit
             ,"opera.autostart" .= operaAutoStart
             ,"opera.display" .= operaDisplay
             ,"opera.idle" .= operaIdle
             ,"opera.profile" .= operaProfile
             ,"opera.launcher" .= operaLauncher
             ,"opera.port" .= fromMaybe (-1) operaPort
             ,"opera.host" .= operaHost
              --note: I wonder if we should do any sort of quoting
              --around arguments? or perhaps use a flat string?
             ,"opera.arguments" .= intercalate " " operaOptions
             ,"opera.logging.file" .= operaLogFile
             ,"opera.logging.level" .= operaLogPref
             ]
        _ -> []


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

instance FromJSON FailedCommandInfo where
  parseJSON (Object o) = 
    FailedCommandInfo <$> (req "message" >>= maybe (return "") return)
                      <*> pure undefined
                      <*> opt "screen"     Nothing
                      <*> opt "class"      Nothing
                      <*> opt "stackTrace" []
    where req :: FromJSON a => Text -> Parser a 
          req = (o .:)            --required key
          opt :: FromJSON a => Text -> a -> Parser a
          opt k d = o .:? k .!= d --optional key
  parseJSON v = typeMismatch "FailedCommandInfo" v

instance FromJSON StackFrame where
  parseJSON (Object o) = StackFrame <$> reqStr "fileName"
                                    <*> reqStr "className"
                                    <*> reqStr "methodName"
                                    <*> req    "lineNumber"
    where req :: FromJSON a => Text -> Parser a
          req = (o .:) -- all keys are required
          reqStr :: Text -> Parser String
          reqStr k = req k >>= maybe (return "") return
  parseJSON v = typeMismatch "StackFrame" v

$( deriveToJSON (map C.toLower . drop 4) ''Cookie )

$( deriveJSON (map C.toUpper . drop 3) ''LogPref )

instance FromJSON Cookie where
  parseJSON (Object o) = Cookie <$> req "name"
                                <*> req "value"
                                <*> opt "path" Nothing
                                <*> opt "domain" Nothing
                                <*> opt "secure" Nothing
                                <*> opt "expiry" Nothing
    where 
      req :: FromJSON a => Text -> Parser a
      req = (o .:)
      opt :: FromJSON a => Text -> a -> Parser a
      opt k d = o .:? k .!= d
  parseJSON v = typeMismatch "Cookie" v


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

instance ToJSON Orientation where
  toJSON = String . toUpper . fromString . show

instance FromJSON Orientation where
  parseJSON (String jStr) = case toLower jStr of
    "landscape" -> return Landscape
    "portrait"  -> return Portrait
    err         -> fail $ "Invalid Orientation string " ++ show err
  parseJSON v = typeMismatch "Orientation" v
  
instance ToJSON MouseButton where
  toJSON = String . toUpper . fromString . show
  
instance FromJSON MouseButton where
  parseJSON (String jStr) = case toLower jStr of
    "left"   -> return LeftButton
    "middle" -> return MiddleButton
    "right"  -> return RightButton
    err      -> fail $ "Invalid MouseButton string " ++ show err
  parseJSON v = typeMismatch "MouseButton" v


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

instance ToJSON Selector where
  toJSON s = case s of
    ById t              -> selector "id" t
    ByName t            -> selector "name" t
    ByClass t           -> selector "class name" t
    ByTag t             -> selector "tag name" t
    ByLinkText t        -> selector "link text" t
    ByPartialLinkText t -> selector "partial link text" t
    ByCSS t             -> selector "css selector" t
    ByXPath t           -> selector "xpath" t
    where
      selector :: Text -> Text -> Value
      selector sn t = object ["using" .= sn, "value" .= t]
      
instance ToJSON JSArg where
  toJSON (JSArg a) = toJSON a

instance ToJSON FrameSelector where
  toJSON s = case s of
    WithIndex i -> toJSON i
    WithName n -> toJSON n
    WithElement e -> toJSON e
    DefaultFrame -> Null