{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable,
    TemplateHaskell, OverloadedStrings, ExistentialQuantification, 
    MultiParamTypeClasses, TypeFamilies #-}
module Test.WebDriver.Types 
       ( WD(..)
         
       , SessionId(..), WindowHandle(..), currentWindow, Element(..)
       
       , WDSession(..), defaultSession
       , Capabilities(..), defaultCaps, allCaps
       , Browser(..), Platform(..), ProxyType(..)
                                    
       , InvalidURL(..), NoSessionId(..), BadJSON(..)
       , HTTPStatusUnknown(..), HTTPConnError(..)
       , UnknownCommand(..), ServerError(..)
       , FailedCommand(..), FailedCommandType(..)
       , FailedCommandInfo(..), StackFrame(..)
       , mkFailedCommandInfo, failedCommand
                                
       , Cookie(..)
       , Orientation(..)
       , MouseButton(..)
       , Selector(..)
       , JSArg(..)
       ) where
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Network.Stream (ConnError)


import Data.Text as Text (toLower, toUpper)
import Data.Text (Text)
import Data.ByteString.Lazy.Internal (ByteString)

import Control.Exception.Lifted
import Data.Typeable
import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.IO.Class
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Word
import Data.String
import qualified Data.Char as C


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

newtype SessionId = SessionId Text
                  deriving (Eq, Ord, Show, Read, 
                            IsString, FromJSON, ToJSON)

newtype WindowHandle = WindowHandle Text
                     deriving (Eq, Ord, Show, Read, 
                               IsString,  FromJSON, ToJSON)

newtype Element = Element Text
                  deriving (Eq, Ord, Show, Read, IsString)

currentWindow :: WindowHandle
currentWindow = WindowHandle "current"

data WDSession = WDSession { wdHost   :: String
                           , wdPort   :: Word
                           , wdSessId :: Maybe SessionId 
                           } deriving (Eq, Show)


defaultSession = WDSession { wdHost   = "127.0.0.1"
                           , wdPort   = 4444
                           , wdSessId = Nothing
                           }

data Browser = Firefox | Chrome | HTMLUnit | IE | IPhone 
             deriving (Eq, Show, Ord, Bounded, Enum)

data Platform = Windows | XP | Vista | Mac | Linux | Unix | Any
              deriving (Eq, Show, Ord, Bounded, Enum)


data Capabilities = Capabilities { browserName              :: Browser
                                 , version                  :: Maybe String
                                 , platform                 :: Platform
                                 , proxy                    :: ProxyType
                                 , javascriptEnabled        :: Maybe Bool
                                 , takesScreenshot          :: Maybe Bool
                                 , handlesAlerts            :: Maybe Bool
                                 , databaseEnabled          :: Maybe Bool
                                 , locationContextEnabled   :: Maybe Bool
                                 , applicationCacheEnabled  :: Maybe Bool
                                 , browserConnectionEnabled :: Maybe Bool
                                 , cssSelectorsEnabled      :: Maybe Bool
                                 , webStorageEnabled        :: Maybe Bool
                                 , rotatable                :: Maybe Bool
                                 , acceptSslCerts           :: Maybe Bool
                                 , nativeEvents             :: Maybe Bool
                                 } deriving (Eq, Show)



defaultCaps = Capabilities { browserName = Firefox
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
                           , acceptSslCerts = Nothing
                           , nativeEvents = Nothing
                           , proxy = UseSystemSettings
                           }

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
                      , acceptSslCerts = Just True
                      , nativeEvents = Just True
                      }

data ProxyType = NoProxy 
               | UseSystemSettings
               | AutoDetect
               | PAC { autoConfigUrl :: String }
               | Manual { ftpProxy  :: String
                        , sslProxy  :: String
                        , httpProxy :: String
                        }
               deriving (Eq, Show)
      
--todo: simplify error handling. include a module of convenience
--      functions. consider TH.

instance Exception InvalidURL
newtype InvalidURL = InvalidURL String 
                deriving (Eq, Show, Typeable)

instance Exception NoSessionId
newtype NoSessionId = NoSessionId String 
                 deriving (Eq, Show, Typeable)

instance Exception BadJSON
newtype BadJSON = BadJSON String 
             deriving (Eq, Show, Typeable)

instance Exception HTTPStatusUnknown
data HTTPStatusUnknown = HTTPStatusUnknown (Int, Int, Int) String
                       deriving (Eq, Show, Typeable)

instance Exception HTTPConnError
newtype HTTPConnError = HTTPConnError ConnError
                     deriving (Eq, Show, Typeable)

instance Exception UnknownCommand
newtype UnknownCommand = UnknownCommand String 
                    deriving (Eq, Show, Typeable)

instance Exception ServerError
newtype ServerError = ServerError String
                      deriving (Eq, Show, Typeable)

instance Exception FailedCommand
data FailedCommand = FailedCommand FailedCommandType FailedCommandInfo
                   deriving (Eq, Show, Typeable)


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

data FailedCommandInfo = FailedCommandInfo { errMsg    :: String
                                           , errSessId :: Maybe SessionId 
                                           , errScreen :: Maybe ByteString
                                           , errClass  :: Maybe String
                                           , errStack  :: [StackFrame]
                                           }
                       deriving (Eq)


mkFailedCommandInfo :: String -> FailedCommandInfo
mkFailedCommandInfo m = FailedCommandInfo {errMsg = m
                                          , errSessId = Nothing
                                          , errScreen = Nothing
                                          , errClass  = Nothing
                                          , errStack  = []
                                          }

failedCommand :: FailedCommandType -> String -> WD a
failedCommand t = throwIO . FailedCommand t . mkFailedCommandInfo


data StackFrame = StackFrame { sfFileName   :: String
                             , sfClassName  :: String
                             , sfMethodName :: String
                             , sfLineNumber :: Word
                             }
                  deriving (Show, Eq)


data Cookie = Cookie { cookName   :: Text
                     , cookValue  :: Text
                     , cookPath   :: Maybe Text
                     , cookDomain :: Maybe Text
                     , cookSecure :: Maybe Bool
                     , cookExpiry :: Maybe Integer
                     } deriving (Eq, Show)              

data Selector = ById Text
              | ByName Text
              | ByClass Text
              | ByTag Text
              | ByLinkText Text
              | ByPartialLinkText Text
              | ByCSS Text
              | ByXPath Text
                deriving (Eq, Show, Ord)

data JSArg = forall a. ToJSON a => JSArg a

data Orientation = Landscape | Portrait
                 deriving (Eq, Show, Ord, Bounded, Enum)

data MouseButton = LeftButton | MiddleButton | RightButton
                 deriving (Eq, Show, Ord, Bounded, Enum)



instance Show FailedCommandInfo where --todo: pretty print
  show i =   showString "{errMsg = "     . shows (errMsg i) 
           . showString ", errSessId = " . shows (errSessId i)
           . showString ", errScreen = " . screen
           . showString ", errClass = "  . shows (errClass i)
           . showString ", errStack = "  . shows (errStack i) 
           $ "}"
    where screen = showString $ case errScreen i of 
                                  Just v  -> "Just \"...\""
                                  Nothing -> "Nothing"
            


$( deriveToJSON id ''Capabilities )

instance FromJSON Element where
  parseJSON (Object o) = Element <$> o .: "ELEMENT"
  parseJSON v = typeMismatch "Element" v
  
instance ToJSON Element where
  toJSON (Element e) = object ["ELEMENT" .= e]

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
                      <*> pure Nothing
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
  toJSON = String . f . toLower . fromString . show
    where f "ie" = "internet explorer"
          f  x   = x

instance FromJSON Browser where
  parseJSON (String jStr) = case toLower jStr of
    "firefox"           -> return Firefox
    "chrome"            -> return Chrome
    "internet explorer" -> return IE
    "iphone"            -> return IPhone
    "htmlunit"          -> return HTMLUnit
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


instance FromJSON ProxyType where
  parseJSON (Object obj) = do
    let f :: FromJSON a => Text -> Parser a 
        f = (obj .:)
    pTyp <- f "proxyType"
    case toLower pTyp of
      "direct" -> return NoProxy
      "system" -> return UseSystemSettings
      "pac"    -> PAC <$> f "autoConfigUrl"
      "manual" -> Manual <$> f "ftpProxy" 
                         <*> f "sslProxy"
                         <*> f "httpProxy"
      _ -> fail $ "Invalid ProxyType " ++ show pTyp
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
      selector s t = object ["using" .= s, "value" .= t]
      
instance ToJSON JSArg where
  toJSON (JSArg a) = toJSON a
