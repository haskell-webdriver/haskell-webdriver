{-# LANGUAGE GeneralizedNewtypeDeriving, 
    TemplateHaskell, OverloadedStrings, NoMonomorphismRestriction #-}
module Test.WebDriver.Types 
       ( SessionId(..), WindowHandle(..), currentWindow, Element(..)
         
       , WD(..)
         
       , WDSession(..), defaultSession
       , Capabilities(..), defaultCaps, allCaps
       , Browser(..), Platform(..), ProxyType(..)
                                    
       ,Orientation(..)
       
       ,MouseButton(..)
                                    
       , WDError(..), FailedCommandType(..)
       , FailedCommandInfo(..), StackFrame(..)
                                
       , Cookie(..)
        
       , Selector(..)
       ) where
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Text as Text (toLower, toUpper)
import Data.Text (Text)
import Network.Stream (ConnError)

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Word
import Data.String

newtype WD a = WD (StateT WDSession (ErrorT WDError IO) a)
  deriving (Functor, Monad, MonadState WDSession, MonadError WDError, MonadIO,
            MonadPlus, Applicative)

newtype SessionId = SessionId Text
                  deriving (Eq, Show, Read, IsString, FromJSON, ToJSON)

newtype WindowHandle = WindowHandle Text
                     deriving (Eq, Show, Read, IsString,  FromJSON, ToJSON)

newtype Element = Element Text
                  deriving (Show, Read, IsString)

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


data Orientation = Landscape | Portrait
                 deriving (Eq, Show, Ord, Bounded, Enum)

data MouseButton = LeftButton | MiddleButton | RightButton
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
      
data WDError = NoSessionId String
             | InvalidURL String
             | UnknownCommand String
             | ServerError String
             | BadJSON String
             | HTTPConnError ConnError
             | HTTPStatusUnknown (Int, Int, Int) String 
             | FailedCommand FailedCommandType FailedCommandInfo
             | WDZero String  -- used in the Error instance.
             deriving (Eq, Show)

instance Error WDError where
  noMsg =  WDZero ""
  strMsg = WDZero


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
                                           , errScreen :: Maybe String
                                           , errClass  :: Maybe String
                                           , errStack  :: [StackFrame]
                                           }
                       deriving (Eq)


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

data Selector = ById
              | ByName
              | ByClass
              | ByTagName
              | ByLinkText
              | ByPartialLinkText
              | ByCSS
              | ByXPath 


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
    where req = (o .:)            -- required field
          opt k d = o .:? k .!= d -- optional field
          b k = opt k Nothing     -- Maybe Bool field
  parseJSON v = typeMismatch "Capabilities" v

instance FromJSON FailedCommandInfo where
  parseJSON (Object o) = 
    FailedCommandInfo <$> (req "message" >>= maybe (return "") return)
                      <*> pure Nothing
                      <*> opt "screen"     Nothing
                      <*> opt "class"      Nothing
                      <*> opt "stackTrace" []
    where req = (o .:)            --required key
          opt k d = o .:? k .!= d --optional key
  parseJSON v = typeMismatch "FailedCommandInfo" v

instance FromJSON StackFrame where
  parseJSON (Object o) = StackFrame <$> reqStr "fileName"
                                    <*> reqStr "className"
                                    <*> reqStr "methodName"
                                    <*> req    "lineNumber"
    where req = (o .:) -- all keys are required
          reqStr k = req k >>= maybe (return "") return
  parseJSON v = typeMismatch "StackFrame" v

$( deriveToJSON (drop 4) ''Cookie )

instance FromJSON Cookie where
  parseJSON (Object o) = Cookie <$> req "name"
                                <*> req "value"
                                <*> opt "path" Nothing
                                <*> opt "domain" Nothing
                                <*> opt "secure" Nothing
                                <*> opt "expiry" Nothing
    where req = (o .:)
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
    let f = (obj .:)
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
  toJSON s = String $ case s of
    ById              -> "id"
    ByName            -> "name"
    ByClass           -> "class"
    ByTagName         -> "tag name"
    ByLinkText        -> "link text"
    ByPartialLinkText -> "partial link text"
    ByCSS             -> "css selector"
    ByXPath           -> "xpath"