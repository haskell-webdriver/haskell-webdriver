
-- | This module exports basic WD actions that can be used to interact with a
-- browser session.
module Test.WebDriver.Commands (
  -- * Sessions
  -- | A session is equivalent to a single instantiation of a
  -- particular user agent, including all its child browsers. WebDriver gives
  -- each session a unique session ID that can be used to differentiate one
  -- session from another, allowing multiple user agents to be controlled from a
  -- single HTTP server, and allowing sessions to be routed via a multiplexer
  -- (known as an intermediary node).
  --
  -- See https://www.w3.org/TR/webdriver1/#sessions.
  module Test.WebDriver.Commands.Sessions

  -- * Navigation
  -- | The commands in this section allow navigation of the current top-level
  -- browsing context to new URLs and introspection of the document currently
  -- loaded in this browsing context.
  --
  -- See https://www.w3.org/TR/webdriver1/#navigation.
  , module Test.WebDriver.Commands.Navigation

  -- * Command contexts
  -- | Many WebDriver commands happen in the context of either the current
  -- browsing context or current top-level browsing context. The current
  -- top-level browsing context is represented in the protocol by its associated
  -- window handle. When a top-level browsing context is selected using the
  -- Switch To Window command, a specific browsing context can be selected using
  -- the Switch to Frame command.
  --
  -- See https://www.w3.org/TR/webdriver1/#command-contexts.
  , module Test.WebDriver.Commands.CommandContexts

  -- * Element Retrieval
  -- | The Find Element, Find Elements, Find Element From Element, and Find
  -- Elements From Element commands allow lookup of individual elements and
  -- collections of elements. Element retrieval searches are performed using
  -- pre-order traversal of the document’s nodes that match the provided
  -- selector’s expression. Elements are serialized and returned as web
  -- elements.
  --
  -- See https://www.w3.org/TR/webdriver1/#element-retrieval.
  , module Test.WebDriver.Commands.ElementRetrieval

  -- * Element State
  --
  -- See https://www.w3.org/TR/webdriver1/#element-state.
  , module Test.WebDriver.Commands.ElementState

  -- * Element Interaction
  --
  -- See https://www.w3.org/TR/webdriver1/#element-interaction.
  , module Test.WebDriver.Commands.ElementInteraction

  -- * Document handling
  --
  -- See https://www.w3.org/TR/webdriver1/#document-handling.
  , module Test.WebDriver.Commands.DocumentHandling

  -- * Cookies
  --
  -- See https://www.w3.org/TR/webdriver1/#cookies.
  , module Test.WebDriver.Commands.Cookies

  -- * Actions
  --
  -- See https://www.w3.org/TR/webdriver1/#actions.
  , module Test.WebDriver.Commands.Actions

  -- * User Prompts
  --
  -- See https://www.w3.org/TR/webdriver1/#user-prompts.
  , module Test.WebDriver.Commands.UserPrompts

  -- * Screen capture
  --
  -- See https://www.w3.org/TR/webdriver1/#screen-capture.
  , module Test.WebDriver.Commands.ScreenCapture

  -- * Timeouts
  , setImplicitWait
  , setScriptTimeout
  , setPageLoadTimeout

  -- * Web elements
  -- ** Interacting with elements
  , submit
  -- *** Sending key inputs to elements
  -- ** Element information
  , isDisplayed
  -- ** Element equality
  , (<==>)
  , (</=>)

  -- * HTML 5 Web Storage
  , storageSize
  , getAllKeys
  , deleteAllKeys
  , getKey
  , setKey
  , deleteKey
  , WebStorageType(..)

  -- * HTML 5 Application Cache
  , ApplicationCacheStatus(..)
  , getApplicationCacheStatus

  -- * Mobile device support
  -- ** Screen orientation
  , Orientation(..)
  , getOrientation
  , setOrientation
  -- ** Geo-location
  , getLocation
  , setLocation
  -- ** Touch gestures
  , touchClick
  , touchDown
  , touchUp
  , touchMove
  , touchScroll
  , touchScrollFrom
  , touchDoubleClick
  , touchLongClick
  , touchFlick
  , touchFlickFrom

  -- * IME support
  , availableIMEEngines
  , activeIMEEngine
  , checkIMEActive
  , activateIME
  , deactivateIME

  -- * Uploading files to remote server
  -- | These functions allow you to upload a file to a remote server.
  -- Note that this operation isn't supported by all WebDriver servers,
  -- and the location where the file is stored is not standardized.
  , uploadFile
  , uploadRawFile
  , uploadZipEntry

  -- * Server information and logs
  , getLogs
  , getLogTypes
  , LogType
  , LogEntry(..)
  , LogLevel(..)
  ) where

import Codec.Archive.Zip
import Control.Applicative
import Control.Exception (SomeException)
import qualified Control.Exception.Safe as L
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson as A
import Data.Aeson.Types
import Data.ByteString.Base64.Lazy as B64
import Data.ByteString.Lazy as LBS (ByteString)
import Data.CallStack
import Data.String (fromString)
import Data.Text (Text, append, toUpper, toLower)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import Prelude -- hides some "unused import" warnings
import Test.WebDriver.Class
import Test.WebDriver.Commands.Actions
import Test.WebDriver.Commands.CommandContexts
import Test.WebDriver.Commands.Cookies
import Test.WebDriver.Commands.DocumentHandling
import Test.WebDriver.Commands.ElementInteraction
import Test.WebDriver.Commands.ElementRetrieval
import Test.WebDriver.Commands.ElementState
import Test.WebDriver.Commands.Internal
import Test.WebDriver.Commands.LoggingTypes
import Test.WebDriver.Commands.Navigation
import Test.WebDriver.Commands.ScreenCapture
import Test.WebDriver.Commands.Sessions
import Test.WebDriver.Commands.UserPrompts
import Test.WebDriver.JSON
import Test.WebDriver.Utils (urlEncode)


-- | Sets the amount of time (ms) we implicitly wait when searching for elements.
setImplicitWait :: (HasCallStack, WebDriver wd) => Integer -> wd ()
setImplicitWait ms =
  ignoreReturn $ doSessCommand methodPost "/timeouts/implicit_wait" (object msField)
    `L.catch` \(_ :: SomeException) ->
      doSessCommand methodPost "/timeouts" (object allFields)
  where msField   = ["ms" .= ms]
        allFields = ["type" .= ("implicit" :: String)] ++ msField

-- | Sets the amount of time (ms) we wait for an asynchronous script to return a
-- result.
setScriptTimeout :: (HasCallStack, WebDriver wd) => Integer -> wd ()
setScriptTimeout ms =
  ignoreReturn $ doSessCommand methodPost "/timeouts/async_script" (object msField)
    `L.catch` \( _ :: SomeException) ->
      doSessCommand methodPost "/timeouts" (object allFields)
  where msField   = ["ms" .= ms]
        allFields = ["type" .= ("script" :: String)] ++ msField

-- | Sets the amount of time (ms) to wait for a page to finish loading before throwing a 'Timeout' exception.
setPageLoadTimeout :: (HasCallStack, WebDriver wd) => Integer -> wd ()
setPageLoadTimeout ms = ignoreReturn $ doSessCommand methodPost "/timeouts" params
  where params = object ["type" .= ("page load" :: String)
                        ,"ms"   .= ms ]

availableIMEEngines :: (HasCallStack, WebDriver wd) => wd [Text]
availableIMEEngines = doSessCommand methodGet "/ime/available_engines" Null

activeIMEEngine :: (HasCallStack, WebDriver wd) => wd Text
activeIMEEngine = doSessCommand methodGet "/ime/active_engine" Null

checkIMEActive :: (HasCallStack, WebDriver wd) => wd Bool
checkIMEActive = doSessCommand methodGet "/ime/activated" Null

activateIME :: (HasCallStack, WebDriver wd) => Text -> wd ()
activateIME = noReturn . doSessCommand methodPost "/ime/activate" . single "engine"

deactivateIME :: (HasCallStack, WebDriver wd) => wd ()
deactivateIME = noReturn $ doSessCommand methodPost "/ime/deactivate" Null

-- | Submit a form element. This may be applied to descendents of a form element
-- as well.
submit :: (HasCallStack, WebDriver wd) => Element -> wd ()
submit e = noReturn $ doElemCommand methodPost e "/submit" Null

-- | Determine if the element is displayed.
isDisplayed :: (HasCallStack, WebDriver wd) => Element -> wd Bool
isDisplayed e = doElemCommand methodGet e "/displayed" Null

infix 4 <==>
-- | Determines if two element identifiers refer to the same element.
(<==>) :: (HasCallStack, WebDriver wd) => Element -> Element -> wd Bool
e1 <==> (Element e2) = doElemCommand methodGet e1 ("/equals/" `append` urlEncode e2) Null

-- | Determines if two element identifiers refer to different elements.
infix 4 </=>
(</=>) :: (HasCallStack, WebDriver wd) => Element -> Element -> wd Bool
e1 </=> e2 = not <$> (e1 <==> e2)

-- | A screen orientation
data Orientation = Landscape | Portrait
                 deriving (Eq, Show, Ord, Bounded, Enum)

instance ToJSON Orientation where
  toJSON = String . toUpper . fromString . show

instance FromJSON Orientation where
  parseJSON (String jStr) = case toLower jStr of
    "landscape" -> return Landscape
    "portrait"  -> return Portrait
    err         -> fail $ "Invalid Orientation string " ++ show err
  parseJSON v = typeMismatch "Orientation" v

-- | Get the current screen orientation for rotatable display devices.
getOrientation :: (HasCallStack, WebDriver wd) => wd Orientation
getOrientation = doSessCommand methodGet "/orientation" Null

-- | Set the current screen orientation for rotatable display devices.
setOrientation :: (HasCallStack, WebDriver wd) => Orientation -> wd ()
setOrientation = noReturn . doSessCommand methodPost "/orientation" . single "orientation"

-- | Single tap on the touch screen at the given element's location.
touchClick :: (HasCallStack, WebDriver wd) => Element -> wd ()
touchClick (Element e) =
  noReturn . doSessCommand methodPost "/touch/click" . single "element" $ e

-- | Emulates pressing a finger down on the screen at the given location.
touchDown :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
touchDown = noReturn . doSessCommand methodPost "/touch/down" . pair ("x","y")

-- | Emulates removing a finger from the screen at the given location.
touchUp :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
touchUp = noReturn . doSessCommand methodPost "/touch/up" . pair ("x","y")

-- | Emulates moving a finger on the screen to the given location.
touchMove :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
touchMove = noReturn . doSessCommand methodPost "/touch/move" . pair ("x","y")

-- | Emulate finger-based touch scroll. Use this function if you don't care where
-- the scroll begins
touchScroll :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
touchScroll = noReturn . doSessCommand methodPost "/touch/scroll" . pair ("xoffset","yoffset")

-- | Emulate finger-based touch scroll, starting from the given location relative
-- to the given element.
touchScrollFrom :: (HasCallStack, WebDriver wd) => (Int, Int) -> Element -> wd ()
touchScrollFrom (x, y) (Element e) =
  noReturn
  . doSessCommand methodPost "/touch/scroll"
  . triple ("xoffset", "yoffset", "element")
  $ (x, y, e)

-- | Emulate a double click on a touch device.
touchDoubleClick :: (HasCallStack, WebDriver wd) => Element -> wd ()
touchDoubleClick (Element e) =
  noReturn
  . doSessCommand methodPost "/touch/doubleclick"
  . single "element" $ e

-- | Emulate a long click on a touch device.
touchLongClick :: (HasCallStack, WebDriver wd) => Element -> wd ()
touchLongClick (Element e) =
  noReturn
  . doSessCommand methodPost "/touch/longclick"
  . single "element" $ e
-- | Emulate a flick on the touch screen. The coordinates indicate x and y
-- velocity, respectively. Use this function if you don't care where the
-- flick starts.
touchFlick :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
touchFlick =
  noReturn
  . doSessCommand methodPost "/touch/flick"
  . pair ("xSpeed", "ySpeed")

-- | Emulate a flick on the touch screen.
touchFlickFrom :: (HasCallStack, WebDriver wd) =>
                  Int           -- ^ flick velocity
                  -> (Int, Int) -- ^ a location relative to the given element
                  -> Element    -- ^ the given element
                  -> wd ()
touchFlickFrom s (x,y) (Element e) =
  noReturn
  . doSessCommand methodPost "/touch/flick" . object $
  ["xoffset" .= x
  ,"yoffset" .= y
  ,"speed"   .= s
  ,"element" .= e
  ]

-- | Get the current geographical location of the device.
getLocation :: (HasCallStack, WebDriver wd) => wd (Int, Int, Int)
getLocation = doSessCommand methodGet "/location" Null
              >>= parseTriple "latitude" "longitude" "altitude" "getLocation"

-- | Set the current geographical location of the device.
setLocation :: (HasCallStack, WebDriver wd) => (Int, Int, Int) -> wd ()
setLocation = noReturn . doSessCommand methodPost "/location"
              . triple ("latitude",
                        "longitude",
                        "altitude")

-- | Uploads a file from the local filesystem by its file path.
-- Returns the remote filepath of the file
uploadFile :: (HasCallStack, WebDriver wd) => FilePath -> wd Text
uploadFile path = uploadZipEntry =<< liftIO (readEntry [] path)

-- | Uploads a raw bytestring with associated file info.
-- Returns the remote filepath of the file
uploadRawFile :: (HasCallStack, WebDriver wd) =>
                 FilePath          -- ^File path to use with this bytestring.
                 -> Integer        -- ^Modification time
                                   -- (in seconds since Unix epoch).
                 -> LBS.ByteString -- ^ The file contents as a lazy ByteString
                 -> wd Text
uploadRawFile path t str = uploadZipEntry (toEntry path t str)


-- | Lowest level interface to the file uploading mechanism.
-- This allows you to specify the exact details of
-- the zip entry sent across network.
-- Returns the remote filepath of the extracted file
uploadZipEntry :: (HasCallStack, WebDriver wd) => Entry -> wd Text
uploadZipEntry = doSessCommand methodPost "/se/file" . single "file"
                 . TL.decodeUtf8 . B64.encode . fromArchive . (`addEntryToArchive` emptyArchive)


-- | Get the current number of keys in a web storage area.
storageSize :: (HasCallStack, WebDriver wd) => WebStorageType -> wd Integer
storageSize s = doStorageCommand methodGet s "/size" Null

-- | Get a list of all keys from a web storage area.
getAllKeys :: (HasCallStack, WebDriver wd) => WebStorageType -> wd [Text]
getAllKeys s = doStorageCommand methodGet s "" Null

-- | Delete all keys within a given web storage area.
deleteAllKeys :: (HasCallStack, WebDriver wd) => WebStorageType -> wd ()
deleteAllKeys s = noReturn $ doStorageCommand methodDelete s "" Null

-- | An HTML 5 storage type
data WebStorageType = LocalStorage | SessionStorage
  deriving (Eq, Show, Ord, Bounded, Enum)

-- | Get the value associated with a key in the given web storage area.
-- Unset keys result in empty strings, since the Web Storage spec
-- makes no distinction between the empty string and an undefined value.
getKey :: (HasCallStack, WebDriver wd) => WebStorageType -> Text ->  wd Text
getKey s k = doStorageCommand methodGet s ("/key/" `T.append` urlEncode k) Null

-- | Set a key in the given web storage area.
setKey :: (HasCallStack, WebDriver wd) => WebStorageType -> Text -> Text -> wd Text
setKey s k v = doStorageCommand methodPost s "" . object $ ["key"   .= k,
                                                      "value" .= v ]
-- | Delete a key in the given web storage area.
deleteKey :: (HasCallStack, WebDriver wd) => WebStorageType -> Text -> wd ()
deleteKey s k = noReturn $ doStorageCommand methodPost s ("/key/" `T.append` urlEncode k) Null

-- | A wrapper around 'doSessCommand' to create web storage requests.
doStorageCommand :: (
  HasCallStack, WebDriver wd, ToJSON a, FromJSON b, ToJSON b
  ) => Method -> WebStorageType -> Text -> a -> wd b
doStorageCommand m s path a = doSessCommand m (T.concat ["/", s', path]) a
  where s' = case s of
          LocalStorage -> "local_storage"
          SessionStorage -> "session_storage"

-- | Retrieve the log buffer for a given log type. The server-side log buffer is reset after each request.
--
-- Which log types are available is server defined, but the wire protocol lists these as common log types:
-- client, driver, browser, server
getLogs :: (HasCallStack, WebDriver wd) => LogType -> wd [LogEntry]
getLogs t = doSessCommand methodPost "/log" . object $ ["type" .= t]

-- | Get a list of available log types.
getLogTypes :: (HasCallStack, WebDriver wd) => wd [LogType]
getLogTypes = doSessCommand methodGet "/log/types" Null

data ApplicationCacheStatus =
  Uncached
  | Idle
  | Checking
  | Downloading
  | UpdateReady
  | Obsolete
  deriving (Eq, Enum, Bounded, Ord, Show, Read)

instance FromJSON ApplicationCacheStatus where
  parseJSON val = do
    n <- parseJSON val
    case n :: Integer of
      0 -> return Uncached
      1 -> return Idle
      2 -> return Checking
      3 -> return Downloading
      4 -> return UpdateReady
      5 -> return Obsolete
      err -> fail $ "Invalid JSON for ApplicationCacheStatus: " ++ show err

instance ToJSON ApplicationCacheStatus where
  toJSON Uncached = A.Number 0
  toJSON Idle = A.Number 1
  toJSON Checking = A.Number 2
  toJSON Downloading = A.Number 3
  toJSON UpdateReady = A.Number 4
  toJSON Obsolete = A.Number 5

getApplicationCacheStatus :: (HasCallStack, WebDriver wd) => wd ApplicationCacheStatus
getApplicationCacheStatus = doSessCommand methodGet "/application_cache/status" Null
