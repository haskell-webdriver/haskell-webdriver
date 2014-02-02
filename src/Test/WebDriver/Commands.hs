{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ExistentialQuantification,
             TemplateHaskell #-}
-- |This module exports basic WD actions that can be used to interact with a
-- browser session.
module Test.WebDriver.Commands
       ( -- * Sessions
         createSession, closeSession, sessions, getCaps
         -- * Browser interaction
         -- ** Web navigation
       , openPage, forward, back, refresh
         -- ** Page info
       , getCurrentURL, getSource, getTitle, screenshot, screenshotBase64
         -- * Timeouts
       , setImplicitWait, setScriptTimeout, setPageLoadTimeout
         -- * Web elements
       , Element(..), Selector(..)
         -- ** Searching for elements
       , findElem, findElems, findElemFrom, findElemsFrom
         -- ** Interacting with elements
       , click, submit, getText
         -- *** Sending key inputs to elements
       , sendKeys, sendRawKeys, clearInput
         -- ** Element information
       , attr, cssProp, elemPos, elemSize
       , isSelected, isEnabled, isDisplayed
       , tagName, activeElem, elemInfo
         -- ** Element equality
       , (<==>), (</=>)
         -- * Javascript
       , executeJS, asyncJS
       , JSArg(..)
         -- * Windows
       , WindowHandle(..), currentWindow
       , getCurrentWindow, closeWindow, windows, focusWindow,  maximize
       , getWindowSize, setWindowSize, getWindowPos, setWindowPos
         -- * Focusing on frames
       , focusFrame, FrameSelector(..)
         -- * Cookies
       , Cookie(..), mkCookie
       , cookies, setCookie, deleteCookie, deleteVisibleCookies, deleteCookieByName
         -- * Alerts
       , getAlertText, replyToAlert, acceptAlert, dismissAlert
         -- * Mouse gestures
       , moveTo, moveToCenter, moveToFrom
       , clickWith, MouseButton(..)
       , mouseDown, mouseUp, withMouseDown, doubleClick
         -- * HTML 5 Web Storage
       , WebStorageType(..), storageSize, getAllKeys, deleteAllKeys
       , getKey, setKey, deleteKey
         -- * HTML 5 Application Cache
       , ApplicationCacheStatus(..)
       , getApplicationCacheStatus
         -- * Mobile device support
         -- ** Screen orientation
       , Orientation(..)
       , getOrientation, setOrientation
         -- ** Geo-location
       , getLocation, setLocation
         -- ** Touch gestures
       , touchClick, touchDown, touchUp, touchMove
       , touchScroll, touchScrollFrom, touchDoubleClick
       , touchLongClick, touchFlick, touchFlickFrom
         -- * IME support
       , availableIMEEngines, activeIMEEngine, checkIMEActive
       , activateIME, deactivateIME
         -- * Uploading files to remote server
         -- |These functions allow you to upload a file to a remote server.
         -- Note that this operation isn't supported by all WebDriver servers,
         -- and the location where the file is stored is not standardized.
       , uploadFile, uploadRawFile, uploadZipEntry
         -- * Server information and logs
       , serverStatus
       , getLogs, getLogTypes, LogType, LogEntry(..), LogLevel(..)
       ) where

import Test.WebDriver.Commands.Internal
import Test.WebDriver.Classes
import Test.WebDriver.JSON
import Test.WebDriver.Capabilities
import Test.WebDriver.Internal
import Test.WebDriver.Utils (urlEncode)

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import qualified Data.Text as T
import Data.Text (Text, append, toUpper, toLower)
import Data.ByteString.Base64.Lazy as B64
import Data.ByteString.Lazy as LBS (ByteString)
import Network.URI hiding (path)  -- suppresses warnings
import Codec.Archive.Zip
import qualified Data.Text.Lazy.Encoding as TL

import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Base
import Control.Exception (SomeException)
import Control.Exception.Lifted (throwIO, handle)
import qualified Control.Exception.Lifted as L
import Data.Word
import Data.String (fromString)
import Data.Maybe (fromMaybe)
import qualified Data.Char as C




-- |Convenience function to handle webdriver commands with no return value
noReturn :: WebDriver wd => wd NoReturn -> wd ()
noReturn = void

-- |Convenience function to ignore result of a webdriver command
ignoreReturn :: WebDriver wd => wd Value -> wd ()
ignoreReturn = void

-- |Create a new session with the given 'Capabilities'.
createSession :: WebDriver wd => Capabilities -> wd WDSession
createSession caps = do
  ignoreReturn . doCommand POST "/session" . single "desiredCapabilities" $ caps
  getSession

-- |Retrieve a list of active sessions and their 'Capabilities'.
sessions :: WebDriver wd => wd [(SessionId, Capabilities)]
sessions = do
  objs <- doCommand GET "/sessions" Null
  forM objs $ parsePair "id" "capabilities" "sessions"

-- |Get the actual 'Capabilities' of the current session.
getCaps :: WebDriver wd => wd Capabilities
getCaps = doSessCommand GET "" Null

-- |Close the current session and the browser associated with it.
closeSession :: WebDriver wd => wd ()
closeSession = do s <- getSession
                  noReturn $ doSessCommand DELETE "" Null
                  putSession s { wdSessId = Nothing }

-- |Sets the amount of time we implicitly wait when searching for elements.
setImplicitWait :: WebDriver wd => Integer -> wd ()
setImplicitWait ms =
  noReturn $ doSessCommand POST "/timeouts/implicit_wait" (object msField)
    `L.catch` \(_ :: SomeException) ->
      doSessCommand POST "/timeouts" (object allFields)
  where msField   = ["ms" .= ms]
        allFields = ["type" .= ("implicit" :: String)] ++ msField

-- |Sets the amount of time we wait for an asynchronous script to return a
-- result.
setScriptTimeout :: WebDriver wd => Integer -> wd ()
setScriptTimeout ms =
  noReturn $ doSessCommand POST "/timeouts/async_script" (object msField)
    `L.catch` \( _ :: SomeException) ->
      doSessCommand POST "/timeouts" (object allFields)
  where msField   = ["ms" .= ms]
        allFields = ["type" .= ("script" :: String)] ++ msField

-- |Sets the amount of time to wait for a page to finish loading before throwing a 'Timeout' exception
setPageLoadTimeout :: WebDriver wd => Integer -> wd ()
setPageLoadTimeout ms = noReturn $ doSessCommand POST "/timeouts" params
  where params = object ["type" .= ("page load" :: String)
                        ,"ms"   .= ms ]

-- |Gets the URL of the current page.
getCurrentURL :: WebDriver wd => wd String
getCurrentURL = doSessCommand GET "/url" Null

-- |Opens a new page by the given URL.
openPage :: WebDriver wd => String -> wd ()
openPage url
  | isURI url = noReturn . doSessCommand POST "/url" . single "url" $ url
  | otherwise = throwIO . InvalidURL $ url

-- |Navigate forward in the browser history.
forward :: WebDriver wd => wd ()
forward = noReturn $ doSessCommand POST "/forward" Null

-- |Navigate backward in the browser history.
back :: WebDriver wd => wd ()
back = noReturn $ doSessCommand POST "/back" Null

-- |Refresh the current page
refresh :: WebDriver wd => wd ()
refresh = noReturn $ doSessCommand POST "/refresh" Null

-- |An existential wrapper for any 'ToJSON' instance. This allows us to pass
-- parameters of many different types to Javascript code.
data JSArg = forall a. ToJSON a => JSArg a

instance ToJSON JSArg where
  toJSON (JSArg a) = toJSON a

{- |Inject a snippet of Javascript into the page for execution in the
context of the currently selected frame. The executed script is
assumed to be synchronous and the result of evaluating the script is
returned and converted to an instance of FromJSON.

The first parameter defines arguments to pass to the javascript
function. Arguments of type Element will be converted to the
corresponding DOM element. Likewise, any elements in the script result
will be returned to the client as Elements.

The second parameter defines the script itself in the form of a
function body. The value returned by that function will be returned to
the client. The function will be invoked with the provided argument
list and the values may be accessed via the arguments object in the
order specified.
-}
executeJS :: (WebDriver wd, FromJSON a) => [JSArg] -> Text -> wd a
executeJS a s = fromJSON' =<< getResult
  where
    getResult = doSessCommand POST "/execute" . pair ("args", "script") $ (a,s)

{- |Executes a snippet of Javascript code asynchronously. This function works
similarly to 'executeJS', except that the Javascript is passed a callback
function as its final argument. The script should call this function
to signal that it has finished executing, passing to it a value that will be
returned as the result of asyncJS. A result of Nothing indicates that the
Javascript function timed out (see 'setScriptTimeout')
-}
asyncJS :: (WebDriver wd, FromJSON a) => [JSArg] -> Text -> wd (Maybe a)
asyncJS a s = handle timeout $ Just <$> (fromJSON' =<< getResult)
  where
    getResult = doSessCommand POST "/execute_async" . pair ("args", "script")
                $ (a,s)
    timeout (FailedCommand Timeout _)       = return Nothing
    timeout (FailedCommand ScriptTimeout _) = return Nothing
    timeout err = throwIO err

-- |Grab a screenshot of the current page as a PNG image
screenshot :: WebDriver wd => wd LBS.ByteString
screenshot = B64.decodeLenient <$> screenshotBase64

-- |Grab a screenshot as a base-64 encoded PNG image. This is the protocol-defined format.
screenshotBase64 :: WebDriver wd => wd LBS.ByteString
screenshotBase64 = TL.encodeUtf8 <$> doSessCommand GET "/screenshot" Null

availableIMEEngines :: WebDriver wd => wd [Text]
availableIMEEngines = doSessCommand GET "/ime/available_engines" Null

activeIMEEngine :: WebDriver wd => wd Text
activeIMEEngine = doSessCommand GET "/ime/active_engine" Null

checkIMEActive :: WebDriver wd => wd Bool
checkIMEActive = doSessCommand GET "/ime/activated" Null

activateIME :: WebDriver wd => Text -> wd ()
activateIME = noReturn . doSessCommand POST "/ime/activate" . single "engine"

deactivateIME :: WebDriver wd => wd ()
deactivateIME = noReturn $ doSessCommand POST "/ime/deactivate" Null


-- |Specifies the frame used by 'Test.WebDriver.Commands.focusFrame'
data FrameSelector = WithIndex Integer
                     -- |focus on a frame by name or ID
                   | WithName Text
                     -- |focus on a frame 'Element'
                   | WithElement Element
                     -- |focus on the first frame, or the main document
                     -- if iframes are used.
                   | DefaultFrame
                   deriving (Eq, Show, Read)

instance ToJSON FrameSelector where
  toJSON s = case s of
    WithIndex i -> toJSON i
    WithName n -> toJSON n
    WithElement e -> toJSON e
    DefaultFrame -> Null

-- |Switch focus to the frame specified by the FrameSelector.
focusFrame :: WebDriver wd => FrameSelector -> wd ()
focusFrame s = noReturn $ doSessCommand POST "/frame" . single "id" $ s

-- |Returns a handle to the currently focused window
getCurrentWindow :: WebDriver wd => wd WindowHandle
getCurrentWindow = doSessCommand GET "/window_handle" Null

-- |Returns a list of all windows available to the session
windows :: WebDriver wd => wd [WindowHandle]
windows = doSessCommand GET "/window_handles" Null

focusWindow :: WebDriver wd => WindowHandle -> wd ()
focusWindow w = noReturn $ doSessCommand POST "/window" . single "name" $ w

-- |Closes the given window
closeWindow :: WebDriver wd => WindowHandle -> wd ()
closeWindow = noReturn . doSessCommand DELETE "/window" . single "name"

-- |Maximizes the current  window if not already maximized
maximize :: WebDriver wd => wd ()
maximize = noReturn $ doWinCommand GET currentWindow "/maximize" Null

-- |Get the dimensions of the current window.
getWindowSize :: WebDriver wd => wd (Word, Word)
getWindowSize = doWinCommand GET currentWindow "/size" Null
                >>= parsePair "width" "height" "getWindowSize"

-- |Set the dimensions of the current window.
setWindowSize :: WebDriver wd => (Word, Word) -> wd ()
setWindowSize = noReturn . doWinCommand POST currentWindow "/size"
                . pair ("width", "height")

-- |Get the coordinates of the current window.
getWindowPos :: WebDriver wd => wd (Int, Int)
getWindowPos = doWinCommand GET currentWindow "/position" Null
               >>= parsePair "x" "y" "getWindowPos"

-- |Set the coordinates of the current window.
setWindowPos :: WebDriver wd => (Int, Int) -> wd ()
setWindowPos = noReturn . doWinCommand POST currentWindow "/position" . pair ("x","y")

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

-- This line causes a strange out of scope error. Moving to the bottom of the
-- file fixed it.
-- $( deriveToJSON (map C.toLower . drop 4) ''Cookie )

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

-- |Retrieve all cookies visible to the current page.
cookies :: WebDriver wd => wd [Cookie]
cookies = doSessCommand GET "/cookie" Null

-- |Set a cookie. If the cookie path is not specified, it will default to \"/\".
-- Likewise, if the domain is omitted, it will default to the current page's
-- domain
setCookie :: WebDriver wd => Cookie -> wd ()
setCookie = noReturn . doSessCommand POST "/cookie" . single "cookie"

-- |Delete a cookie. This will do nothing is the cookie isn't visible to the
-- current page.
deleteCookie :: WebDriver wd => Cookie -> wd ()
deleteCookie c = noReturn $ doSessCommand DELETE ("/cookie/" `append` urlEncode (cookName c)) Null

deleteCookieByName :: WebDriver wd => Text -> wd ()
deleteCookieByName n = noReturn $ doSessCommand DELETE ("/cookie/" `append` n) Null

-- |Delete all visible cookies on the current page.
deleteVisibleCookies :: WebDriver wd => wd ()
deleteVisibleCookies = noReturn $ doSessCommand DELETE "/cookie" Null

-- |Get the current page source
getSource :: WebDriver wd => wd Text
getSource = doSessCommand GET "/source" Null

-- |Get the title of the current page.
getTitle :: WebDriver wd => wd Text
getTitle = doSessCommand GET "/title" Null

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

-- |Find an element on the page using the given element selector.
findElem :: WebDriver wd => Selector -> wd Element
findElem = doSessCommand POST "/element"

-- |Find all elements on the page matching the given selector.
findElems :: WebDriver wd => Selector -> wd [Element]
findElems = doSessCommand POST "/elements"

-- |Return the element that currently has focus.
activeElem :: WebDriver wd => wd Element
activeElem = doSessCommand POST "/element/active" Null

-- |Search for an element using the given element as root.
findElemFrom :: WebDriver wd => Element -> Selector -> wd Element
findElemFrom e = doElemCommand POST e "/element"

-- |Find all elements matching a selector, using the given element as root.
findElemsFrom :: WebDriver wd => Element -> Selector -> wd [Element]
findElemsFrom e = doElemCommand POST e "/elements"

-- |Describe the element. Returns a JSON object whose meaning is currently
-- undefined by the WebDriver protocol.
elemInfo :: WebDriver wd => Element -> wd Value
elemInfo e = doElemCommand GET e "" Null

-- |Click on an element.
click :: WebDriver wd => Element -> wd ()
click e = noReturn $ doElemCommand POST e "/click" Null

-- |Submit a form element. This may be applied to descendents of a form element
-- as well.
submit :: WebDriver wd => Element -> wd ()
submit e = noReturn $ doElemCommand POST e "/submit" Null

-- |Get all visible text within this element.
getText :: WebDriver wd => Element -> wd Text
getText e = doElemCommand GET e "/text" Null

-- |Send a sequence of keystrokes to an element. All modifier keys are released
-- at the end of the function. For more information about modifier keys, see
-- <http://code.google.com/p/selenium/wiki/JsonWireProtocol#/session/:sessionId/element/:id/value>
sendKeys :: WebDriver wd => Text -> Element -> wd ()
sendKeys t e = noReturn . doElemCommand POST e "/value" . single "value" $ [t]

-- |Similar to sendKeys, but doesn't implicitly release modifier keys
-- afterwards. This allows you to combine modifiers with mouse clicks.
sendRawKeys :: WebDriver wd => Text -> Element -> wd ()
sendRawKeys t e = noReturn . doElemCommand POST e "/keys" . single "value" $ [t]

-- |Return the tag name of the given element.
tagName :: WebDriver wd => Element -> wd Text
tagName e = doElemCommand GET e "/name" Null

-- |Clear a textarea or text input element's value.
clearInput :: WebDriver wd => Element -> wd ()
clearInput e = noReturn $ doElemCommand POST e "/clear" Null

-- |Determine if the element is selected.
isSelected :: WebDriver wd => Element -> wd Bool
isSelected e = doElemCommand GET e "/selected" Null

-- |Determine if the element is enabled.
isEnabled :: WebDriver wd => Element -> wd Bool
isEnabled e = doElemCommand GET e "/enabled" Null

-- |Determine if the element is displayed.
isDisplayed :: WebDriver wd => Element -> wd Bool
isDisplayed e = doElemCommand GET e "/displayed" Null

-- |Retrieve the value of an element's attribute
attr :: WebDriver wd => Element -> Text -> wd (Maybe Text)
attr e t = doElemCommand GET e ("/attribute/" `append` urlEncode t) Null

-- |Retrieve the value of an element's computed CSS property
cssProp :: WebDriver wd => Element -> Text -> wd (Maybe Text)
cssProp e t = doElemCommand GET e ("/css/" `append` urlEncode t) Null

-- |Retrieve an element's current position.
elemPos :: WebDriver wd => Element -> wd (Int, Int)
elemPos e = doElemCommand GET e "/location" Null >>= parsePair "x" "y" "elemPos"

-- |Retrieve an element's current size.
elemSize :: WebDriver wd => Element -> wd (Word, Word)
elemSize e = doElemCommand GET e "/size" Null
             >>= parsePair "width" "height" "elemSize"

infix 4 <==>
-- |Determines if two element identifiers refer to the same element.
(<==>) :: WebDriver wd => Element -> Element -> wd Bool
e1 <==> (Element e2) = doElemCommand GET e1 ("/equals/" `append` urlEncode e2) Null

-- |Determines if two element identifiers refer to different elements.
infix 4 </=>
(</=>) :: WebDriver wd => Element -> Element -> wd Bool
e1 </=> e2 = not <$> (e1 <==> e2)

-- |A screen orientation
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

-- |Get the current screen orientation for rotatable display devices.
getOrientation :: WebDriver wd => wd Orientation
getOrientation = doSessCommand GET "/orientation" Null

-- |Set the current screen orientation for rotatable display devices.
setOrientation :: WebDriver wd => Orientation -> wd ()
setOrientation = noReturn . doSessCommand POST "/orientation" . single "orientation"

-- |Get the text of an alert dialog.
getAlertText :: WebDriver wd => wd Text
getAlertText = doSessCommand GET "/alert_text" Null

-- |Sends keystrokes to Javascript prompt() dialog.
replyToAlert :: WebDriver wd => Text -> wd ()
replyToAlert = noReturn . doSessCommand POST "/alert_text" . single "text"

-- |Accepts the currently displayed alert dialog.
acceptAlert :: WebDriver wd => wd ()
acceptAlert = noReturn $ doSessCommand POST "/accept_alert" Null

-- |Dismisses the currently displayed alert dialog.
dismissAlert :: WebDriver wd => wd ()
dismissAlert = noReturn $ doSessCommand POST "/dismiss_alert" Null

-- |Moves the mouse to the given position relative to the active element.
moveTo :: WebDriver wd => (Int, Int) -> wd ()
moveTo = noReturn . doSessCommand POST "/moveto" . pair ("xoffset","yoffset")

-- |Moves the mouse to the center of a given element.
moveToCenter :: WebDriver wd => Element -> wd ()
moveToCenter (Element e) =
  noReturn . doSessCommand POST "/moveto" . single "element" $ e

-- |Moves the mouse to the given position relative to the given element.
moveToFrom :: WebDriver wd => (Int, Int) -> Element -> wd ()
moveToFrom (x,y) (Element e) =
  noReturn . doSessCommand POST "/moveto"
  . triple ("element","xoffset","yoffset") $ (e,x,y)

-- |A mouse button
data MouseButton = LeftButton | MiddleButton | RightButton
                 deriving (Eq, Show, Ord, Bounded, Enum)

instance ToJSON MouseButton where
  toJSON = toJSON . fromEnum

instance FromJSON MouseButton where
  parseJSON v = do
    n <- parseJSON v
    case n :: Integer of
      0 -> return LeftButton
      1 -> return MiddleButton
      2 -> return RightButton
      err -> fail $ "Invalid JSON for MouseButton: " ++ show err

-- |Click at the current mouse position with the given mouse button.
clickWith :: WebDriver wd => MouseButton -> wd ()
clickWith = noReturn . doSessCommand POST "/click" . single "button"

-- |Perform the given action with the left mouse button held down. The mouse
-- is automatically released afterwards.
withMouseDown :: WebDriver wd => wd a -> wd a
withMouseDown wd = mouseDown >> wd <* mouseUp

-- |Press and hold the left mouse button down. Note that undefined behavior
-- occurs if the next mouse command is not mouseUp.
mouseDown :: WebDriver wd => wd ()
mouseDown = noReturn $ doSessCommand POST "/buttondown" Null

-- |Release the left mouse button.
mouseUp :: WebDriver wd => wd ()
mouseUp = noReturn $ doSessCommand POST "/buttonup" Null

-- |Double click at the current mouse location.
doubleClick :: WebDriver wd => wd ()
doubleClick = noReturn $ doSessCommand POST "/doubleclick" Null

-- |Single tap on the touch screen at the given element's location.
touchClick :: WebDriver wd => Element -> wd ()
touchClick (Element e) =
  noReturn . doSessCommand POST "/touch/click" . single "element" $ e

-- |Emulates pressing a finger down on the screen at the given location.
touchDown :: WebDriver wd => (Int, Int) -> wd ()
touchDown = noReturn . doSessCommand POST "/touch/down" . pair ("x","y")

-- |Emulates removing a finger from the screen at the given location.
touchUp :: WebDriver wd => (Int, Int) -> wd ()
touchUp = noReturn . doSessCommand POST "/touch/up" . pair ("x","y")

-- |Emulates moving a finger on the screen to the given location.
touchMove :: WebDriver wd => (Int, Int) -> wd ()
touchMove = noReturn . doSessCommand POST "/touch/move" . pair ("x","y")

-- |Emulate finger-based touch scroll. Use this function if you don't care where
-- the scroll begins
touchScroll :: WebDriver wd => (Int, Int) -> wd ()
touchScroll = noReturn . doSessCommand POST "/touch/scroll" . pair ("xoffset","yoffset")

-- |Emulate finger-based touch scroll, starting from the given location relative
-- to the given element.
touchScrollFrom :: WebDriver wd => (Int, Int) -> Element -> wd ()
touchScrollFrom (x, y) (Element e) =
  noReturn 
  . doSessCommand POST "/touch/scroll"
  . triple ("xoffset", "yoffset", "element")
  $ (x, y, e)

-- |Emulate a double click on a touch device.
touchDoubleClick :: WebDriver wd => Element -> wd ()
touchDoubleClick (Element e) = 
  noReturn
  . doSessCommand POST "/touch/doubleclick"
  . single "element" $ e

-- |Emulate a long click on a touch device.
touchLongClick :: WebDriver wd => Element -> wd ()
touchLongClick (Element e) = 
  noReturn 
  . doSessCommand POST "/touch/longclick"
  . single "element" $ e
-- |Emulate a flick on the touch screen. The coordinates indicate x and y
-- velocity, respectively. Use this function if you don't care where the
-- flick starts.
touchFlick :: WebDriver wd => (Int, Int) -> wd ()
touchFlick = 
  noReturn 
  . doSessCommand POST "/touch/flick"
  . pair ("xSpeed", "ySpeed")

-- |Emulate a flick on the touch screen.
touchFlickFrom :: WebDriver wd =>
                  Int           -- ^ flick velocity
                  -> (Int, Int) -- ^ a location relative to the given element
                  -> Element    -- ^ the given element
                  -> wd ()
touchFlickFrom s (x,y) (Element e) =
  noReturn
  . doSessCommand POST "/touch/flick" . object $
  ["xoffset" .= x
  ,"yoffset" .= y
  ,"speed"   .= s
  ,"element" .= e
  ]

-- |Get the current geographical location of the device.
getLocation :: WebDriver wd => wd (Int, Int, Int)
getLocation = doSessCommand GET "/location" Null
              >>= parseTriple "latitude" "longitude" "altitude" "getLocation"

-- |Set the current geographical location of the device.
setLocation :: WebDriver wd => (Int, Int, Int) -> wd ()
setLocation = noReturn . doSessCommand POST "/location" 
              . triple ("latitude",
                        "longitude",
                        "altitude")

-- |Uploads a file from the local filesystem by its file path.
uploadFile :: WebDriver wd => FilePath -> wd ()
uploadFile path = uploadZipEntry =<< liftBase (readEntry [] path)

-- |Uploads a raw bytestring with associated file info.
uploadRawFile :: WebDriver wd =>
                 FilePath          -- ^File path to use with this bytestring.
                 -> Integer        -- ^Modification time
                                   -- (in seconds since Unix epoch).
                 -> LBS.ByteString -- ^ The file contents as a lazy ByteString
                 -> wd ()
uploadRawFile path t str = uploadZipEntry (toEntry path t str)


-- |Lowest level interface to the file uploading mechanism.
-- This allows you to specify the exact details of
-- the zip entry sent across network.
uploadZipEntry :: WebDriver wd => Entry -> wd ()
uploadZipEntry = noReturn . doSessCommand POST "/file" . single "file"
                 . TL.decodeUtf8 . B64.encode . fromArchive . (`addEntryToArchive` emptyArchive)


-- |Get the current number of keys in a web storage area.
storageSize :: WebDriver wd => WebStorageType -> wd Integer
storageSize s = doStorageCommand GET s "/size" Null

-- |Get a list of all keys from a web storage area.
getAllKeys :: WebDriver wd => WebStorageType -> wd [Text]
getAllKeys s = doStorageCommand GET s "" Null

-- |Delete all keys within a given web storage area.
deleteAllKeys :: WebDriver wd => WebStorageType -> wd ()
deleteAllKeys s = noReturn $ doStorageCommand DELETE s "" Null

-- |An HTML 5 storage type
data WebStorageType = LocalStorage | SessionStorage
                    deriving (Eq, Show, Ord, Bounded, Enum)

-- |Get the value associated with a key in the given web storage area.
-- Unset keys result in empty strings, since the Web Storage spec
-- makes no distinction between the empty string and an undefined value.
getKey :: WebDriver wd => WebStorageType -> Text ->  wd Text
getKey s k = doStorageCommand GET s ("/key/" `T.append` urlEncode k) Null

-- |Set a key in the given web storage area.
setKey :: WebDriver wd => WebStorageType -> Text -> Text -> wd Text
setKey s k v = doStorageCommand POST s "" . object $ ["key"   .= k,
                                                      "value" .= v ]
-- |Delete a key in the given web storage area.
deleteKey :: WebDriver wd => WebStorageType -> Text -> wd ()
deleteKey s k = noReturn $ doStorageCommand POST s ("/key/" `T.append` urlEncode k) Null

-- |A wrapper around 'doStorageCommand' to create web storage URLs.
doStorageCommand :: (WebDriver wd, ToJSON a, FromJSON b) =>
                     RequestMethod -> WebStorageType -> Text -> a -> wd b
doStorageCommand m s path a = doSessCommand m (T.concat ["/", s', path]) a
  where s' = case s of
          LocalStorage -> "local_storage"
          SessionStorage -> "session_storage"

-- |Get information from the server as a JSON 'Object'. For more information
-- about this object see
-- <http://code.google.com/p/selenium/wiki/JsonWireProtocol#/status>
serverStatus :: (WebDriver wd) => wd Value   -- todo: make this a record type
serverStatus = doCommand GET "/status" Null

-- |A record that represents a single log entry.
data LogEntry = 
  LogEntry { logTime  :: Integer  -- ^ timestamp for the log entry. The standard 
                                  -- does not specify the epoch or the unit of 
                                  -- time.
           , logLevel :: LogLevel -- ^ log verbosity level
           , logMsg   :: Text
           }
  deriving (Eq, Ord, Show, Read)


instance FromJSON LogEntry where
  parseJSON (Object o) =
    LogEntry <$> o .: "timestamp"
             <*> o .: "level"
             <*> (fromMaybe "" <$> o .: "message")
  parseJSON v = typeMismatch "LogEntry" v

type LogType = String

-- |Retrieve the log buffer for a given log type. The server-side log buffer is reset after each request.
--
-- Which log types are available is server defined, but the wire protocol lists these as common log types:
-- client, driver, browser, server
getLogs :: WebDriver wd => LogType -> wd [LogEntry]
getLogs t = doSessCommand POST "/log" . object $ ["type" .= t]

-- |Get a list of available log types.
getLogTypes :: WebDriver wd => wd [LogType]
getLogTypes = doSessCommand GET "/log/types" Null

data ApplicationCacheStatus = Uncached | Idle | Checking | Downloading | UpdateReady | Obsolete deriving (Eq, Enum, Bounded, Ord, Show, Read)

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
            
getApplicationCacheStatus :: (WebDriver wd) => wd ApplicationCacheStatus
getApplicationCacheStatus = doSessCommand GET "/application_cache/status" Null

-- Moving this closer to the definition of Cookie seems to cause strange compile
-- errors, so I'm leaving it here for now.
$( deriveToJSON (defaultOptions{fieldLabelModifier = map C.toLower . drop 4}) ''Cookie )
