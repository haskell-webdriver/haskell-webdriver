{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- |This module exports basic WD actions that can be used to interact with a
-- browser session.
module Test.WebDriver.Commands 
       ( -- * Sessions
         createSession, closeSession, sessions, getCaps
         -- * Browser interaction
         -- ** Web navigation
       , openPage, forward, back, refresh
         -- ** Page info
       , getCurrentURL, getSource, getTitle, screenshot                    
         -- * Timeouts
       , setImplicitWait, setScriptTimeout
         -- * Web elements
       , Element(..), Selector(..)
         -- ** Searching for elements
       , findElem, findElems, findElemFrom, findElemsFrom
         -- ** Interacting with elements
       , click, submit, getText
       , sendKeys, sendRawKeys, clearInput
         -- ** Element information
       , attr, cssProp, elemPos, elemSize
       , isSelected, isEnabled, isDisplayed
       , tagName, activeElem, elemInfo
         -- ** Element equality
       , (<==>), (</=>)
         -- * Focus on windows and frames
       , focus, FocusSelector(..)
         -- * Javascript            
       , executeJS, asyncJS
       , JSArg(..)
         -- * Windows                                                       
       , WindowHandle(..), currentWindow
       , getCurrentWindow, closeWindow, windows
       , getWindowSize, setWindowSize, getWindowPos, setWindowPos
         -- * Cookies
       , Cookie(..)
       , cookies, setCookie, deleteCookie, deleteVisibleCookies
         -- * Alerts
       , getAlertText, replyToAlert, acceptAlert, dismissAlert
         -- * Mouse gestures                                          
       , moveTo, moveToCenter, moveToFrom
       , clickWith, MouseButton(..)
       , mouseDown, mouseUp, withMouseDown, doubleClick
         -- * Touch gestures
       , touchClick, touchDown, touchUp, touchMove
       , touchScroll, touchScrollFrom, touchDoubleClick
       , touchLongClick, touchFlick, touchFlickFrom
         -- * Screen orientation
       , Orientation(..)
       , getOrientation, setOrientation
         -- * Geo-location
       , getLocation, setLocation
         -- * IME support              
       , availableIMEEngines, activeIMEEngine, checkIMEActive
       , activateIME, deactivateIME
         -- * Server information
       , serverStatus
       ) where

import Test.WebDriver.Types
import Test.WebDriver.Commands.Internal
import Test.WebDriver.JSON

import Data.Aeson
import qualified Data.Text as T
import Data.Text (Text, splitOn, append)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 as B64
import Network.URI

import Control.Applicative
import Control.Monad.State.Strict
import Control.Exception (SomeException)
import Control.Exception.Lifted (throwIO, catch, handle)
import Data.Word

import Prelude hiding (catch)


-- |Get information from the server as a JSON 'Object'. For more information 
-- about this object see 
-- <http://code.google.com/p/selenium/wiki/JsonWireProtocol#/status>
serverStatus :: WD Value   -- todo: make this a record type
serverStatus = doCommand GET "/status" ()


-- |Create a new session with the given 'Capabilities'. This command
-- resets the current session ID to that of the new session.
createSession :: Capabilities -> WD WDSession
createSession caps = do
  sessUrl <- doCommand POST "/session" . single "desiredCapabilities" $ caps
  let sessId = SessionId . last . filter (not . T.null) . splitOn "/" $  sessUrl
  modify $ \sess -> sess {wdSessId = Just sessId}
  return =<< get

-- |Retrieve a list of active sessions and their 'Capabilities'.
sessions :: WD [(SessionId, Capabilities)]
sessions = do
  objs <- doCommand GET "/sessions" ()
  forM objs $ parsePair "id" "capabilities" "sessions"

-- |Get the actual 'Capabilities' of the current session.
getCaps :: WD Capabilities
getCaps = doSessCommand GET "" ()

-- |Close the current session and the browser associated with it.
closeSession :: WD ()
closeSession = do s <- get
                  doSessCommand DELETE "" () :: WD ()
                  put s { wdSessId = Nothing } 

-- |Sets the amount of time we implicitly wait when searching for 'Elements'.
setImplicitWait :: Integer -> WD ()
setImplicitWait ms = 
  doSessCommand POST "/timeouts/implicit_wait" (object msField)
    `catch` \(_ :: SomeException) ->  
      doSessCommand POST "/timeouts" (object allFields)
  where msField   = ["ms" .= ms] 
        allFields = ["type" .= ("implicit" :: String)] ++ msField

-- |Sets the amount of time we wait for an asynchronous script to return a 
-- result
setScriptTimeout :: Integer -> WD () 
setScriptTimeout ms =
  doSessCommand POST "/timeouts/async_script" (object msField)
    `catch` \(_ :: SomeException) ->  
      doSessCommand POST "/timeouts" (object allFields)
  where msField   = ["ms" .= ms]
        allFields = ["type" .= ("script" :: String)] ++ msField

-- |Gets the URL of the current page.
getCurrentURL :: WD String
getCurrentURL = doSessCommand GET "/url" ()

-- |Opens a new page by the given URL.
openPage :: String -> WD ()
openPage url 
  | isURI url = doSessCommand POST "/url" . single "url" $ url
  | otherwise = throwIO . InvalidURL $ url

-- |Navigate forward in the browser history.
forward :: WD ()
forward = doSessCommand POST "/forward" ()

-- |Navigate backward in the browser history.
back :: WD ()
back = doSessCommand POST "/back" ()

-- |Refresh the current page
refresh :: WD ()
refresh = doSessCommand POST "/refresh" ()

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
executeJS :: FromJSON a => [JSArg] -> Text -> WD a
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
asyncJS :: FromJSON a => [JSArg] -> Text -> WD (Maybe a)
asyncJS a s = handle timeout $ fromJSON' =<< getResult
  where 
    getResult = doSessCommand POST "/execute_async" . pair ("args", "script") 
                $ (a,s)
    timeout (FailedCommand Timeout _) = return Nothing
    timeout err = throwIO err
        
-- |Grab a screenshot of the current page as a PNG image
screenshot :: WD ByteString
screenshot = B64.decodeLenient <$> doSessCommand GET "/screenshot" () 


availableIMEEngines :: WD [Text]
availableIMEEngines = doSessCommand GET "/ime/available_engines" ()

activeIMEEngine :: WD Text
activeIMEEngine = doSessCommand GET "/ime/active_engine" ()

checkIMEActive :: WD Bool
checkIMEActive = doSessCommand GET "/ime/activated" ()

activateIME :: Text -> WD ()
activateIME = doSessCommand POST "/ime/activate" . single "engine"

deactivateIME :: WD ()
deactivateIME = doSessCommand POST "/ime/deactivate" ()

-- |Switch focus to the window or frame specified by the FocusSelector.
focus :: FocusSelector -> WD ()
focus (OnWindow w) = doSessCommand POST "/window" . single "name" $ w
focus s            = doSessCommand POST "/frame" . single "id" $ s 

-- |Returns a handle to the currently focused window
getCurrentWindow :: WD WindowHandle
getCurrentWindow = doSessCommand GET "/window_handle" ()

-- |Returns a list of all windows available to the session
windows :: WD [WindowHandle]
windows = doSessCommand GET "/window_handles" ()

-- |Closes the given window
closeWindow :: WindowHandle -> WD ()
closeWindow = doSessCommand DELETE "/window" . single "name"

-- |Get the dimensions of the given window.
getWindowSize :: WindowHandle -> WD (Word, Word)
getWindowSize w =
  doWinCommand GET w "/size" () >>= parsePair "width" "height" "getWindowSize"

-- |Set the dimensions of the given window.
setWindowSize :: WindowHandle -> (Word, Word) -> WD ()
setWindowSize win = doWinCommand POST win "/size" . pair ("width", "height")

-- |Get the coordinates of the given window.
getWindowPos :: WindowHandle -> WD (Int, Int)
getWindowPos w = do 
  doWinCommand GET w "/position" () >>= parsePair "x" "y" "getWindowPos"

-- |Set the coordinates of the given window.
setWindowPos :: WindowHandle -> (Int, Int) -> WD ()
setWindowPos w = doWinCommand POST w "/position" . pair ("x","y")

-- |Retrieve all cookies visible to the current page.
cookies :: WD [Cookie]
cookies = doSessCommand GET "/cookie" ()

-- |Set a cookie. If the cookie path is not specified, it will default to \"/\".
-- Likewise, if the domain is omitted, it will default to the current page's 
-- domain
setCookie :: Cookie -> WD ()
setCookie = doSessCommand POST "/cookie" . single "cookie"

-- |Delete a cookie. This will do nothing is the cookie isn't visible to the 
-- current page.
deleteCookie :: Cookie -> WD ()
deleteCookie c = doSessCommand DELETE ("/cookie/" `append` cookName c) ()

-- |Delete all visible cookies on the current page.
deleteVisibleCookies :: WD ()
deleteVisibleCookies = doSessCommand DELETE "/cookie" ()

-- |Get the current page source
getSource :: WD Text
getSource = doSessCommand GET "/source" ()

-- |Get the title of the current page.
getTitle :: WD Text
getTitle = doSessCommand GET "/title" ()

-- |Find an element on the page using the given element selector.
findElem :: Selector -> WD Element
findElem = doSessCommand POST "/element"

-- |Find all elements on the page matching the given selector.
findElems :: Selector -> WD [Element]
findElems = doSessCommand POST "/elements"

-- |Return the element that currently has focus.
activeElem :: WD Element
activeElem = doSessCommand POST "/element/active" () 

-- |Search for an element using the given element as root.
findElemFrom :: Element -> Selector -> WD Element
findElemFrom e = doElemCommand POST e "/element"

-- |Find all elements matching a selector, using the given element as root.
findElemsFrom :: Element -> Selector -> WD [Element]
findElemsFrom e = doElemCommand POST e "/elements"

-- |Describe the element. Returns a JSON object whose meaning is currently  
-- undefined by the WebDriver protocol.
elemInfo :: Element -> WD Value
elemInfo e = doElemCommand GET e "" ()

-- |Click on an element.
click :: Element -> WD ()
click e = doElemCommand POST e "/click" ()

-- |Submit a form element. This may be applied to descendents of a form element
-- as well.
submit :: Element -> WD ()
submit e = doElemCommand POST e "/submit" ()

-- |Get all visible text within this element.
getText :: Element -> WD Text
getText e = doElemCommand GET e "/text" ()

-- |Send a sequence of keystrokes to an element. All modifier keys are released
-- at the end of the function. For more information about modifier keys, see 
-- <http://code.google.com/p/selenium/wiki/JsonWireProtocol#/session/:sessionId/element/:id/value>
sendKeys :: Text -> Element -> WD ()
sendKeys t e = doElemCommand POST e "/value" . single "value" $ [t]

-- |Similar to sendKeys, but doesn't implicitly release modifier keys
-- afterwards. This allows you to combine modifiers with mouse clicks.
sendRawKeys :: Text -> Element -> WD ()
sendRawKeys t e = doElemCommand POST e "/keys" . single "value" $ [t]

-- |Return the tag name of the given element.
tagName :: Element -> WD Text
tagName e = doElemCommand GET e "/name" ()

-- |Clear a textarea or text input element's value.
clearInput :: Element -> WD ()
clearInput e = doElemCommand POST e "/clear" ()

-- |Determine if the element is selected.
isSelected :: Element -> WD Bool
isSelected e = doElemCommand GET e "/selected" ()

-- |Determine if the element is enabled.
isEnabled :: Element -> WD Bool
isEnabled e = doElemCommand GET e "/enabled" ()

-- |Determine if the element is displayed.
isDisplayed :: Element -> WD Bool
isDisplayed e = doElemCommand GET e "/displayed" ()

-- |Retrieve the value of an element's attribute
attr :: Element -> Text -> WD (Maybe Text)
attr e t = doElemCommand GET e ("/attribute/" `append` t) ()

-- |Retrieve the value of an element's computed CSS property
cssProp :: Element -> Text -> WD (Maybe Text)
cssProp e t = doElemCommand GET e ("/css/" `append` t) ()

-- |Retrieve an element's current position.
elemPos :: Element -> WD (Int, Int)
elemPos e = doElemCommand GET e "/location" () >>= parsePair "x" "y" "elemPos"

-- |Retrieve an element's current size.
elemSize :: Element -> WD (Word, Word)
elemSize e = doElemCommand GET e "/size" () 
             >>= parsePair "width" "height" "elemSize"

infix 4 <==>
-- |Determines if two element identifiers refer to the same element.
(<==>) :: Element -> Element -> WD Bool
e1 <==> (Element e2) = doElemCommand GET e1 ("/equals/" `append` e2) ()

-- |Determines if two element identifiers refer to different elements.
infix 4 </=>
(</=>) :: Element -> Element -> WD Bool
e1 </=> e2 = not <$> (e1 <==> e2)

-- |Get the current screen orientation for rotatable display devices.
getOrientation :: WD Orientation
getOrientation = doSessCommand GET "/orientation" ()

-- |Set the current screen orientation for rotatable display devices.
setOrientation :: Orientation -> WD ()
setOrientation = doSessCommand POST "/orientation" . single "orientation"

-- |Get the text of an alert dialog.
getAlertText :: WD Text
getAlertText = doSessCommand GET "/alert_text" ()

-- |Sends keystrokes to Javascript prompt() dialog.
replyToAlert :: Text -> WD ()
replyToAlert = doSessCommand POST "/alert_text" . single "text"

-- |Accepts the currently displayed alert dialog.
acceptAlert :: WD ()
acceptAlert = doSessCommand POST "/accept_alert" ()

-- |Dismisses the currently displayed alert dialog.
dismissAlert :: WD ()
dismissAlert = doSessCommand POST "/dismiss_alert" ()

-- |Moves the mouse to the given position relative to the active element.
moveTo :: (Int, Int) -> WD ()
moveTo = doSessCommand POST "/moveto" . pair ("xoffset","yoffset")

-- |Moves the mouse to the center of a given element.
moveToCenter :: Element -> WD ()
moveToCenter (Element e) = 
  doSessCommand POST "/moveto" . single "element" $ e

-- |Moves the mouse to the given position relative to the given element.
moveToFrom :: (Int, Int) -> Element -> WD ()
moveToFrom (x,y) (Element e) = 
  doSessCommand POST "/moveto" 
  . triple ("element","xoffset","yoffset") $ (e,x,y)

-- |Click at the current mouse position with the given mouse button.
clickWith :: MouseButton -> WD ()
clickWith = doSessCommand POST "/click" . single "button"

-- |Perform the given action with the left mouse button held down. The mouse
-- is automatically released afterwards.
withMouseDown :: WD a -> WD a
withMouseDown wd = mouseDown >> wd <* mouseUp

-- |Press and hold the left mouse button down. Note that undefined behavior 
-- occurs if the next mouse command is not mouseUp.
mouseDown :: WD ()
mouseDown = doSessCommand POST "/buttondown" ()

-- |Release the left mouse button.
mouseUp :: WD ()
mouseUp = doSessCommand POST "/buttonup" ()

-- |Double click at the current mouse location.
doubleClick :: WD ()
doubleClick = doSessCommand POST "/doubleclick" ()

-- |Single tap on the touch screen at the given element's location.
touchClick :: Element -> WD ()
touchClick (Element e) = 
  doSessCommand POST "/touch/click" . single "element" $ e 

-- |Emulates pressing a finger down on the screen at the given location.
touchDown :: (Int, Int) -> WD ()
touchDown = doSessCommand POST "/touch/down" . pair ("x","y")

-- |Emulates removing a finger from the screen at the given location.
touchUp :: (Int, Int) -> WD ()
touchUp = doSessCommand POST "/touch/up" . pair ("x","y")

-- |Emulates moving a finger on the screen to the given location.
touchMove :: (Int, Int) -> WD ()
touchMove = doSessCommand POST "/touch/move" . pair ("x","y")

-- |Emulate finger-based touch scroll. Use this function if you don't care where
-- the scroll begins
touchScroll :: (Int, Int) -> WD ()
touchScroll = doSessCommand POST "/touch/scroll" . pair ("xoffset","yoffset")

-- |Emulate finger-based touch scroll, starting from the given location relative
-- to the given element.
touchScrollFrom :: (Int, Int) -> Element -> WD ()
touchScrollFrom (x, y) (Element e) = 
  doSessCommand POST "/touch/scroll"
  . triple ("xoffset", "yoffset", "element")
  $ (x, y, e)

-- |Emulate a double click on a touch device.
touchDoubleClick :: Element -> WD ()
touchDoubleClick (Element e) = doSessCommand POST "/touch/doubleclick"
                               . single "element" $ e

-- |Emulate a long click on a touch device.
touchLongClick :: Element -> WD ()
touchLongClick (Element e) = doSessCommand POST "/touch/longclick"
                             . single "element" $ e
-- |Emulate a flick on the touch screen. The coordinates indicate x and y 
-- velocity, respectively. Use this function if you don't care where the 
-- flick starts.
touchFlick :: (Int, Int) -> WD ()
touchFlick = doSessCommand POST "/touch/flick" . pair ("xSpeed", "ySpeed")

-- |Emulate a flick on the touch screen.
touchFlickFrom :: Int           -- ^ flick velocity
                  -> (Int, Int) -- ^ a location relative to the given element
                  -> Element    -- ^ the given element
                  -> WD ()
touchFlickFrom s (x,y) (Element e) = 
  doSessCommand POST "/touch/flick" . object $
  ["xoffset" .= x
  ,"yoffset" .= y
  ,"speed"   .= s
  ,"element" .= e
  ]
  
-- |Get the current geographical location of the device.
getLocation :: WD (Int, Int, Int)
getLocation = doSessCommand GET "/location" () 
              >>= parseTriple "latitude" "longitude" "altitude" "getLocation"

-- |Set the current geographical location of the device.
setLocation :: (Int, Int, Int) -> WD ()
setLocation = doSessCommand POST "/location" . triple ("latitude",
                                                       "longitude",
                                                       "altitude")

