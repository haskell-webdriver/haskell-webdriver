{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Test.WebDriver.Commands 
       ( Element(..)
       , WindowHandle(..), currentWindow
       , Cookie(..)
       , Orientation(..)
       , MouseButton(..)
       , Selector(..)
       , JSArg(..)
       , module Test.WebDriver.Commands
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
import Control.Exception.Lifted (throwIO, catch)
import Data.Word

import Prelude hiding (catch)


serverStatus :: WD Value   -- todo: make this a record type
serverStatus = doCommand GET "/status" ()

createSession :: Capabilities -> WD WDSession
createSession caps = do
  sessUrl <- doCommand POST "/session" . single "desiredCapabilities" $ caps
  let sessId = SessionId . last . filter (not . T.null) . splitOn "/" $  sessUrl
  modify $ \sess -> sess {wdSessId = Just sessId}
  return =<< get

sessions :: WD [(SessionId, Capabilities)]
sessions = do
  objs <- doCommand GET "/sessions" ()
  forM objs $ parsePair "id" "capabilities" "sessions"
        
getCaps :: WD Capabilities
getCaps = doSessCommand GET "" ()

closeSession :: WD ()
closeSession = doSessCommand DELETE "" ()

setImplicitWait :: Integer -> WD ()
setImplicitWait ms = 
  doSessCommand POST "/timeouts/implicit_wait" (object msField)
    `catch` \(_ :: SomeException) ->  
      doSessCommand POST "/timeouts" (object allFields)
  where msField   = ["ms" .= ms] 
        allFields = ["type" .= ("implicit" :: String)] ++ msField

setScriptTimeout :: Integer -> WD () 
setScriptTimeout ms =
  doSessCommand POST "/timeouts/async_script" (object msField)
    `catch` \(_ :: SomeException) ->  
      doSessCommand POST "/timeouts" (object allFields)
  where msField   = ["ms" .= ms]
        allFields = ["type" .= ("script" :: String)] ++ msField
  

getCurrentWindow :: WD WindowHandle
getCurrentWindow = doSessCommand GET "/window_handle" ()

windows :: WD [WindowHandle]
windows = doSessCommand GET "/window_handles" ()

getCurrentURL :: WD String
getCurrentURL = doSessCommand GET "/url" ()

openPage :: String -> WD ()
openPage url 
  | isURI url = doSessCommand POST "/url" . single "url" $ url
  | otherwise = throwIO . InvalidURL $ url

forward :: WD ()
forward = doSessCommand POST "/forward" ()

back :: WD ()
back = doSessCommand POST "/back" ()

refresh :: WD ()
refresh = doSessCommand POST "/refresh" ()

executeJS :: [JSArg] -> Text -> WD Value
executeJS = ((doSessCommand POST "/execute"
            . pair ("args", "script")) .) . (,)

asyncJS :: [JSArg] -> Text -> WD (Maybe Value)
asyncJS = (((Just <$>) . doSessCommand POST "/execute_async"
          . pair ("args", "script")) .) . (,) 
        
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

--TODO!!!
--focusFrame :: Frame -> WD ()
--focusFrame = undefined

focusWindow :: WindowHandle -> WD ()
focusWindow = doSessCommand POST "/window" . single "name"

closeWindow :: WindowHandle -> WD ()
closeWindow = doSessCommand DELETE "/window" . single "name"

getWindowSize :: WindowHandle -> WD (Word, Word)
getWindowSize w =
  doWinCommand GET w "/size" () >>= parsePair "width" "height" "getWindowSize"

setWindowSize :: WindowHandle -> (Word, Word) -> WD ()
setWindowSize win = doWinCommand POST win "/size" . pair ("width", "height")

getWindowPos :: WindowHandle -> WD (Int, Int)
getWindowPos w = do 
  doWinCommand GET w "/position" () >>= parsePair "x" "y" "getWindowPos"

setWindowPos :: WindowHandle -> (Int, Int) -> WD ()
setWindowPos w = doWinCommand POST w "/position" . pair ("x","y")

cookies :: WD [Cookie]
cookies = doSessCommand GET "/cookie" ()

setCookie :: Cookie -> WD ()
setCookie = doSessCommand POST "/cookie" . single "cookie"

deleteCookie :: Cookie -> WD ()
deleteCookie c = doSessCommand DELETE ("/cookie/" `append` cookName c) ()

deleteVisibleCookies :: WD ()
deleteVisibleCookies = doSessCommand DELETE "/cookie" ()

getSource :: WD Text
getSource = doSessCommand GET "/source" ()

getTitle :: WD Text
getTitle = doSessCommand GET "/title" ()

findElem :: Selector -> WD Element
findElem = doSessCommand POST "/element"
               
findElems :: Selector -> WD [Element]
findElems = doSessCommand POST "/elements"

activeElem :: WD Element
activeElem = doSessCommand POST "/element/active" () 

findElemFrom :: Element -> Selector -> WD Element
findElemFrom e = doElemCommand POST e "/element"

findElemsFrom :: Element -> Selector -> WD [Element]
findElemsFrom e = doElemCommand POST e "/elements"

elemInfo :: Element -> WD Value
elemInfo e = doElemCommand GET e "" ()

click :: Element -> WD ()
click e = doElemCommand POST e "/click" ()

submit :: Element -> WD ()
submit e = doElemCommand POST e "/submit" ()

getText :: Element -> WD Text
getText e = doElemCommand GET e "/text" ()

sendKeys :: Text -> Element -> WD ()
sendKeys t e = doElemCommand POST e "/value" . single "value" $ [t]

sendRawKeys :: Text -> Element -> WD ()
sendRawKeys t e = doElemCommand POST e "/keys" . single "value" $ [t]

tagName :: Element -> WD Text
tagName e = doElemCommand GET e "/name" ()

clearInput :: Element -> WD ()
clearInput e = doElemCommand POST e "/clear" ()

isSelected :: Element -> WD Bool
isSelected e = doElemCommand GET e "/selected" ()

isEnabled :: Element -> WD Bool
isEnabled e = doElemCommand GET e "/enabled" ()

isDisplayed :: Element -> WD Bool
isDisplayed e = doElemCommand GET e "/displayed" ()

attr :: Element -> Text -> WD (Maybe Text)
attr e t = doElemCommand GET e ("/attribute/" `append` t) ()

cssAttr :: Element -> Text -> WD (Maybe Text)
cssAttr e t = doElemCommand GET e ("/css/" `append` t) ()

elemPos :: Element -> WD (Int, Int)
elemPos e = doElemCommand GET e "/location" () >>= parsePair "x" "y" "elemPos"

elemSize :: Element -> WD (Word, Word)
elemSize e = doElemCommand GET e "/size" () 
             >>= parsePair "width" "height" "elemSize"

infix 4 <==>
(<==>) :: Element -> Element -> WD Bool
e1 <==> (Element e2) = doElemCommand GET e1 ("/equals/" `append` e2) ()

infix 4 </=>
(</=>) :: Element -> Element -> WD Bool
e1 </=> e2 = not <$> (e1 <==> e2)

getOrientation :: WD Orientation
getOrientation = doSessCommand GET "/orientation" ()

setOrientation :: Orientation -> WD ()
setOrientation = doSessCommand POST "/orientation" . single "orientation"

getAlertText :: WD Text
getAlertText = doSessCommand GET "/alert_text" ()

replyToAlert :: Text -> WD ()
replyToAlert = doSessCommand POST "/alert_text" . single "text"

acceptAlert :: WD ()
acceptAlert = doSessCommand POST "/accept_alert" ()

dismissAlert :: WD ()
dismissAlert = doSessCommand POST "/dismiss_alert" ()


moveTo :: (Int, Int) -> WD ()
moveTo = doSessCommand POST "/moveto" . pair ("xoffset","yoffset")

moveToCenter :: Element -> WD ()
moveToCenter (Element e) = 
  doSessCommand POST "/moveto" . single "element" $ e

moveToFrom :: (Int, Int) -> Element -> WD ()
moveToFrom (x,y) (Element e) = 
  doSessCommand POST "/moveto" 
  . triple ("element","xoffset","yoffset") $ (e,x,y)

clickWith :: MouseButton -> WD ()
clickWith = doSessCommand POST "/click" . single "button"

mouseDown :: WD ()
mouseDown = doSessCommand POST "/buttondown" ()

mouseUp :: WD ()
mouseUp = doSessCommand POST "/buttonup" ()

doubleClick :: WD ()
doubleClick = doSessCommand POST "/doubleclick" ()

touchClick :: (Int, Int) -> WD ()
touchClick = doSessCommand POST "/touch/click" . pair ("x","y")

touchDown :: (Int, Int) -> WD ()
touchDown = doSessCommand POST "/touch/down" . pair ("x","y")

touchUp :: (Int, Int) -> WD ()
touchUp = doSessCommand POST "/touch/up" . pair ("x","y")

touchMove :: (Int, Int) -> WD ()
touchMove = doSessCommand POST "/touch/move" . pair ("x","y")

touchScroll :: (Int, Int) -> WD ()
touchScroll = doSessCommand POST "/touch/scroll" . pair ("xoffset","yoffset")

touchScrollFrom :: (Int, Int) -> Element -> WD ()
touchScrollFrom (x, y) (Element e) = 
  doSessCommand POST "/touch/scroll"
  . triple ("xoffset", "yoffset", "element")
  $ (x, y, e)

touchDoubleClick :: Element -> WD ()
touchDoubleClick (Element e) = doSessCommand POST "/touch/doubleclick"
                               . single "element" $ e

touchLongClick :: Element -> WD ()
touchLongClick (Element e) = doSessCommand POST "/touch/longclick"
                             . single "element" $ e

touchFlick :: (Int, Int) -> WD ()
touchFlick = doSessCommand POST "/touch/flick" . pair ("xSpeed", "ySpeed")

touchFlickFrom :: (Int, Int) -> Int -> Element -> WD ()
touchFlickFrom (x,y) s (Element e) = 
  doSessCommand POST "/touch/flick" . object $
  ["xoffset" .= x
  ,"yoffset" .= y
  ,"speed"   .= s
  ,"element" .= e
  ]
                           
getLocation :: WD (Int, Int, Int)
getLocation = doSessCommand GET "/location" () 
              >>= parseTriple "latitude" "longitude" "altitude" "getLocation"

setLocation :: (Int, Int, Int) -> WD ()
setLocation = doSessCommand POST "/location" . triple ("latitude",
                                                       "longitude",
                                                       "altitude")

