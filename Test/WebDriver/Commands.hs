{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.Commands where

import Test.WebDriver.Types
import Test.WebDriver.Commands.Internal
import Test.WebDriver.JSON

import Data.Aeson
import Network.HTTP (RequestMethod(..))
import qualified Data.Text as T
import Data.Text (Text, splitOn, append)
import Data.ByteString.Lazy.Char8 (ByteString)
import Network.URI
import Control.Concurrent
import Data.Time.Clock

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Data.Word

runWD :: WDSession -> WD a -> IO (Either WDError a)
runWD = (runErrorT .) . tryWD

tryWD :: WDSession -> WD a -> ErrorT WDError IO a
tryWD sess (WD wd) = evalStateT wd sess

withSession :: WDSession -> WD a -> WD a
withSession s' (WD wd) = WD $ do
  s <- get
  withStateT (const s') wd 
    `catchError` (\err -> do put s
                             throwError err
                 ) <* put s

runSession :: WDSession -> Capabilities -> WD a -> IO (Either WDError a)
runSession = ((runErrorT .) .) . trySession

trySession :: WDSession -> Capabilities ->  WD a -> ErrorT WDError IO a
trySession s caps wd = tryWD s $ createSession caps >> wd <* closeSession
                            `catchError` handler
  where handler = const closeSession


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
  `mplus` doSessCommand POST "/timeouts"               (object allFields)
  where msField   = ["ms" .= ms] 
        allFields = ["type" .= ("implicit" :: String)] ++ msField

setScriptTimeout :: Integer -> WD () 
setScriptTimeout ms =
          doSessCommand POST "/timeouts/async_script" (object msField)
  `mplus` doSessCommand POST "/timeouts"               (object allFields)
  where msField   = ["ms" .= ms]
        allFields = ["type" .= ("script" :: String)] ++ msField
  

getCurrentWindow :: WD WindowHandle
getCurrentWindow = doSessCommand GET "/window_handle" ()

windows :: WD [WindowHandle]
windows = doSessCommand GET "/window_handles" ()

currentURL :: WD String
currentURL = doSessCommand GET "/url" ()

openPage :: String -> WD ()
openPage url 
  | isURI url = doSessCommand POST "/url" . single "url" $ url
  | otherwise = throwError . InvalidURL $ url

forward :: WD ()
forward = doSessCommand POST "/forward" ()

back :: WD ()
back = doSessCommand POST "/back" ()

refresh :: WD ()
refresh = doSessCommand POST "/refresh" ()

executeJS :: [Value] -> Text -> WD Value
executeJS = ((doSessCommand POST "/execute"
            . pair ("args", "script")) .) . (,)

asyncJS :: [Value] -> Text -> WD (Maybe Value)
asyncJS = (((Just <$>) . doSessCommand POST "/execute_async"
          . pair ("args", "script")) .) . (,) 
        
screenshot :: WD ByteString
screenshot = doSessCommand GET "/screenshot" ()

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

--focusFrame :: Frame -> WD ()
focusFrame = undefined

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

findElem :: Selector -> Text -> WD Element
findElem s t = doSessCommand POST "/element" (selector s t)
               
findElems :: Selector -> Text -> WD [Element]
findElems s t = doSessCommand POST "/elements" (selector s t)

activeElem :: WD Element
activeElem = doSessCommand POST "/element/active" () 

findElemFrom :: Element -> Selector -> Text -> WD Element
findElemFrom e l t = doElemCommand POST e "/element" (selector l t)

findElemsFrom :: Element -> Selector -> Text -> WD [Element]
findElemsFrom e l t = doElemCommand POST e "/elements" (selector l t)

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

--TODO:  /session/:sessionId/keys

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

--todo: /session/:sessionid/element/:id/location_in_view ?????

elemSize :: Element -> WD (Word, Word)
elemSize e = doElemCommand GET e "/size" () 
             >>= parsePair "width" "height" "elemSize"

infix 4 <==>
(<==>) :: Element -> Element -> WD Bool
e1 <==> (Element e2) = doElemCommand GET e1 ("/equals/" `append` e2) ()

infix 4 </=>
(</=>) :: Element -> Element -> WD Bool
e1 </=> e2 = not <$> (e1 <==> e2)

(<&&>) :: Monad m  => m Bool -> m Bool -> m Bool
(<&&>) = liftM2 (&&)

(<||>) :: Monad m => m Bool -> m Bool -> m Bool
(<||>) = liftM2 (||)

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

moveToFrom :: Element -> (Int, Int) -> WD ()
moveToFrom e (x,y) = doSessCommand POST "/moveto" 
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
touchScroll = doSessCommand POST "/touch/scroll" . pair ("xOffset","yOffset")

touchScrollFrom :: (Int, Int) -> Element -> WD ()
touchScrollFrom (x, y) e = doSessCommand POST "/touch/scroll"
                           . triple ("xOffset", "yOffset", "element")
                           $ (x, y, e)

touchDoubleClick :: Element -> WD ()
touchDoubleClick = doSessCommand POST "/touch/doubleclick"

touchLongClick :: Element -> WD ()
touchLongClick = doSessCommand POST "/touch/longclick"

touchFlick :: (Int, Int) -> WD ()
touchFlick = doSessCommand POST "/touch/flick" . pair ("xSpeed", "ySpeed")

touchFlickFrom :: (Int, Int) -> Int -> Element -> WD ()
touchFlickFrom (x,y) s e = doSessCommand POST "/touch/flick" . object $
                           ["xOffset" .= x
                           ,"yOffset" .= y
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

waitUntil :: Double -> WD Bool -> WD ()
waitUntil = waitUntil' 500000

waitUntil' :: Int -> Double -> WD Bool -> WD () 
waitUntil' wait t cond = do
  let timeout = realToFrac t
  startTime <- getTime
  fix $ \loop -> do
    p <- cond `catchError` handler
    unless p $ do
      now <- getTime
      unless (diffUTCTime now startTime >= timeout) $ do
        sleep wait
        loop
  where
    handler (FailedCommand NoSuchElement _) = return False
    handler otherErr = throwError otherErr
    getTime = liftIO getCurrentTime
    sleep   = liftIO . threadDelay

