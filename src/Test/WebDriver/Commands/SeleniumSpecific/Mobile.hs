
module Test.WebDriver.Commands.SeleniumSpecific.Mobile (
  -- ** Screen orientation
  getOrientation
  , setOrientation
  , Orientation(..)
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
  ) where

import Data.Aeson as A
import Data.Aeson.Types
import Data.CallStack
import Data.String (fromString)
import Data.Text (toUpper, toLower)
import Test.WebDriver.Monad
import Test.WebDriver.CommandUtil
import Test.WebDriver.JSON


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
