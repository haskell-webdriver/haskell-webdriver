
module Test.WebDriver.Commands.CommandContexts (
  getCurrentWindow
  , closeWindow
  , focusWindow
  , windows
  , focusFrame
  , focusParentFrame

  -- * Resizing and positioning windows
  , getWindowRect
  , setWindowRect
  , maximize
  , minimize
  , fullscreen

  -- * Types
  , FrameSelector(..)
  , Rect(..)
  , WindowHandle(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson as A
import Data.Aeson.Types
import Data.CallStack
import Data.Text (Text)
import Prelude -- hides some "unused import" warnings
import Test.WebDriver.Class
import Test.WebDriver.Commands.Internal
import Test.WebDriver.JSON

-- | Returns a handle to the currently focused window
getCurrentWindow :: (HasCallStack, WebDriver wd) => wd WindowHandle
getCurrentWindow = doSessCommand methodGet "/window" Null

-- | Closes the given window
closeWindow :: (HasCallStack, WebDriver wd) => WindowHandle -> wd ()
closeWindow w = do
  cw <- getCurrentWindow
  focusWindow w
  ignoreReturn $ doSessCommand methodDelete "/window" Null
  unless (w == cw) $ focusWindow cw

-- | Switch to a given window
focusWindow :: (HasCallStack, WebDriver wd) => WindowHandle -> wd ()
focusWindow w = noReturn $ doSessCommand methodPost "/window" . single "handle" $ w

-- | Returns a list of all windows available to the session
windows :: (HasCallStack, WebDriver wd) => wd [WindowHandle]
windows = doSessCommand methodGet "/window/handles" Null

-- | Switch focus to the frame specified by the FrameSelector.
focusFrame :: (HasCallStack, WebDriver wd) => FrameSelector -> wd ()
focusFrame s = noReturn $ doSessCommand methodPost "/frame" . single "id" $ s

-- | Switch focus to the frame specified by the FrameSelector.
focusParentFrame :: (HasCallStack, WebDriver wd) => wd ()
focusParentFrame = ignoreReturn $ doSessCommand methodPost "/frame/parent" (A.object [])

-- | Get the dimensions of the current window.
getWindowRect :: (HasCallStack, WebDriver wd) => wd Rect
getWindowRect = doSessCommand methodGet "/window/rect" Null

-- | Set the dimensions of the current window.
setWindowRect :: (HasCallStack, WebDriver wd) => Rect -> wd ()
setWindowRect = ignoreReturn . doSessCommand methodPost "/window/rect"

-- | Maximizes the current window
maximize :: (HasCallStack, WebDriver wd) => wd ()
maximize = ignoreReturn $ doSessCommand methodPost "/window/maximize" (A.object [])

-- | Minimizes the current window
minimize :: (HasCallStack, WebDriver wd) => wd ()
minimize = ignoreReturn $ doSessCommand methodPost "/window/minimize" (A.object [])

-- | Fullscreens the current window
fullscreen :: (HasCallStack, WebDriver wd) => wd ()
fullscreen = ignoreReturn $ doSessCommand methodPost "/window/fullscreen" (A.object [])


-- | Specifies the frame used by 'Test.WebDriver.Commands.focusFrame'
data FrameSelector =
  WithIndex Integer
  -- | Focus on a frame by name or ID
  | WithName Text
  -- | Focus on a frame 'Element'
  | WithElement Element
  -- | Focus on the first frame, or the main document if iframes are used.
  | DefaultFrame
  deriving (Eq, Show, Read)

instance ToJSON FrameSelector where
  toJSON s = case s of
    WithIndex i -> toJSON i
    WithName n -> toJSON n
    WithElement e -> toJSON e
    DefaultFrame -> Null

data Rect = Rect {
  rectX :: Float
  , rectY :: Float
  , rectWidth :: Float
  , rectHeight :: Float
  } deriving (Eq, Ord, Show)

instance FromJSON Rect where
  parseJSON (Object o) = Rect <$> o .: "x"
                              <*> o .: "y"
                              <*> o .: "width"
                              <*> o .: "height"
  parseJSON j = typeMismatch "Rect" j

instance ToJSON Rect where
  toJSON (Rect x y width height)
    = object [ "x" .= x
             , "y" .= y
             , "width" .= width
             , "height" .= height
             ]
