
module Test.WebDriver.Commands.Actions (
   moveTo
  , moveToCenter
  , moveToFrom
  , clickWith
  , mouseDown
  , mouseUp
  , withMouseDown
  , doubleClick

  -- * Types
  , MouseButton(..)
  ) where

import Data.Aeson as A
import Data.CallStack
import Test.WebDriver.Class
import Test.WebDriver.Commands.ElementRetrieval
import Test.WebDriver.CommandUtil
import Test.WebDriver.JSON


-- | Moves the mouse to the given position relative to the active element.
moveTo :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
moveTo = noReturn . doSessCommand methodPost "/moveto" . pair ("xoffset","yoffset")

-- | Moves the mouse to the center of a given element.
moveToCenter :: (HasCallStack, WebDriver wd) => Element -> wd ()
moveToCenter (Element e) =
  noReturn . doSessCommand methodPost "/moveto" . single "element" $ e

-- | Moves the mouse to the given position relative to the given element.
moveToFrom :: (HasCallStack, WebDriver wd) => (Int, Int) -> Element -> wd ()
moveToFrom (x,y) (Element e) =
  noReturn . doSessCommand methodPost "/moveto"
  . triple ("element","xoffset","yoffset") $ (e,x,y)

-- | Click at the current mouse position with the given mouse button.
clickWith :: (HasCallStack, WebDriver wd) => MouseButton -> wd ()
clickWith = noReturn . doSessCommand methodPost "/click" . single "button"

-- | Press and hold the left mouse button down. Note that undefined behavior
-- occurs if the next mouse command is not mouseUp.
mouseDown :: (HasCallStack, WebDriver wd) => wd ()
mouseDown = noReturn $ doSessCommand methodPost "/buttondown" noObject

-- | Release the left mouse button.
mouseUp :: (HasCallStack, WebDriver wd) => wd ()
mouseUp = noReturn $ doSessCommand methodPost "/buttonup" noObject

-- | Perform the given action with the left mouse button held down. The mouse
-- is automatically released afterwards.
withMouseDown :: (HasCallStack, WebDriver wd) => wd a -> wd a
withMouseDown wd = mouseDown >> wd <* mouseUp

-- | Double click at the current mouse location.
doubleClick :: (HasCallStack, WebDriver wd) => wd ()
doubleClick = noReturn $ doSessCommand methodPost "/doubleclick" noObject

-- | A mouse button
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
