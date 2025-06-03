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
import Test.WebDriver.CommandUtil
import Test.WebDriver.JSON

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

-- | Execute actions and release them
executeActions :: (HasCallStack, WebDriver wd) => A.Value -> wd ()
executeActions actions = do
  noReturn $ doSessCommand methodPost "/actions" actions
  -- Release all actions to reset state
  noReturn $ doSessCommand methodDelete "/actions" noObject

-- | Moves the mouse to the given position relative to the current pointer position.
moveTo :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
moveTo (x, y) = executeActions $ createPointerAction "mouse1"
  [pointerMoveAction (Just "pointer") Nothing x y 250]

-- | Moves the mouse to the center of a given element.
moveToCenter :: (HasCallStack, WebDriver wd) => Element -> wd ()
moveToCenter (Element e) = executeActions $ createPointerAction "mouse1"
  [pointerMoveAction Nothing (Just elementRef) 0 0 250]
  where
    elementRef = A.object ["element-6066-11e4-a52e-4f735466cecf" A..= e]

-- | Moves the mouse to the given position relative to the given element.
moveToFrom :: (HasCallStack, WebDriver wd) => (Int, Int) -> Element -> wd ()
moveToFrom (x, y) (Element e) = executeActions $ createPointerAction "mouse1"
  [pointerMoveAction Nothing (Just elementRef) x y 250]
  where
    elementRef = A.object ["element-6066-11e4-a52e-4f735466cecf" A..= e]

-- | Click at the current mouse position with the given mouse button.
clickWith :: (HasCallStack, WebDriver wd) => MouseButton -> wd ()
clickWith button = executeActions $ createPointerAction "mouse1" [
  pointerDownAction button
  , pointerUpAction button
  ]

-- | Press and hold the left mouse button down. Note that undefined behavior
-- occurs if the next mouse command is not mouseUp.
mouseDown :: (HasCallStack, WebDriver wd) => wd ()
mouseDown = executeActions $ createPointerAction "mouse1"
  [pointerDownAction LeftButton]

-- | Release the left mouse button.
mouseUp :: (HasCallStack, WebDriver wd) => wd ()
mouseUp = executeActions $ createPointerAction "mouse1" [pointerUpAction LeftButton]

-- | Perform the given action with the left mouse button held down. The mouse
-- is automatically released afterwards.
withMouseDown :: (HasCallStack, WebDriver wd) => wd a -> wd a
withMouseDown wd = mouseDown >> wd <* mouseUp

-- | Double click at the current mouse location.
doubleClick :: (HasCallStack, WebDriver wd) => wd ()
doubleClick = executeActions $ createPointerAction "mouse1" [
  pointerDownAction LeftButton
  , pointerUpAction LeftButton
  , pauseAction 100
  , pointerDownAction LeftButton
  , pointerUpAction LeftButton
  ]

-- * Internal helpers

createPointerAction :: String -> [A.Value] -> A.Value
createPointerAction actionId actions = A.object ["actions" A..= [action]]
  where
    action = A.object [
      "type" A..= ("pointer" :: String)
      , "id" A..= actionId
      , "actions" A..= actions
      ]

pointerMoveAction :: Maybe String -> Maybe A.Value -> Int -> Int -> Int -> A.Value
pointerMoveAction origin element x y duration = A.object $ [
  "type" A..= ("pointerMove" :: String)
  , "duration" A..= duration
  , "x" A..= x
  , "y" A..= y
  ] ++ originField
  where
    originField = case (origin, element) of
      (Just o, Nothing) -> ["origin" A..= o]
      (Nothing, Just e) -> ["origin" A..= e]
      _ -> []

pointerDownAction :: MouseButton -> A.Value
pointerDownAction button = A.object [
  "type" A..= ("pointerDown" :: String)
  , "button" A..= button
  ]

pointerUpAction :: MouseButton -> A.Value
pointerUpAction button = A.object [
  "type" A..= ("pointerUp" :: String)
  , "button" A..= button
  ]

pauseAction :: Int -> A.Value
pauseAction duration = A.object [
  "type" A..= ("pause" :: String)
  , "duration" A..= duration
  ]
