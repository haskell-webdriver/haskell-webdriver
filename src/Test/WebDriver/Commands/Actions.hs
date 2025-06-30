module Test.WebDriver.Commands.Actions (
  moveTo
  , moveToCenter
  , moveToFrom
  , clickCenter
  , doubleClickCenter

  -- * Lower-level actions API
  , performActions
  , releaseActions
  , Action(..)
  , ActionSource(..)
  , PointerAction(..)
  , KeyAction(..)
  , PointerOrigin(..)

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
  parseJSON v = parseJSON v >>= \case
    (0 :: Int) -> return LeftButton
    1 -> return MiddleButton
    2 -> return RightButton
    err -> fail $ "Invalid JSON for MouseButton: " ++ show err

-- -----------------------------------------------------------------------------
-- Legacy API (Wire Protocol)
-- -----------------------------------------------------------------------------

movementTimeMs :: Int
movementTimeMs = 50

-- | Moves the mouse to the given position relative to the current pointer position.
moveTo :: (HasCallStack, WebDriver wd) => (Int, Int) -> wd ()
moveTo (x, y) = performActions [PointerSource "mouse1" [ActionPointer $ PointerMove PointerCurrent x y movementTimeMs]]

-- | Moves the mouse to the center of a given element.
moveToCenter :: (HasCallStack, WebDriver wd) => Element -> wd ()
moveToCenter el = performActions [PointerSource "mouse1" [ActionPointer $ PointerMove (PointerElement el) 0 0 movementTimeMs]]

-- | Moves the mouse to the given position relative to the given element.
moveToFrom :: (HasCallStack, WebDriver wd) => (Int, Int) -> Element -> wd ()
moveToFrom (x, y) el = performActions [PointerSource "mouse1" [ActionPointer $ PointerMove (PointerElement el) x y movementTimeMs]]

-- | Helper to click the center of an element.
clickCenter :: (HasCallStack, WebDriver wd) => Element -> wd ()
clickCenter el = performActions [PointerSource "mouse1" [
  ActionPointer $ PointerMove (PointerElement el) 0 0 movementTimeMs
  , ActionPointer $ PointerDown LeftButton
  , ActionPointer $ PointerUp LeftButton
  ]]

-- | Helper to double click the center of an element.
doubleClickCenter :: (HasCallStack, WebDriver wd) => Element -> wd ()
doubleClickCenter el = performActions [PointerSource "mouse1" [
  ActionPointer $ PointerMove (PointerElement el) 0 0 movementTimeMs
  , ActionPointer $ PointerDown LeftButton
  , ActionPointer $ PointerUp LeftButton
  , ActionPause 100
  , ActionPointer $ PointerDown LeftButton
  , ActionPointer $ PointerUp LeftButton
  ]]

-- -----------------------------------------------------------------------------
-- Modern Actions API
-- -----------------------------------------------------------------------------

-- | Origin for pointer actions
data PointerOrigin
  = PointerViewport
  | PointerCurrent
  | PointerElement Element
  deriving (Eq, Show)

-- | Individual pointer actions
data PointerAction
  = PointerMove { moveOrigin :: PointerOrigin, moveX :: Int, moveY :: Int, moveDuration :: Int }
  | PointerDown { downButton :: MouseButton }
  | PointerUp { upButton :: MouseButton }
  | PointerCancel
  deriving (Eq, Show)

-- | Individual key actions
data KeyAction
  = KeyDown { keyValue :: String }
  | KeyUp { keyValue :: String }
  deriving (Eq, Show)

-- | Generic action that can be pause, pointer, or key
data Action
  = ActionPause { pauseDuration :: Int }
  | ActionPointer { pointerAction :: PointerAction }
  | ActionKey { keyAction :: KeyAction }
  deriving (Eq, Show)

-- | Action source (input device)
data ActionSource
  = PointerSource { sourceId :: String, actions :: [Action] }
  | KeySource { sourceId :: String, actions :: [Action] }
  deriving (Eq, Show)

-- | Perform a sequence of actions from multiple input sources
performActions :: (HasCallStack, WebDriver wd) => [ActionSource] -> wd ()
performActions sources = noReturn $ doSessCommand methodPost "/actions"
  (A.object ["actions" A..= map sourceToJSON sources])

-- | Release all currently pressed keys and buttons
releaseActions :: (HasCallStack, WebDriver wd) => wd ()
releaseActions = noReturn $ doSessCommand methodDelete "/actions" noObject

-- -----------------------------------------------------------------------------
-- Internal helpers
-- -----------------------------------------------------------------------------

sourceToJSON :: ActionSource -> A.Value
sourceToJSON (PointerSource sid acts) = A.object [
  "type" A..= ("pointer" :: String)
  , "id" A..= sid
  , "actions" A..= map actionToJSON acts
  ]
sourceToJSON (KeySource sid acts) = A.object [
  "type" A..= ("key" :: String)
  , "id" A..= sid
  , "actions" A..= map actionToJSON acts
  ]

actionToJSON :: Action -> A.Value
actionToJSON (ActionPause dur) = A.object [
  "type" A..= ("pause" :: String)
  , "duration" A..= dur
  ]
actionToJSON (ActionPointer pact) = pointerActionToJSON pact
actionToJSON (ActionKey kact) = keyActionToJSON kact

pointerActionToJSON :: PointerAction -> A.Value
pointerActionToJSON (PointerMove orig x y dur) = A.object ([
  "type" A..= ("pointerMove" :: String)
  , "duration" A..= dur
  , "x" A..= x
  , "y" A..= y
  ] <> originToJSON orig)
  where
    originToJSON :: PointerOrigin -> [(Key, A.Value)]
    originToJSON PointerViewport = [("origin", A.String "viewport")]
    originToJSON PointerCurrent = [("origin", A.String "pointer")]
    originToJSON (PointerElement (Element e)) = [("origin", A.object ["element-6066-11e4-a52e-4f735466cecf" A..= e])]
pointerActionToJSON (PointerDown btn) = A.object [
  "type" A..= ("pointerDown" :: String)
  , "button" A..= btn
  ]
pointerActionToJSON (PointerUp btn) = A.object [
  "type" A..= ("pointerUp" :: String)
  , "button" A..= btn
  ]
pointerActionToJSON PointerCancel = A.object [
  "type" A..= ("pointerCancel" :: String)
  ]

keyActionToJSON :: KeyAction -> A.Value
keyActionToJSON (KeyDown val) = A.object [
  "type" A..= ("keyDown" :: String)
  , "value" A..= val
  ]
keyActionToJSON (KeyUp val) = A.object [
  "type" A..= ("keyUp" :: String)
  , "value" A..= val
  ]
