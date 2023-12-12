{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.WebDriver.Actions.Internal where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Test.WebDriver.Commands.Internal (Element)
import Test.WebDriver.JSON.Internal (lower1)

data Actions = Actions
  { actionsId :: Text
  , actionsType :: ActionsType
  , actionsParameters :: Maybe ActionsParameters
  , actionsActions :: [Action]
  } deriving (Eq, Show)

data ActionsType =
    ActionsPointer
  | ActionsKey
  | ActionsNone
  deriving (Eq, Show)

data ActionsParameters = ActionsParameters
  { paramsPointerType :: Maybe PointerType
  } deriving (Eq, Show)

data Action = Action
  { actionType :: ActionType
  , actionDurtion :: Maybe Int
  , actionX :: Maybe Int
  , actionY :: Maybe Int
  , actionOrigin :: Maybe MoveOrigin
  , actionValue :: Maybe Text
  , actionButton :: Maybe MouseButton
  } deriving (Eq, Show)

data PointerType =
    PointerMouse
  | PointerPen
  | PointerTouch
  deriving (Eq, Show)

data MoveOrigin =
    OriginViewport
  | OriginPointer
  | OriginElement Element
  deriving (Eq, Show)

instance ToJSON MoveOrigin where
  toJSON OriginViewport = String "viewport"
  toJSON OriginPointer = String "pointer"
  toJSON (OriginElement e) = toJSON e

data ActionType =
    ActionPause
  | ActionKeyUp
  | ActionKeyDown
  | ActionPointerUp
  | ActionPointerDown
  | ActionPointerMove
  | ActionPointerCancel
  | ActionScroll
  deriving (Eq, Show)

-- |A mouse button
data MouseButton =
    LeftButton
  | MiddleButton
  | RightButton
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

$(deriveToJSON (defaultOptions{constructorTagModifier = lower1 . drop 7}) ''PointerType)
$(deriveToJSON (defaultOptions{constructorTagModifier = lower1 . drop 6}) ''ActionType)
$(deriveToJSON (defaultOptions{constructorTagModifier = lower1 . drop 7}) ''ActionsType)
$(deriveToJSON (defaultOptions{fieldLabelModifier = lower1 . drop 6, omitNothingFields = True}) ''Action)
$(deriveToJSON (defaultOptions{fieldLabelModifier = lower1 . drop 6, omitNothingFields = True}) ''ActionsParameters)
$(deriveToJSON (defaultOptions{fieldLabelModifier = lower1 . drop 7, omitNothingFields = True}) ''Actions)

pointerMoveAction :: (Int, Int) -> MoveOrigin -> Action
pointerMoveAction (x, y) origin =
  Action
    { actionType = ActionPointerMove
    , actionDurtion = Nothing
    , actionX = Just x
    , actionY = Just y
    , actionOrigin = Just origin
    , actionValue = Nothing
    , actionButton = Nothing
    }

pointerDownAction :: Action
pointerDownAction =
  Action
    { actionType = ActionPointerDown
    , actionDurtion = Nothing
    , actionX = Nothing
    , actionY = Nothing
    , actionOrigin = Nothing
    , actionValue = Nothing
    , actionButton = Nothing
    }

pointerUpAction :: Action
pointerUpAction =
  Action
    { actionType = ActionPointerUp
    , actionDurtion = Nothing
    , actionX = Nothing
    , actionY = Nothing
    , actionOrigin = Nothing
    , actionValue = Nothing
    , actionButton = Nothing
    }

keyDownAction :: Text -> Action
keyDownAction c =
  Action
    { actionType = ActionKeyDown
    , actionDurtion = Nothing
    , actionX = Nothing
    , actionY = Nothing
    , actionOrigin = Nothing
    , actionValue = Just c
    , actionButton = Nothing
    }

keyUpAction :: Text -> Action
keyUpAction c =
  Action
    { actionType = ActionKeyUp
    , actionDurtion = Nothing
    , actionX = Nothing
    , actionY = Nothing
    , actionOrigin = Nothing
    , actionValue = Just c
    , actionButton = Nothing
    }
