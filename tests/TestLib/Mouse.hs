{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

module TestLib.Mouse (
  assertWithinPixels
  , getBoundingClientRect
  , getElementCenter

  , MouseEventType(..)
  , MouseEvent(..)
  , ClientRect(..)
  ) where

import Control.Monad.IO.Class
import Data.Aeson as A
import Data.String.Interpolate
import qualified Data.Text as T
import GHC.Generics
import Test.Sandwich
import Test.WebDriver
import TestLib.Types


data MouseEventType =
  MouseEventTypeClick
  | MouseEventTypeDoubleClick
  | MouseEventTypeContextMenu
  deriving (Show, Eq)
instance FromJSON MouseEventType where
  parseJSON (String "click") = pure MouseEventTypeClick
  parseJSON (String "dblclick") = pure MouseEventTypeDoubleClick
  parseJSON (String "contextmenu") = pure MouseEventTypeContextMenu
  parseJSON x = fail [i|Unexpected mouse event type: #{x}|]

data MouseEvent = MouseEvent {
  eventType :: MouseEventType
  , screenX :: Int
  , screenY :: Int
  , clientX :: Int
  , clientY :: Int
  , pageX :: Int
  , pageY :: Int
  , button :: Int
  } deriving (Show, Eq, Generic, FromJSON)

data ClientRect = ClientRect {
  bottom :: Double
  , height :: Double
  , left :: Double
  , right :: Double
  , top :: Double
  , width :: Double
  , x :: Double
  , y :: Double
  } deriving (Show, Eq, Generic, FromJSON)

assertWithinPixels :: (MonadIO m) => (Double, Double) -> (Double, Double) -> Double -> m ()
assertWithinPixels (x1, y1) (x2, y2) tolerance =
  if | d <= tolerance -> return ()
     | otherwise -> expectationFailure [i|Points were supposed to be within #{tolerance}, but distance was #{d}|]
  where
    d = sqrt ((x2 - x1)**2 + (y2 - y1)**2)

getBoundingClientRect :: (HasWDSession ctx) => T.Text -> ExampleT ctx IO ClientRect
getBoundingClientRect cssSelector =
  executeJS [JSArg cssSelector] [i|return document.querySelector(arguments[0]).getBoundingClientRect()|]

getElementCenter :: (HasWDSession ctx) => T.Text -> ExampleT ctx IO (Double, Double)
getElementCenter cssSelector = do
  ClientRect {..} <- getBoundingClientRect cssSelector
  return (left + (width / 2.0), top + (height / 2.0))
