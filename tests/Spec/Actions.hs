{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}

module Spec.Actions where

import Data.Aeson as A
import Data.String.Interpolate
import qualified Data.Text.Lazy.Encoding as TLE
import System.FilePath
import Test.Sandwich
import Test.WebDriver
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Mouse
import TestLib.Types


setUp :: (
  HasStaticServerContext context, HasWDSession context
  ) => SpecFree context IO () -> SpecFree context IO ()
setUp x = before "Open test page" openSimpleTestPage $ before "scroll to clickable box" scrollToClickableBox x

tests :: SessionSpec
tests = introduceSession $ describe "Actions" $ setUp $ do
  it "moveToCenter" $ do
    Just dir <- getCurrentFolder
    saveScreenshot (dir </> "before.png")

    box <- findElem (ByCSS "#clickable-box")
    moveToCenter box
    clickWith LeftButton
    getBoundingClientRect "#clickable-box" >>= \bcr -> info [i|bcr: #{bcr}|]
    getLastMouseEvent >>= \lme -> info [i|lme: #{lme}|]

  it "clickCenter" $ do
    findElem (ByCSS "#clickable-box") >>= clickCenter
    center <- getElementCenter "#clickable-box"
    Just MouseEvent { eventType=MouseEventTypeClick, .. } <- getLastMouseEvent
    assertWithinPixels center (fromIntegral clientX, fromIntegral clientY) 15

  it "doubleClick" $ do
    doubleClick
    center <- getElementCenter "#clickable-box"
    Just MouseEvent { eventType=MouseEventTypeDoubleClick, .. } <- getLastMouseEvent
    assertWithinPixels center (fromIntegral clientX, fromIntegral clientY) 15


scrollToClickableBox :: (HasWDSession ctx) => ExampleT ctx IO ()
scrollToClickableBox = do
  -- { behavior: 'smooth', block: 'nearest', inline: 'nearest' }
  executeJS [] [i|document.querySelector("\#clickable-box").scrollIntoView()|]

getLastMouseEvent :: (HasWDSession ctx) => ExampleT ctx IO (Maybe MouseEvent)
getLastMouseEvent = do
  executeJS [] [i|return document.querySelector("\#last-mouse-event").innerText|] >>= \case
    "{}" -> return Nothing
    json -> case A.eitherDecode (TLE.encodeUtf8 json) of
      Left err -> expectationFailure [i|Failed to decode last mouse event: #{err}|]
      Right x -> pure x
