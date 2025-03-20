
module Spec.ScreenCapture where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL
import System.FilePath
import Test.Sandwich
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Screen capture" $ before "Open test page" openSimpleTestPage $ do
  it "screenshot" $ do
    Just dir <- getCurrentFolder
    screenshot >>= liftIO . BL.writeFile (dir </> "screenshot.png")

  it "screenshotElement" $ do
    Just dir <- getCurrentFolder
    e <- findElem (ByCSS ".frame-container")
    screenshotElement e >>= liftIO . BL.writeFile (dir </> "screenshotElement.png")

  it "saveScreenshot" $ do
    Just dir <- getCurrentFolder
    saveScreenshot (dir </> "saveScreenshot.png")
