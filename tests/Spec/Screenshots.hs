
module Spec.Screenshots where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate
import System.FilePath
import Test.Sandwich
import Test.WebDriver
import TestLib.Contexts.Session
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Screenshots" $ do
  it "takes a screenshot" $ do
    openPage [i|https://www.google.com|]
    search <- findElem (ByCSS [i|*[title="Search"]|])
    click search

    sendKeys "Haskell Sandwich" search
    findElem (ByCSS [i|input[type="submit"]|]) >>= click

    Just dir <- getCurrentFolder
    screenshot >>= liftIO . BL.writeFile (dir </> "screenshot.png")
