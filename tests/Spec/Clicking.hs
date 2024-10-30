
module Spec.Clicking where

import Data.String.Interpolate
import Test.Sandwich
import Test.Sandwich.Contexts.Waits
import Test.WebDriver
import TestLib.Contexts.Session
import TestLib.Types

-- import Control.Concurrent
-- import Control.Monad.IO.Class


tests :: SessionSpec
tests = introduceSession $ describe "Clicking" $ do
  it "works" $ do
    -- liftIO $ threadDelay 120000000

    openPage "http://www.wikipedia.org/"
    -- el <- findElem (ByCSS "div[lang=es] a")
    -- click el

    -- waitUntil 60 $ do
    --   url <- getCurrentURL
    --   info [i|Got URL: #{url}|]
    --   url `shouldContain` "es.wikipedia.org"
