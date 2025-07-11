
module Test.WebDriver.Commands.ScreenCapture (
  screenshot
  , screenshotElement

  -- * Convenience functions
  , saveScreenshot
  ) where

import Control.Monad.IO.Class
import Data.Aeson as A
import Data.ByteString.Base64.Lazy as B64
import Data.ByteString.Lazy as LBS (ByteString, writeFile)
import qualified Data.Text.Lazy.Encoding as TL
import GHC.Stack
import Test.WebDriver.Types
import Test.WebDriver.Util.Commands


-- | Grab a screenshot of the current page as a PNG image
screenshot :: (HasCallStack, WebDriver wd) => wd LBS.ByteString
screenshot = (B64.decodeLenient . TL.encodeUtf8) <$> doSessCommand methodGet "/screenshot" Null

-- | Grab a screenshot of the current page as a PNG image
screenshotElement :: (HasCallStack, WebDriver wd) => Element -> wd LBS.ByteString
screenshotElement e = (B64.decodeLenient . TL.encodeUtf8) <$> doElemCommand methodGet e "/screenshot" Null

-- | Save a screenshot to a particular location
saveScreenshot :: (HasCallStack, WebDriver wd) => FilePath -> wd ()
saveScreenshot path = screenshot >>= liftIO . LBS.writeFile path
