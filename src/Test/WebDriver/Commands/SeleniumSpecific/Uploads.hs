
module Test.WebDriver.Commands.SeleniumSpecific.Uploads (
  seleniumUploadFile
  , seleniumUploadRawFile
  , seleniumUploadZipEntry
  ) where

import Codec.Archive.Zip
import Control.Monad.IO.Class
import Data.ByteString.Base64.Lazy as B64
import Data.ByteString.Lazy as LBS (ByteString)
import Data.CallStack
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text.Lazy.Encoding as TL
import Test.WebDriver.Class
import Test.WebDriver.CommandUtil
import Test.WebDriver.JSON


-- | Uploads a file from the local filesystem by its file path. Returns the
-- remote filepath of the file.
seleniumUploadFile :: (HasCallStack, WebDriver wd) => FilePath -> wd Text
seleniumUploadFile path = seleniumUploadZipEntry =<< liftIO (readEntry [] path)

-- | Uploads a raw 'LBS.ByteString' with associated file info. Returns the
-- remote filepath of the file.
seleniumUploadRawFile :: (
  HasCallStack, WebDriver wd
  )
  -- | File path to use with this bytestring.
  => FilePath
  -- | Modification time (in seconds since Unix epoch).
  -> Integer
  -- | The file contents as a lazy ByteString.
  -> LBS.ByteString
  -> wd Text
seleniumUploadRawFile path t str = seleniumUploadZipEntry (toEntry path t str)

-- | Lowest level interface to the file uploading mechanism. This allows you to
-- specify the exact details of the zip entry sent across network. Returns the
-- remote filepath of the extracted file
seleniumUploadZipEntry :: (HasCallStack, WebDriver wd) => Entry -> wd Text
seleniumUploadZipEntry entry = doSessCommand methodPost "/se/file" $ single "file" file
  where
    file = entry
         & (`addEntryToArchive` emptyArchive)
         & fromArchive
         & B64.encode
         & TL.decodeUtf8
