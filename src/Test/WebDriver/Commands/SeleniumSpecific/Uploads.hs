
module Test.WebDriver.Commands.SeleniumSpecific.Uploads (
  uploadFile
  , uploadRawFile
  , uploadZipEntry
  ) where

import Codec.Archive.Zip
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Base64.Lazy as B64
import Data.ByteString.Lazy as LBS (ByteString)
import Data.CallStack
import Data.Text (Text)
import qualified Data.Text.Lazy.Encoding as TL
import Prelude -- hides some "unused import" warnings
import Test.WebDriver.Class
import Test.WebDriver.Commands.Internal
import Test.WebDriver.JSON


-- | Uploads a file from the local filesystem by its file path.
-- Returns the remote filepath of the file
uploadFile :: (HasCallStack, WebDriver wd) => FilePath -> wd Text
uploadFile path = uploadZipEntry =<< liftIO (readEntry [] path)

-- | Uploads a raw bytestring with associated file info.
-- Returns the remote filepath of the file
uploadRawFile :: (HasCallStack, WebDriver wd) =>
                 FilePath          -- ^File path to use with this bytestring.
                 -> Integer        -- ^Modification time
                                   -- (in seconds since Unix epoch).
                 -> LBS.ByteString -- ^ The file contents as a lazy ByteString
                 -> wd Text
uploadRawFile path t str = uploadZipEntry (toEntry path t str)


-- | Lowest level interface to the file uploading mechanism.
-- This allows you to specify the exact details of
-- the zip entry sent across network.
-- Returns the remote filepath of the extracted file
uploadZipEntry :: (HasCallStack, WebDriver wd) => Entry -> wd Text
uploadZipEntry = doSessCommand methodPost "/se/file" . single "file"
                 . TL.decodeUtf8 . B64.encode . fromArchive . (`addEntryToArchive` emptyArchive)
