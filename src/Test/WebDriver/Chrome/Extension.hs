{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |Functions and types for working with Google Chrome extensions.
module Test.WebDriver.Chrome.Extension 
       ( ChromeExtension
       , loadExtension
       , loadRawExtension 
       ) where
import Data.ByteString as BS
import Data.ByteString.Base64 as B64
import Data.Aeson
import Control.Applicative
import Control.Monad.IO.Class

-- |An opaque type representing a Google Chrome extension. Values of this type
-- are passed to the 'Test.Webdriver.chromeExtensions' field. 
newtype ChromeExtension = ChromeExtension ByteString
                        deriving (Eq, Show, Read, ToJSON, FromJSON)

-- |Load a .crx file as a 'ChromeExtension'.
loadExtension :: MonadIO m => FilePath -> m ChromeExtension
loadExtension path = liftIO $ loadRawExtension <$> BS.readFile path

-- |Load raw .crx data as a 'ChromeExtension'.
loadRawExtension :: ByteString -> ChromeExtension
loadRawExtension = ChromeExtension . B64.encode