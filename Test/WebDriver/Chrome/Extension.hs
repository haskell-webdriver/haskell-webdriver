{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

newtype ChromeExtension = ChromeExtension ByteString
                        deriving (Eq, Show, Read, ToJSON, FromJSON)

loadExtension :: MonadIO m => FilePath -> m ChromeExtension
loadExtension path = liftIO $ loadRawExtension <$> BS.readFile path

loadRawExtension :: ByteString -> ChromeExtension
loadRawExtension = ChromeExtension . B64.encode