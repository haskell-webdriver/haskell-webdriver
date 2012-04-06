module Test.WebDriver.Utils where
import Test.WebDriver.Types (ServerError(..))
import Data.ByteString
import Data.ByteString.Base64
import Control.Exception.Lifted

b64Decode :: ByteString -> ByteString
b64Decode = either 
              (throw . ServerError
              . ("screenshot: Error decoding base64 PNG from server" ++))
              id . decode