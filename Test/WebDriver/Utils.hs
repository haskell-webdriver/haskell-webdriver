module Test.WebDriver.Utils where
import Test.WebDriver.Types (ServerError(..))
import Data.ByteString
import Data.ByteString.Base64
import Control.Exception.Lifted

b64Decode :: ByteString -> ByteString
b64Decode = either 
              (throw . ServerError
              . ("Error decoding base64-encoded PNG screenshot from server: " 
                 ++))
              id . decode