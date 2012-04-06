module Test.WebDriver.Utils where
import Test.WebDriver.Types (ServerError(..))
import Data.ByteString.Char8
import Data.ByteString.Base64
import Control.Exception.Lifted
import Debug.Trace

b64Decode :: ByteString -> ByteString
b64Decode = decodeLenient