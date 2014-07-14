module Test.WebDriver.Utils where

import Data.Text (Text)
import Data.Text.Encoding as TE
import qualified Network.HTTP.Types.URI as HTTP

urlEncode :: Text -> Text
urlEncode = TE.decodeUtf8 . HTTP.urlEncode False . TE.encodeUtf8