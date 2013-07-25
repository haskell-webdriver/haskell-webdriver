module Test.WebDriver.Utils where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Base as HTTP

urlEncode :: Text -> Text
urlEncode = T.pack . HTTP.urlEncode . T.unpack 