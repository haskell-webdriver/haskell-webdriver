{-# OverloadedStrings #-}
module Test.Config where
import Data
import Data.String (fromString)
import Data.Text as T (Text, concat)

serverHost, serverUrl, staticContentPath :: Text
serverHost = "localhost" :: Text
serverPort = 4444

serverUrl = T.concat ["http://", serverHost, ":", fromString (show serverPort), "/"]

staticContentPath = "test/web/"