{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Config where
--import Data
import Data.String (fromString)
import Data.Text as T (Text, concat)

--SauceLabs server info
serverHost = "ondemand.saucelabs.com"
serverPort = 80
serverUsername = "kallisti-dev"
serverAccessKey = "21af2b08-07b5-427c-a90b-dc3c7b86ec06"
serverUrl = T.concat ["http://", serverUsername, ":", serverAccessKey, "@", serverHost, ":", fromString (show serverPort), "/"]

staticContentPath = "test/web/"
