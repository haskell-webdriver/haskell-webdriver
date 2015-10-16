module Test.Utils where

import Test.Config
import Test.WebDriver
import Data.Text as T

openTestPage path = openPage $ serverUrl `T.append` path
