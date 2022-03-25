module Utils where

import           Data.Text      as T
import           Test.Config
import           Test.WebDriver

openTestPage path = openPage $ serverUrl `T.append` path
