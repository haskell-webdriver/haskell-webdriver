
module Test.WebDriver.Commands.Common where

import Data.Aeson


-- Selenium 3.x doesn't seem to like receiving Null for click parameter
noObject :: Value
noObject = Object mempty
