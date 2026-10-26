module Test.WebDriver.Capabilities.Browser where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text, toLower)

-- | Represents the browserName option of the primary capabilities
data Browser =
  Chrome
  | Firefox
  | Safari
  -- | some other browser, specified by a string name
  | Browser Text
  deriving (Eq, Show, Ord)

instance ToJSON Browser where
  toJSON Chrome = String "chrome"
  toJSON Firefox = String "firefox"
  toJSON Safari = String "safari"
  toJSON (Browser name) = String name

instance FromJSON Browser where
  -- Matches names ignoring case, since safaridriver reports "Safari".
  parseJSON (String name) = return $ case toLower name of
    "chrome" -> Chrome
    "firefox" -> Firefox
    "safari" -> Safari
    _ -> Browser name
  parseJSON v = typeMismatch "Browser" v
