
module Test.WebDriver.Capabilities.Platform where

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch, Pair)
import Data.String (fromString)
import Data.Text (Text, toLower, toUpper)

-- | Represents the platformName option of the primary capabilities
data Platform =
  Windows
  | XP
  | Vista
  | Mac
  | Linux
  | Unix
  | Any
  | Other Text
  deriving (Eq, Show, Ord)

instance ToJSON Platform where
  toJSON (Other t) = String t
  toJSON x = String $ toUpper $ fromString $ show x

instance FromJSON Platform where
  parseJSON (String jStr) = case toLower jStr of
    "windows" -> return Windows
    "xp" -> return XP
    "vista" -> return Vista
    "mac" -> return Mac
    "linux" -> return Linux
    "unix" -> return Unix
    "any" -> return Any
    t -> return $ Other t
  parseJSON v = typeMismatch "Platform" v
