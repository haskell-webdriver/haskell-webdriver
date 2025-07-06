
module Test.WebDriver.Commands.SeleniumSpecific.HTML5 (
  -- * HTML 5 Web Storage
  storageSize
  , getAllKeys
  , deleteAllKeys
  , getKey
  , setKey
  , deleteKey
  , WebStorageType(..)

  -- * HTML 5 Application Cache
  , ApplicationCacheStatus(..)
  , getApplicationCacheStatus
  ) where

import Data.Aeson as A
import Data.CallStack
import Data.Text (Text)
import qualified Data.Text as T
import Test.WebDriver.Types
import Test.WebDriver.Util.Commands
import Test.WebDriver.JSON


-- | Get the current number of keys in a web storage area.
storageSize :: (HasCallStack, WebDriver wd) => WebStorageType -> wd Integer
storageSize s = doStorageCommand methodGet s "/size" Null

-- | Get a list of all keys from a web storage area.
getAllKeys :: (HasCallStack, WebDriver wd) => WebStorageType -> wd [Text]
getAllKeys s = doStorageCommand methodGet s "" Null

-- | Delete all keys within a given web storage area.
deleteAllKeys :: (HasCallStack, WebDriver wd) => WebStorageType -> wd ()
deleteAllKeys s = noReturn $ doStorageCommand methodDelete s "" Null

-- | An HTML 5 storage type
data WebStorageType = LocalStorage | SessionStorage
  deriving (Eq, Show, Ord, Bounded, Enum)

-- | Get the value associated with a key in the given web storage area.
-- Unset keys result in empty strings, since the Web Storage spec
-- makes no distinction between the empty string and an undefined value.
getKey :: (HasCallStack, WebDriver wd) => WebStorageType -> Text ->  wd Text
getKey s k = doStorageCommand methodGet s ("/key/" `T.append` urlEncode k) Null

-- | Set a key in the given web storage area.
setKey :: (HasCallStack, WebDriver wd) => WebStorageType -> Text -> Text -> wd Text
setKey s k v = doStorageCommand methodPost s "" . object $ ["key"   .= k,
                                                      "value" .= v ]
-- | Delete a key in the given web storage area.
deleteKey :: (HasCallStack, WebDriver wd) => WebStorageType -> Text -> wd ()
deleteKey s k = noReturn $ doStorageCommand methodPost s ("/key/" `T.append` urlEncode k) Null

-- | A wrapper around 'doSessCommand' to create web storage requests.
doStorageCommand :: (
  HasCallStack, WebDriver wd, ToJSON a, FromJSON b, ToJSON b
  ) => Method -> WebStorageType -> Text -> a -> wd b
doStorageCommand m s path a = doSessCommand m (T.concat ["/", s', path]) a
  where s' = case s of
          LocalStorage -> "local_storage"
          SessionStorage -> "session_storage"

data ApplicationCacheStatus =
  Uncached
  | Idle
  | Checking
  | Downloading
  | UpdateReady
  | Obsolete
  deriving (Eq, Enum, Bounded, Ord, Show, Read)

instance FromJSON ApplicationCacheStatus where
  parseJSON val = do
    n <- parseJSON val
    case n :: Integer of
      0 -> return Uncached
      1 -> return Idle
      2 -> return Checking
      3 -> return Downloading
      4 -> return UpdateReady
      5 -> return Obsolete
      err -> fail $ "Invalid JSON for ApplicationCacheStatus: " ++ show err

instance ToJSON ApplicationCacheStatus where
  toJSON Uncached = A.Number 0
  toJSON Idle = A.Number 1
  toJSON Checking = A.Number 2
  toJSON Downloading = A.Number 3
  toJSON UpdateReady = A.Number 4
  toJSON Obsolete = A.Number 5

getApplicationCacheStatus :: (HasCallStack, WebDriver wd) => wd ApplicationCacheStatus
getApplicationCacheStatus = doSessCommand methodGet "/application_cache/status" Null
