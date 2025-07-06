{-# LANGUAGE CPP #-}

module Test.WebDriver.Util.Aeson (
  aesonToList
  , aesonLookup
  ) where

import Data.Text (Text)


#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as KM

aesonToList :: KM.KeyMap v -> [(A.Key, v)]
aesonToList = KM.toList

aesonLookup :: Text -> KM.KeyMap v -> Maybe v
aesonLookup = KM.lookup . A.fromText
#else
import qualified Data.HashMap.Strict        as HM

aesonToList :: HM.KeyMap v -> [(A.Key, v)]
aesonToList = HM.toList

aesonLookup :: (Eq k, Hashable k) => k -> HM.HashMap k v -> Maybe v
aesonLookup = HM.lookup
#endif
