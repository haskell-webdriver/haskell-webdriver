
module Test.WebDriver.Commands.ElementRetrieval (
  findElem
  , findElems
  , findElemFrom
  , findElemsFrom
  , activeElem

  , Selector(..)
  , Element(..)
  ) where

import Data.Aeson as A
import Data.CallStack
import Data.Text (Text)
import Test.WebDriver.Util.Commands
import Test.WebDriver.Types


-- | Find an element on the page using the given element selector.
findElem :: (HasCallStack, WebDriver wd) => Selector -> wd Element
findElem = doSessCommand methodPost "/element"

-- | Find all elements on the page matching the given selector.
findElems :: (HasCallStack, WebDriver wd) => Selector -> wd [Element]
findElems = doSessCommand methodPost "/elements"

-- | Search for an element using the given element as root.
findElemFrom :: (HasCallStack, WebDriver wd) => Element -> Selector -> wd Element
findElemFrom e = doElemCommand methodPost e "/element"

-- | Find all elements matching a selector, using the given element as root.
findElemsFrom :: (HasCallStack, WebDriver wd) => Element -> Selector -> wd [Element]
findElemsFrom e = doElemCommand methodPost e "/elements"

-- | Return the element that currently has focus.
activeElem :: (HasCallStack, WebDriver wd) => wd Element
activeElem = doSessCommand methodGet "/element/active" Null

-- | Specifies element(s) within a DOM tree using various selection methods.
data Selector =
  ByCSS Text
  | ByLinkText Text
  | ByPartialLinkText Text
  | ByTag Text
  | ByXPath Text
  deriving (Eq, Show, Ord)

instance ToJSON Selector where
  toJSON s = case s of
    ByTag t             -> selector "tag name" t
    ByLinkText t        -> selector "link text" t
    ByPartialLinkText t -> selector "partial link text" t
    ByCSS t             -> selector "css selector" t
    ByXPath t           -> selector "xpath" t
    where
      selector :: Text -> Text -> Value
      selector sn t = object ["using" .= sn, "value" .= t]
