{-# LANGUAGE ExistentialQuantification #-}

module Test.WebDriver.Commands.DocumentHandling (
  getSource
  , executeJS
  , asyncJS
  , JSArg(..)
  ) where

import Control.Applicative
import Control.Exception.Safe (throwIO, handle)
import Control.Monad
import Data.Aeson as A
import Data.CallStack
import qualified Data.Foldable as F
import Data.Maybe
import Data.Text (Text)
import Prelude -- hides some "unused import" warnings
import Test.WebDriver.Class
import Test.WebDriver.Commands.Internal
import Test.WebDriver.Exceptions.Internal
import Test.WebDriver.JSON


-- | Get the current page source
getSource :: (HasCallStack, WebDriver wd) => wd Text
getSource = doSessCommand methodGet "/source" Null

{- | Inject a snippet of Javascript into the page for execution in the
context of the currently selected frame. The executed script is
assumed to be synchronous and the result of evaluating the script is
returned and converted to an instance of FromJSON.

The first parameter defines a sequence of arguments to pass to the javascript
function. Arguments of type Element will be converted to the
corresponding DOM element. Likewise, any elements in the script result
will be returned to the client as Elements.

The second parameter defines the script itself in the form of a
function body. The value returned by that function will be returned to
the client. The function will be invoked with the provided argument
list and the values may be accessed via the arguments object in the
order specified.

When using 'executeJS', GHC might complain about an ambiguous type in
situations where the result of the executeJS call is ignored/discard.
Consider the following example:

@
	jsExample = do
		e <- findElem (ByCSS "#foo")
		executeJS [] "someAction()"
		return e
@

Because the result of the 'executeJS' is discarded, GHC cannot resolve
which instance of the 'fromJSON' class to use when parsing the
Selenium server response. In such cases, we can use the 'ignoreReturn'
helper function located in "Test.WebDriver.JSON". 'ignoreReturn' has
no runtime effect; it simply helps the type system by expicitly providing
a `fromJSON` instance to use.

@
	import Test.WebDriver.JSON (ignoreReturn)
	jsExample = do
		e <- findElem (ByCSS "#foo")
		ignoreReturn $ executeJS [] "someAction()"
		return e
@
-}
executeJS :: (HasCallStack, F.Foldable f, FromJSON a, WebDriver wd) => f JSArg -> Text -> wd a
executeJS a s = do
  (doSessCommand methodPost "/execute/sync" . pair ("args", "script") $ (F.toList a,s))
    >>= fromJSON'

-- | Executes a snippet of Javascript code asynchronously. This function works
-- similarly to 'executeJS', except that the Javascript is passed a callback
-- function as its final argument. The script should call this function
-- to signal that it has finished executing, passing to it a value that will be
-- returned as the result of asyncJS. A result of Nothing indicates that the
-- Javascript function timed out (see 'setScriptTimeout')
asyncJS :: (HasCallStack, F.Foldable f, FromJSON a, WebDriver wd) => f JSArg -> Text -> wd (Maybe a)
asyncJS a s = handle timeout $ do
  Just <$> (fromJSON' =<< getResult "/execute/async")

  where
    getResult endpoint = doSessCommand methodPost endpoint . pair ("args", "script") $ (F.toList a,s)

    timeout (FailedCommand Timeout _)       = return Nothing
    timeout (FailedCommand ScriptTimeout _) = return Nothing
    timeout err = throwIO err

-- | An existential wrapper for any 'ToJSON' instance. This allows us to pass
-- parameters of many different types to Javascript code.
data JSArg = forall a. ToJSON a => JSArg a

instance ToJSON JSArg where
  toJSON (JSArg a) = toJSON a
