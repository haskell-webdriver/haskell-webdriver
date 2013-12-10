{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
-- | This module exposes WD actions that can be used to interact with a page
-- which uses <http://angularjs.org/ AngularJs>.  This provides similar functionality
-- as <https://github.com/angular/protractor protractor> and in fact we share some code
-- with protractor.
module Test.WebDriver.Commands.Angular
    (
    -- * Loading
      waitForAngular

    -- * Searching for elements
    , NgSelector(..)
    , findNg
    , findNgFrom
    , NgRepeater(..)
    , findRepeater
    , findRepeaterFrom

    -- * Misc
    , ngEvaluate
    , getLocationAbsUrl
    ) where

import Control.Applicative ((<$>))
import Test.WebDriver.Classes
import Test.WebDriver.Commands
import Test.WebDriver.JSON (fromJSON')
import Test.WebDriver.Commands.Internal (clientScripts)
import Language.Haskell.TH (runIO, litE, stringL)
import qualified Data.Aeson as A
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T

-- | Map of the clientsidescripts for angular
cs :: M.HashMap T.Text T.Text
cs = either (\err -> error $ "Error parsing scripts " ++ err) id mhash
    where
        mhash = clientScripts j
        j = $(runIO (readFile "js/angular-clientsidescripts.js") >>= litE . stringL)

execCS :: (WebDriver wd, A.FromJSON a) => T.Text -> [JSArg] -> wd a
execCS script arg = executeJS arg body
    where
        body = maybe (error $ "Unable to find " ++ T.unpack script) id $ M.lookup script cs

-- | Variant of execCS that fails properly on Null
execElems :: WebDriver wd => T.Text -> [JSArg] -> wd [Element]
execElems script arg = do
    x <- execCS script arg
    case x of
        A.Null -> return []
        _ -> fromJSON' x

asyncCS :: (WebDriver wd, A.FromJSON a) => T.Text -> [JSArg] -> wd (Maybe a)
asyncCS script arg = asyncJS arg body
    where
        body = maybe (error $ "Unable to find " ++ T.unpack script) id $ M.lookup script cs

-- | Wait until Angular has finished rendering before continuing.  @False@ indicates the timeout
-- was hit (see 'setScriptTimeout') and we stopped waiting and @True@ means that angular has
-- finished rendering.
waitForAngular :: WebDriver wd => T.Text -- ^ CSS selector to element which has ng-app
                               -> wd Bool
waitForAngular sel = do
    a <- asyncCS "waitForAngular" [JSArg sel] :: WebDriver wd => wd (Maybe ())
    return $ maybe False (const True) a

data NgSelector = 
    ByBinding T.Text -- ^ Argument is the binding, e.g. {{dog.name}}
  | ByModel T.Text   -- ^ Argument is the model name.  This is just a combination of 'ByInput', 'ByTextarea', and 'BySelect'
  | ByInput T.Text   -- ^ Input element with matching model name, e.g. @\<input ng-model=\"name\" ...\>@.
  | ByTextarea T.Text -- ^ Textarea element with matching model name, e.g. @\<textarea ng-model=\"name\" ... \>@
  | BySelect T.Text   -- ^ Select element with matching model name, e.g. @\<select ng-model=\"name\" ... \>@
  | BySelectedOption T.Text -- ^ Selected options with a select element matching the modelname.  That is,
                          --   The @\<option:checked\>@ elements within a @\<select ng-model=\"name\" ... \>@.

data NgRepeater =
    ByRows T.Text    -- ^ All the rows matching the repeater (e.g. 'dog in dogs')
  | ByRow T.Text Int -- ^ A single row specified by the text of the repeater (e.g. 'dog in dogs') and the row index
  | ByColumn T.Text T.Text -- ^ A single column matching the text of the repeater (e.g. 'dog in dogs') and the
                         -- column binding (e.g '{{dog.name}}').
  | ByRowAndCol T.Text Int T.Text -- ^ A single row and column, given (repeater, row index, column binding).

-- | Find elements from the document matching the given Angular selector.
findNg :: WebDriver wd => NgSelector -> wd [Element]
findNg = findNg' $ JSArg A.Null

-- | Find elements from within the given element which match the given Angular selector.
findNgFrom :: WebDriver wd => Element -> NgSelector -> wd [Element]
findNgFrom e = findNg' $ JSArg e

findNg' :: WebDriver wd => JSArg -> NgSelector -> wd [Element]
findNg' e (ByBinding name) = execElems "findBindings" [e, JSArg name]
findNg' e (ByInput name) = execElems "findInputs" [e, JSArg name]
findNg' e (ByTextarea name) = execElems "findTextareas" [e, JSArg name]
findNg' e (BySelect name) = execElems "findSelects" [e, JSArg name]
findNg' e (BySelectedOption name) = execElems "findSelectedOptions" [e, JSArg name]
findNg' e (ByModel name) = concat <$> sequence [ findNg' e $ ByInput name
                                               , findNg' e $ ByTextarea name
                                               , findNg' e $ BySelect name
                                               ]

-- | Find elements from the document which match the Angular repeater.
findRepeater :: WebDriver wd => NgRepeater -> wd [Element]
findRepeater = findRepeater' $ JSArg A.Null

-- | Find elements from the given element which match the Angular repeater.
findRepeaterFrom :: WebDriver wd => Element -> NgRepeater -> wd [Element]
findRepeaterFrom e = findRepeater' $ JSArg e

findRepeater' :: WebDriver wd => JSArg -> NgRepeater -> wd [Element]
findRepeater' e (ByRows rep) = execElems "findAllRepeaterRows" [e, JSArg rep]
findRepeater' e (ByRow rep idx) = execElems "findRepeaterRows" [e, JSArg rep, JSArg idx]
findRepeater' e (ByColumn rep idx) = execElems "findRepeaterColumn" [e, JSArg rep, JSArg idx]
findRepeater' e (ByRowAndCol rep row col) = execElems "findRepeaterElement" [e, JSArg rep, JSArg row, JSArg col]

-- | Evaluate an angular expression, using the scope attached to the given element.
ngEvaluate :: (WebDriver wd, A.FromJSON a) 
           => Element -- ^ element in whose scope to evaluate
           -> T.Text  -- ^ expression to evaluate, e.g. \"dog.name | reverse\"
           -> wd a
ngEvaluate e expr = execCS "evaluate" [JSArg e, JSArg expr]

-- | Return the current absolute url according to Angular (using @$location.absUrl()@).
getLocationAbsUrl :: WebDriver wd => T.Text -- ^ CSS selector to element which has ng-app
                                  -> wd T.Text
getLocationAbsUrl sel = execCS "getLocationAbsUrl" [JSArg sel]
