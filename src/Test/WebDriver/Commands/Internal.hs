{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, GeneralizedNewtypeDeriving  #-}
{-# OPTIONS_HADDOCK not-home #-}
-- |Internal functions used to implement the functions exported by
-- "Test.WebDriver.Commands". These may be useful for implementing non-standard
-- webdriver commands.
module Test.WebDriver.Commands.Internal
       (-- * Low-level webdriver functions
         doCommand
        -- ** Commands with :sessionId URL parameter
       , doSessCommand, SessionId(..)
        -- ** Commands with element :id URL parameters
       , doElemCommand, Element(..)
        -- ** Commands with :windowHandle URL parameters
       , doWinCommand, WindowHandle(..), currentWindow
        -- * Exceptions
       , NoSessionId(..)
        -- * Parse a javascript file
       , clientScripts
       ) where

import Test.WebDriver.Classes
import Test.WebDriver.Utils (urlEncode)

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as M
import Control.Exception.Lifted
import Data.Typeable
import Data.Default
import Control.Applicative
import Language.JavaScript.Parser (JSNode(..), Node(..))
import qualified Language.JavaScript.Parser as JS

{- |An opaque identifier for a web page element. -}
newtype Element = Element Text
                  deriving (Eq, Ord, Show, Read)

instance FromJSON Element where
  parseJSON (Object o) = Element <$> o .: "ELEMENT"
  parseJSON v = typeMismatch "Element" v

instance ToJSON Element where
  toJSON (Element e) = object ["ELEMENT" .= e]


{- |An opaque identifier for a browser window -}
newtype WindowHandle = WindowHandle Text
                     deriving (Eq, Ord, Show, Read,
                               FromJSON, ToJSON)
instance Default WindowHandle where
  def = currentWindow

-- |A special 'WindowHandle' that always refers to the currently focused window.
-- This is also used by the 'Default' instance.
currentWindow :: WindowHandle
currentWindow = WindowHandle "current"

instance Exception NoSessionId
-- |A command requiring a session ID was attempted when no session ID was
-- available.
newtype NoSessionId = NoSessionId String
                 deriving (Eq, Show, Typeable)

-- |This a convenient wrapper around 'doCommand' that automatically prepends
-- the session URL parameter to the wire command URL. For example, passing
-- a URL of \"/refresh\" will expand to \"/session/:sessionId/refresh\", where
-- :sessionId is a URL parameter as described in
-- <http://code.google.com/p/selenium/wiki/JsonWireProtocol>
doSessCommand :: (WebDriver wd, ToJSON a, FromJSON b) =>
                  RequestMethod -> Text -> a -> wd b
doSessCommand method path args = do
  WDSession { wdSessId = mSessId } <- getSession
  case mSessId of
      Nothing -> throwIO . NoSessionId $ msg
        where
          msg = "doSessCommand: No session ID found for relative URL "
                ++ show path
      Just (SessionId sId) -> doCommand method
                              (T.concat ["/session/", urlEncode sId, path]) args

-- |A wrapper around 'doSessCommand' to create element URLs.
-- For example, passing a URL of "/active" will expand to
-- \"/session/:sessionId/element/:id/active\", where :sessionId and :id are URL
-- parameters as described in the wire protocol.
doElemCommand :: (WebDriver wd, ToJSON a, FromJSON b) =>
                  RequestMethod -> Element -> Text -> a -> wd b
doElemCommand m (Element e) path a =
  doSessCommand m (T.concat ["/element/", urlEncode e, path]) a

-- |A wrapper around 'doSessCommand' to create window handle URLS.
-- For example, passing a URL of \"/size\" will expand to
-- \"/session/:sessionId/window/:windowHandle/\", where :sessionId and
-- :windowHandle are URL parameters as described in the wire protocol
doWinCommand :: (WebDriver wd, ToJSON a, FromJSON b) =>
                 RequestMethod -> WindowHandle -> Text -> a -> wd b
doWinCommand m (WindowHandle w) path a =
  doSessCommand m (T.concat ["/window/", urlEncode w, path]) a


-- | Parse top level javascript commands
parseClientTop :: JSNode -> [(T.Text,T.Text)]
parseClientTop (NN (JSSourceElementsTop es)) = catMaybes $ map parseClientDef es
parseClientTop _ = []

-- | Parse a call clientSideScripts.somefunction = function() {...}, returning the function name
-- and the body.
parseClientDef :: JSNode -> Maybe (T.Text, T.Text)
parseClientDef (NN (JSExpression
                        [NN (JSMemberDot
                            [NT (JSIdentifier "clientSideScripts") _ _]
                            _ -- literal .
                            (NT (JSIdentifier name) _ _))
                        , NN (JSOperator (NT (JSLiteral "=") _ _))
                        , NN (JSFunctionExpression _ _ _ _ _ (NN (JSBlock _ body _)))
                        ]))
    = Just (T.pack name, T.pack $ dropWhile (=='\n') $ JS.renderToString $ NN $ JSSourceElementsTop body)
                        -- Use renderToString instead of renderJS so we don't need to depend on builder
parseClientDef _ = Nothing

-- | Parse a javascript file.  All toplevel commands which look like
--
-- >clientSideScripts.somefunction = function() {
-- > ...
-- >};
--
-- are loaded.  The return value is either an error message or
-- a map with keys the function names and value the body of the function.
clientScripts :: String -> Either String (M.HashMap T.Text T.Text)
clientScripts j = M.fromList . parseClientTop <$> JS.parse j "<client>"

