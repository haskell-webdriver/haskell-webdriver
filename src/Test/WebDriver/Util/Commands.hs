{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-deriving-typeable #-}

{-# OPTIONS_HADDOCK not-home #-}
-- | Internal functions used to implement the functions exported by
-- "Test.WebDriver.Commands". These may be useful for implementing non-standard
-- webdriver commands.
module Test.WebDriver.Util.Commands (
  -- * Low-level webdriver functions
  doCommand
  -- ** Commands with :sessionId URL parameter
  , doSessCommand
  , SessionId(..)
  -- ** Commands with element :id URL parameters
  , doElemCommand
  , Element(..)
  -- ** Commands with :windowHandle URL parameters
  , WindowHandle(..)

  -- * Helpers
  , urlEncode
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding as TE
import GHC.Stack
import Network.HTTP.Client (Response(..))
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status (Status(..))
import qualified Network.HTTP.Types.URI as HTTP
import Prelude -- hides some "unused import" warnings
import Test.WebDriver.Capabilities.Aeson
import Test.WebDriver.Exceptions
import Test.WebDriver.JSON
import Test.WebDriver.Types
import Test.WebDriver.Util.Aeson
import UnliftIO.Exception


data SuccessResponse a = SuccessResponse {
  successValue :: a
  }
deriveFromJSON toCamel1 ''SuccessResponse

-- | An opaque identifier for a web page element
newtype Element = Element Text
  deriving (Eq, Ord, Show, Read)

instance FromJSON Element where
  parseJSON (Object o) = case fmap snd (aesonToList o) of
    (String id' : _) -> pure $ Element id'
    _ -> fail "No elements returned"
  parseJSON v = typeMismatch "Element" v

instance ToJSON Element where
  toJSON (Element e) = object ["element-6066-11e4-a52e-4f735466cecf" .= e]


-- | An opaque identifier for a browser window
newtype WindowHandle = WindowHandle Text
  deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)

-- | This a convenient wrapper around 'doCommand' that automatically prepends
-- the session URL parameter to the wire command URL. For example, passing
-- a URL of \"/refresh\" will expand to \"/session/:sessionId/refresh\", where
-- :sessionId is a URL parameter as described in
-- <https://github.com/SeleniumHQ/selenium/wiki/JsonWireProtocol>
doSessCommand :: (
  HasCallStack, WebDriver wd, ToJSON a, FromJSON b
  ) => Method -> Text -> a -> wd b
doSessCommand method path args = do
  Session { sessionId = SessionId sId } <- getSession
  -- Catch BadJSON exceptions here, since most commands go through this function.
  -- Then, re-throw them with "error", which automatically appends a callstack
  -- to the message in modern GHCs.
  -- This callstack makes it easy to see which command caused the BadJSON exception,
  -- without exposing too many internals.
  catch
    (doCommand method (T.concat ["/session/", urlEncode sId, path]) args)
    (\(e :: BadJSON) -> error $ show e)

-- | A wrapper around 'doSessCommand' to create element URLs.
-- For example, passing a URL of "/active" will expand to
-- \"/session/:sessionId/element/:id/active\", where :sessionId and :id are URL
-- parameters as described in the wire protocol.
doElemCommand :: (
  HasCallStack, WebDriver wd, ToJSON a, FromJSON b
  ) => Method -> Element -> Text -> a -> wd b
doElemCommand m (Element e) path a =
  doSessCommand m (T.concat ["/element/", urlEncode e, path]) a

urlEncode :: Text -> Text
urlEncode = TE.decodeUtf8 . HTTP.urlEncode False . TE.encodeUtf8

-- | Parse a 'FailedCommand' object from a given HTTP response.
getJSONResult :: (MonadIO m, FromJSON a) => Response ByteString -> m (Either SomeException a)
getJSONResult r
  | code >= 400 && code < 600 =
      case lookup hContentType headers of
        Just ct
          | "application/json" `BS.isInfixOf` ct -> do
              SuccessResponse {..} :: SuccessResponse FailedCommand <- parseJSON' body
              throwIO successValue
          | otherwise ->
              return $ Left $ toException $ ServerError reason
        Nothing ->
          return $ Left $ toException $ ServerError ("HTTP response missing content type. Server reason was: " <> reason)
  | code >= 200 && code < 300 = do
      SuccessResponse {successValue} <- parseJSON' body
      return $ Right successValue
  | otherwise = return $ Left $ toException $ (HTTPStatusUnknown code) reason
  where
    code = statusCode status
    reason = BS.unpack $ statusMessage status
    status = responseStatus r
    body = responseBody r
    headers = responseHeaders r


doCommand :: (
  HasCallStack, WebDriver m, ToJSON a, FromJSON b
  )
  -- | HTTP request method
  => Method
  -- | URL of request
  -> Text
  -- | JSON parameters passed in the body of the request. Note that, as a
  -- special case, anything that converts to Data.Aeson.Null will result in an
  -- empty request body.
  -> a
  -- | The JSON result of the HTTP request.
  -> m b
doCommand method url params = do
  Session {sessionDriver} <- getSession
  doCommandBase sessionDriver method url params
    >>= getJSONResult
    >>= either throwIO return
