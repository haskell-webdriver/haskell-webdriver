{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The HTTP/JSON plumbing used to implement the 'WD' monad.
--
-- These functions can be used to create your own 'WebDriver' instances, providing extra functionality for your application if desired. All exports
-- of this module are subject to change at any point.
--
module Test.WebDriver.Internal (
  getJSONResult

  , doCommand
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text)
import GHC.Stack
import Network.HTTP.Client (Response(..))
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status (Status(..))
import Prelude -- hides some "unused import" warnings
import Test.WebDriver.Capabilities.Aeson
import Test.WebDriver.Exceptions
import Test.WebDriver.JSON
import Test.WebDriver.Types
import UnliftIO.Exception (SomeException(..), toException, throwIO)


data SuccessResponse a = SuccessResponse {
  successValue :: a
  }
deriveFromJSON toCamel1 ''SuccessResponse

-- | Parse a 'FailedCommand' object from a given HTTP response.
getJSONResult :: (MonadIO m, FromJSON a) => Response ByteString -> m (Either SomeException a)
getJSONResult r
  -- | Errors
  | code >= 400 && code < 600 =
      case lookup hContentType headers of
        Just ct
          | "application/json" `BS.isInfixOf` ct -> do
              failedCommand :: FailedCommand <- parseJSON' body
              throwIO failedCommand
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
