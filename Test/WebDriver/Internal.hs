{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Test.WebDriver.Internal 
       ( mkWDUri, mkRequest
       , Request(..), RequestMethod(..), Response(..)
                                         
       , handleHTTPErr, handleJSONErr, handleHTTPResp
       ) where

import Test.WebDriver.Types
import Test.WebDriver.Types.Internal
import Test.WebDriver.JSON
import Network.HTTP (simpleHTTP, Request(..), Response(..), RequestMethod(..))
import Network.HTTP.Headers (findHeader, Header(..), HeaderName(..))
import Network.URI
import Data.Aeson
import Data.Aeson.Types (emptyArray)

import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Vector as V

--import Control.Applicative
import Control.Monad.Error (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Data.List (isInfixOf)
import Data.Maybe (fromJust)  -- used with relativeTo
import Data.String (fromString)

mkWDUri :: String -> WD URI  --todo: remove String :(
mkWDUri path = do 
  WDSession{wdHost = host, 
            wdPort = port
           } <- get
  let urlStr   = "http://" ++ host ++ ":" ++ show port
      relPath  = "/wd/hub" ++ path
      mBaseURI = parseAbsoluteURI urlStr
      mRelURI  = parseRelativeReference relPath
  case (mBaseURI, mRelURI) of
    (Nothing, _) -> throwError $ InvalidURL urlStr 
    (_, Nothing) -> throwError $ InvalidURL relPath
    (Just baseURI, Just relURI) -> return . fromJust $ relURI `relativeTo` baseURI

mkRequest :: ToJSON a => [Header] -> RequestMethod -> Text -> a -> WD (Response ByteString)
mkRequest headers method path args = do
  uri <- mkWDUri (T.unpack path)
  let body = case toJSON args of
        Array v | V.null v -> ""   --an ugly corner case to allow empty requests
        other              -> encode other 
      req = Request { rqURI = uri             --todo: normalization of headers
                    , rqMethod = method
                    , rqBody = body
                    , rqHeaders = headers ++ [ Header HdrAccept 
                                               "application/json;charset=UTF-8"
                                             ,Header HdrContentType 
                                               "application/json;charset=UTF-8"
                                             , Header HdrContentLength 
                                               . show . BS.length $ body 
                                             ]
                    }
--  liftIO . print $ req
  liftIO (simpleHTTP req) >>= either (throwError . HTTPConnError) return


handleHTTPErr :: Response ByteString -> WD ()
handleHTTPErr r@Response{rspBody = body, rspCode = code, rspReason = reason} = 
  case code of 
    (4,_,_)  -> err UnknownCommand
    (5,_,_)  -> 
      case findHeader HdrContentType r of
        Just ct
          | "application/json;" `isInfixOf` ct -> parseJSON' body 
                                                  >>= handleJSONErr       
          | otherwise -> err ServerError
        Nothing -> 
          err (ServerError . ("Missing content type. Server response: "++))

    (2,_,_)  -> return ()
    (3,0,2)  -> return ()
    _        -> err (HTTPStatusUnknown code)
    where 
      err errType = throwError $ errType reason
      
handleHTTPResp ::  FromJSON a => Response ByteString -> WD a
handleHTTPResp resp@Response{rspBody = body, rspCode = code} = 
  case code of
    (2,0,4) -> returnEmptyArray
    (3,0,2) -> fromJSON' =<< maybe statusErr (return . String . fromString) 
                 (findHeader HdrLocation resp)
               where 
                 statusErr = throwError . HTTPStatusUnknown code  
                             $ (BS.unpack body)
    other 
      | BS.null body -> returnEmptyArray
      | otherwise -> fromJSON' . rspVal =<< parseJSON' body
  where
    returnEmptyArray = fromJSON' emptyArray
    
handleJSONErr :: WDResponse -> WD ()
handleJSONErr WDResponse{rspStatus = 0} = return ()
handleJSONErr WDResponse{rspVal = val, rspStatus = status} = do
  info <- fromJSON' val
  throwError . ($ errMsg info) $ case status of
    7   -> NoSuchElement
    8   -> NoSuchFrame
    9   -> UnknownCommand
    10  -> StaleElementReference
    11  -> ElementNotVisible
    12  -> InvalidElementState
    15  -> ElementIsNotSelectable
    17  -> JavascriptError
    19  -> XPathLookupError
    21  -> Timeout
    23  -> NoSuchWindow
    24  -> InvalidCookieDomain
    25  -> UnableToSetCookie
    26  -> UnexpectedAlertOpen
    27  -> NoAlertOpen
    28  -> ScriptTimeout
    29  -> InvalidElementCoordinates
    30  -> IMENotAvailable
    31  -> IMEEngineActivationFailed        
    32  -> InvalidSelector
    34  -> MoveTargetOutOfBounds
    51  -> InvalidXPathSelector
    52  -> InvalidXPathSelectorReturnType
    405 -> MethodNotAllowed
    _   -> const $ UnknownError info
