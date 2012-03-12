{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Test.WebDriver.Internal 
       ( mkWDUri, mkRequest
       , Request(..), RequestMethod(..), Response(..)
              
       , doCommand', doSessCommand', doElemCommand', doWinCommand'
       , doCommand, doSessCommand, doElemCommand, doWinCommand
                    
                                         
       , handleHTTPErr, handleJSONErr, handleHTTPResp
       
       , selector, single, pair, triple
                                       
       , parseJSON', fromJSON', (!:), 
         parsePair, parseTriple
                                      
       ) where
import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.IO.Class
import Network.HTTP (simpleHTTP, Request(..), Response(..), RequestMethod(..))
import Network.HTTP.Headers (findHeader, Header(..), HeaderName(..))
import Network.URI
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Attoparsec.ByteString.Lazy (Result(..))
import qualified Data.Attoparsec.ByteString.Lazy as AP
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as H
import qualified Data.Vector as V
import Data.Text (Text)
import Data.List
import Data.Maybe
import Data.String

import Test.WebDriver.Types
import Test.WebDriver.Types.Internal


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
  liftIO . print $ req
  liftIO (simpleHTTP req) >>= either (throwError . HTTPConnError) return


doSessCommand :: (ToJSON a, FromJSON b) => RequestMethod -> Text -> a -> WD b
doSessCommand = doSessCommand' []

doElemCommand :: (ToJSON a, FromJSON b) => 
                 RequestMethod -> Element -> Text -> a -> WD b
doElemCommand = doElemCommand' []

doWinCommand :: (ToJSON a, FromJSON b) => 
                RequestMethod -> WindowHandle -> Text -> a -> WD b
doWinCommand = doWinCommand' []

doCommand :: (ToJSON a, FromJSON b) => RequestMethod -> Text -> a -> WD b
doCommand = doCommand' []

doSessCommand' :: (ToJSON a, FromJSON b) => [Header] -> RequestMethod -> Text -> a -> WD b
doSessCommand' headers method path args = do
  WDSession { wdSessId = mSessId } <- get
  case mSessId of 
      Nothing -> throwError . NoSessionId $ 
                 "No session ID found when making request for relative URL " 
                 ++ show path
      Just (SessionId sId) -> doCommand' headers method 
                              (T.concat ["/session/", sId, path]) args

doWinCommand' :: (ToJSON a, FromJSON b) => 
                [Header] -> RequestMethod -> WindowHandle -> Text -> a -> WD b
doWinCommand' h m (WindowHandle w) path a = 
  doSessCommand' h m (T.concat ["/window/", w, path]) a

doElemCommand' :: (ToJSON a, FromJSON b) => 
                 [Header] -> RequestMethod -> Element -> Text -> a -> WD b
doElemCommand' h m (Element e) path a =
  doSessCommand' h m (T.concat ["/element/", e, path]) a

doCommand' :: (ToJSON a, FromJSON b) => 
                [Header] -> RequestMethod -> Text -> a -> WD b  
doCommand' headers method path args = do
  r <- mkRequest headers method path args
  liftIO . print $ r
  handleHTTPErr r
  --liftIO . print . rspBody $ r
  handleHTTPResp r

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
  sess <- get
  errInfo <- fromJSON' val
  let errInfo' = errInfo { errSessId = wdSessId sess } 
      e errType = throwError $ FailedCommand errType errInfo'
  case status of
    7   -> e NoSuchElement
    8   -> e NoSuchFrame
    9   -> throwError . UnknownCommand . errMsg $ errInfo
    10  -> e StaleElementReference
    11  -> e ElementNotVisible
    12  -> e InvalidElementState
    13  -> e UnknownError
    15  -> e ElementIsNotSelectable
    17  -> e JavascriptError
    19  -> e XPathLookupError
    21  -> e Timeout
    23  -> e NoSuchWindow
    24  -> e InvalidCookieDomain
    25  -> e UnableToSetCookie
    26  -> e UnexpectedAlertOpen
    27  -> e NoAlertOpenError
    28  -> e ScriptTimeout
    29  -> e InvalidElementCoordinates
    30  -> e IMENotAvailable
    31  -> e IMEEngineActivationFailed        
    32  -> e InvalidSelector
    34  -> e MoveTargetOutOfBounds
    51  -> e InvalidXPathSelector
    52  -> e InvalidXPathSelectorReturnType
    405 -> e MethodNotAllowed
    _   -> e UnknownError


apResultToWD :: FromJSON a => AP.Result Value -> WD a
apResultToWD p = case p of
  Done _ res -> fromJSON' res
  Fail _ _ err -> throwError $ BadJSON err

aesonResultToWD r = case r of
  Success val -> return val
  Error err   -> throwError $ BadJSON err

parseJSON' :: FromJSON a => ByteString -> WD a
parseJSON' = apResultToWD . AP.parse json

fromJSON' :: FromJSON a => Value -> WD a
fromJSON' = aesonResultToWD . fromJSON

(!:) :: FromJSON a => Object -> Text -> WD a
o !: k = aesonResultToWD $ parse (.: k) o

parsePair :: (FromJSON a, FromJSON b) => 
             String -> String -> String -> Value -> WD (a, b)
parsePair a b funcName v = 
  case v of
    Object o -> (,) <$> o !: fromString a <*> o !: fromString b
    _        -> throwError . BadJSON $ funcName ++ 
                ": cannot parse non-object JSON response as a (" ++ a  
                ++ ", " ++ b ++ ") pair" ++ ")"

parseTriple a b c funcName v = 
  case v of
    Object o -> (,,) <$> o !: fromString a 
                     <*> o !: fromString b 
                     <*> o !: fromString c
    _        -> throwError . BadJSON $ funcName ++
                ": cannot parse non-object JSON response as a (" ++ a
                ++ ", " ++ b ++ ", " ++ c ++ ") pair"
                
selector :: Selector -> Text -> Value
selector s t = object ["using" .= s, "value" .= t]

single :: ToJSON a => Text -> a -> Value
single a x = object [a .= x]

pair :: (ToJSON a, ToJSON b) => (Text,Text) -> (a,b) -> Value
pair (a,b) (x,y) = object [a .= x, b .= y]

triple :: (ToJSON a, ToJSON b, ToJSON c) => 
          (Text,Text,Text) -> (a,b,c) -> Value
triple (a,b,c) (x,y,z) = object [a .= x, b.= y, c .= z]