module Main where

import Control.Exception
import Control.Concurrent

import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import WaiAppStatic.Types

import Config
import Test.WebDriver

--import qualified Test.BasicTests as BasicTests

serverConf = setPort Config.serverPort
           $ defaultSettings

serverStaticConf = defaultFileServerSettings staticContentPath

browsers = [firefox, chrome]

wdConfigs = map (`useBrowser` conf) browsers
    where 
    conf = defaultConfig
        { wdPort = getPort serverConf 
        } 

main = bracket
    ( forkIO $ runSettings serverConf (staticApp serverStaticConf) )
    (\_ -> return () ) -- (\_ -> mapM_ BasicTests.runTestsWith wdConfigs )
    killThread
