module Main where

import Config
import Control.Concurrent
import Control.Exception
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Test.WebDriver
import WaiAppStatic.Types

--import qualified Test.BasicTests as BasicTests

serverConf = setPort Config.serverPort
           $ defaultSettings

serverStaticConf = defaultFileServerSettings staticContentPath

browsers = [firefox, chrome]

wdConfigs = map (`useBrowser` conf) browsers
  where
    conf = defaultConfig { wdPort = getPort serverConf }

main = bracket
  (forkIO $ runSettings serverConf (staticApp serverStaticConf))
  (\_ -> return ()) -- (\_ -> mapM_ BasicTests.runTestsWith wdConfigs )
  killThread
