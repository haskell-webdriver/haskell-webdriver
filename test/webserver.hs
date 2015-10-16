module Main where

import Control.Exception
import Control.Concurrent

import Network.Wai.Handler.Warp
import Network.Wai.Application.Static

import Test.Config as Config
import Test.WebDriver

import qualified Test.BasicTests as BasicTests

serverConf = setPort Config.serverPort
	       $ defaultSettings

serverStaticConf = defaultFileServerSettings
	{ ssLookupFile = toPieces [Config.staticContentPath]
	}

browsers = [firefox, chrome]

wdConfigs = map (`useBrowser` conf) browser
	where conf = defaultConfig
		{ wdPort = getPort serverConf 
		} 

main = bracket
	( runSettings serverConf (staticApp serverStaticConf) )
	(\_ -> mapM_ BasicTests.runTestsWith wdConfigs )
	killThread
