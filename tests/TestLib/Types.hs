{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TestLib.Types where

import Control.Monad.IO.Unlift
import Data.Int
import GHC.Stack
import Options.Applicative
import Test.Sandwich hiding (BrowserToUse(..))


data BrowserToUse = UseChrome | UseFirefox
  deriving (Show)

browserToUse :: (forall f a. Mod f a) -> Parser BrowserToUse
browserToUse maybeInternal =
  flag' UseFirefox (long "firefox" <> help "Use Firefox" <> maybeInternal)
  <|> flag UseChrome UseChrome (long "chrome" <> help "Use Chrome (default)" <> maybeInternal)

data UserOptions = UserOptions {
  optSeleniumJar :: Maybe FilePath

  , optChromeBinary :: Maybe FilePath
  , optChromeDriver :: Maybe FilePath

  , optFirefoxBinary :: Maybe FilePath
  , optGeckoDriver :: Maybe FilePath

  , optBrowserToUse :: BrowserToUse
  } deriving (Show)


userOptions :: Parser UserOptions
userOptions = UserOptions
  <$> optional (strOption (long "selenium-jar" <> help "selenium.jar file to use"))

  <*> optional (strOption (long "chrome-binary" <> help "Path to Chrome binary"))
  <*> optional (strOption (long "chromedriver" <> help "Path to chromedriver"))

  <*> optional (strOption (long "firefox-binary" <> help "Path to Firefox binary"))
  <*> optional (strOption (long "geckodriver" <> help "Path to geckodriver"))

  <*> browserToUse mempty

type BaseMonad m = (HasCallStack, MonadUnliftIO m)
type BaseMonadContext m context = (BaseMonad m, HasBaseContext context)

data WebDriver = WebDriver {
  webDriverHostname :: String
  , webDriverPort :: Int16
  }

webdriver :: Label "webdriver" WebDriver
webdriver = Label
