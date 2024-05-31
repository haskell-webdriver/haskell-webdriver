{-# LANGUAGE RankNTypes #-}

module TestLib.Types.Cli where

import Options.Applicative


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
