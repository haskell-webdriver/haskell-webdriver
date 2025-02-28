{-# LANGUAGE RankNTypes #-}

module TestLib.Types.Cli where

import Options.Applicative


data BrowserToUse = UseChrome | UseFirefox
  deriving (Show)

browserToUse :: (forall f a. Mod f a) -> Parser BrowserToUse
browserToUse maybeInternal =
  flag' UseFirefox (long "use-firefox" <> help "Use Firefox" <> maybeInternal)
  <|> flag UseChrome UseChrome (long "use-chrome" <> help "Use Chrome (default)" <> maybeInternal)

data UserOptions = UserOptions {
  optChromeBinary :: Maybe FilePath
  , optChromeDriver :: Maybe FilePath
  , optChromeNoSandbox :: Maybe Bool

  , optFirefoxBinary :: Maybe FilePath
  , optGeckoDriver :: Maybe FilePath

  , optBrowserToUse :: BrowserToUse

  , optHeadlessTests :: Maybe Bool
  } deriving (Show)

userOptions :: Parser UserOptions
userOptions = UserOptions
  <$> optional (strOption (long "webdriver-chrome" <> help "Path to Chrome binary"))
  <*> optional (strOption (long "webdriver-chromedriver" <> help "Path to chromedriver"))
  <*> optional (flag False True (long "webdriver-chrome-no-sandbox" <> help "Pass the --no-sandbox flag to Chrome (useful in GitHub Actions when installing Chrome via Nia)"))

  <*> optional (strOption (long "webdriver-firefox" <> help "Path to Firefox binary"))
  <*> optional (strOption (long "webdriver-geckodriver" <> help "Path to geckodriver"))

  <*> browserToUse mempty

  <*> optional (flag False True (long "headless-tests" <> help "Run the test browser in headless mode"))
