{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import qualified Data.Aeson as A
import Data.String.Interpolate
import GHC.Stack
import Options.Applicative
import Test.Sandwich hiding (BrowserToUse(..))
import Test.WebDriver
import UnliftIO.Process


data BrowserToUse = UseChrome | UseFirefox
  deriving (Show)

browserToUse :: (forall f a. Mod f a) -> Parser BrowserToUse
browserToUse maybeInternal =
  flag' UseFirefox (long "firefox" <> help "Use Firefox" <> maybeInternal)
  <|> flag UseChrome UseChrome (long "chrome" <> help "Use Chrome (default)" <> maybeInternal)

data UserOptions = UserOptions {
  optSeleniumJar :: FilePath

  , optChromeBinary :: Maybe FilePath
  , optChromeDriver :: Maybe FilePath

  , optFirefoxBinary :: Maybe FilePath
  , optGeckoDriver :: Maybe FilePath

  , optBrowserToUse :: BrowserToUse
  } deriving (Show)


userOptions :: Parser UserOptions
userOptions = UserOptions
  <$> strOption (long "selenium-jar" <> help "selenium.jar file to use")

  <*> optional (strOption (long "chrome-binary" <> help "Path to Chrome binary"))
  <*> optional (strOption (long "chromedriver" <> help "Path to chromedriver"))

  <*> optional (strOption (long "firefox-binary" <> help "Path to Firefox binary"))
  <*> optional (strOption (long "geckodriver" <> help "Path to geckodriver"))

  <*> browserToUse mempty

type BaseMonad m = (HasCallStack, MonadUnliftIO m)
type BaseMonadContext m context = (BaseMonad m, HasBaseContext context)

introduceWebDriverOptions :: forall a context m. (BaseMonadContext m context, HasCommandLineOptions context a)
  => () -> SpecFree (LabelValue "webdriver" WebDriver :> context) m () -> SpecFree context m ()
introduceWebDriverOptions wdOptions = introduceWith "Introduce WebDriver session" webdriver alloc
  where
    alloc = do
      UserOptions {..} <- getUserCommandLineOptions

      javaArgs :: [String] <- case optBrowserToUse of
        UseChrome -> do
          chromedriverLog <- undefined
          -- return ([
          --   "-Dwebdriver.chrome.logfile=" <> chromedriverLog
          --   , "-Dwebdriver.chrome.verboseLogging=true"
          --   ]
          --   <> maybe [] ("-Dwebdriver.chrome.driver=" <>) optChromeDriver
          --   <> maybe [] ("-Dwebdriver.chrome.driver=" <>) optChromeBinary
          --   )
          undefined
        UseFirefox -> return []

      let cp = (proc "java" (javaArgs <> ["-jar", optSeleniumJar])) { create_group = True }

      withCreateProcess cp $ \_ _ _ p -> do
        undefined


spec :: TopSpecWithOptions' UserOptions
spec = do
  it "tests async" $ liftIO $ do
    runSession (defaultConfig { wdCapabilities = defaultCaps { browser = (chrome { chromeBinary = Just "/nix/store/nb4vdqyy0by2h75794aqhw409iccpvmq-google-chrome-125.0.6422.60/bin/google-chrome-stable" }) } }) . finallyClose $ do
      liftIO $ putStrLn "Got here 1"
      openPage "http://www.wikipedia.org/"
      liftIO $ putStrLn "Got here 2"
      r <- asyncJS [] "arguments[0]();"
      liftIO $ putStrLn "Got here 3"
      r `shouldBe` (Just A.Null)

main :: IO ()
main = do
  clo <- parseCommandLineArgs userOptions spec

  putStrLn [i|clo: #{clo}|]

  runSandwichWithCommandLineArgs' defaultOptions userOptions spec


  -- introduceWebDriverOptions @() (defaultWdOptions "/tmp/tools") $ do
  --   it "opens Google and searches" $ withSession1 $ do
  --     openPage [i|https://www.google.com|]
  --     search <- findElem (ByCSS [i|*[title="Search"]|])
  --     click search
  --     sendKeys "Haskell Sandwich" search
  --     findElem (ByCSS [i|input[type="submit"]|]) >>= click

  --     Just dir <- getCurrentFolder
  --     screenshot >>= liftIO . BL.writeFile (dir </> "screenshot.png")

  --     liftIO $ threadDelay 3000000
