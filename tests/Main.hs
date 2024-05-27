{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
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
import qualified Data.Text as T
import System.Environment
import Test.Sandwich hiding (BrowserToUse(..))
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.WebDriver
import TestLib.Types
import TestLib.WebDriverContext
import UnliftIO.Process


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

  -- it "takes a screenshot" $ liftIO $ do
  --   openPage [i|https://www.google.com|]
  --   search <- findElem (ByCSS [i|*[title="Search"]|])
  --   click search
  --   sendKeys "Haskell Sandwich" search
  --   findElem (ByCSS [i|input[type="submit"]|]) >>= click

  --   Just dir <- getCurrentFolder
  --   screenshot >>= liftIO . BL.writeFile (dir </> "screenshot.png")

  --   liftIO $ threadDelay 3000000


main :: IO ()
main = do
  args <- getArgs
  putStrLn [i|args: #{args}|]

  clo <- parseCommandLineArgs userOptions spec
  putStrLn [i|clo: #{clo}|]

  let CommandLineWebdriverOptions {..} = optWebdriverOptions clo

  runSandwichWithCommandLineArgs' defaultOptions userOptions $
    introduceNixContext (nixpkgsReleaseDefault { nixpkgsDerivationAllowUnfree = True }) $
    introduceBinaryViaNixPackage @"java" "jre" $
    (case optSeleniumJar of Just p -> introduceFile @"selenium.jar" p; Nothing -> introduceFileViaNixPackage @"selenium.jar" "selenium-server-standalone" tryFindSeleniumJar) $
    introduceBrowserDependencies (optBrowserToUse (optUserOptions clo)) $
    introduceWebDriver $
    spec


introduceBrowserDependencies :: forall m context. (
  MonadUnliftIO m, HasBaseContext context, HasNixContext context
  ) => BrowserToUse -> SpecFree (LabelValue "browserDependencies" BrowserDependencies :> context) m () -> SpecFree context m ()
introduceBrowserDependencies browser = introduce "Introduce browser dependencies" browserDependencies alloc (const $ return ())
  where
    alloc = do
      deps <- case browser of
        UseChrome ->
          BrowserDependenciesChrome <$> getBinaryViaNixPackage @"google-chrome-stable" "google-chrome"
                                    <*> getBinaryViaNixPackage @"chromedriver" "chromedriver"
        UseFirefox ->
          BrowserDependenciesFirefox <$> getBinaryViaNixPackage @"firefox" "firefox"
                                     <*> getBinaryViaNixPackage @"geckodriver" "geckodriver"
      debug [i|Got browser dependencies: #{deps}|]
      return deps

tryFindSeleniumJar :: FilePath -> IO FilePath
tryFindSeleniumJar path = (T.unpack . T.strip . T.pack) <$> readCreateProcess (proc "find" [path, "-name", "*.jar"]) ""
