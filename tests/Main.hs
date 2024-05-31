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
import qualified Spec as Spec
import System.Environment
import Test.Sandwich hiding (BrowserToUse(..))
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.WebDriver
import TestLib.Types
import TestLib.WebDriverContext
import UnliftIO.Process


main :: IO ()
main = do
  args <- getArgs
  putStrLn [i|args: #{args}|]

  clo <- parseCommandLineArgs userOptions (return ())
  putStrLn [i|clo: #{clo}|]

  let CommandLineWebdriverOptions {..} = optWebdriverOptions clo

  runSandwichWithCommandLineArgs' defaultOptions userOptions $
    introduceNixContext (nixpkgsReleaseDefault { nixpkgsDerivationAllowUnfree = True }) $
    introduceBinaryViaNixPackage @"java" "jre" $
    (case optSeleniumJar of Just p -> introduceFile @"selenium.jar" p; Nothing -> introduceFileViaNixPackage @"selenium.jar" "selenium-server-standalone" tryFindSeleniumJar) $
    (case optChromeBinary of Just p -> introduceFile @"google-chrome-stable" p; Nothing -> introduceBinaryViaNixPackage @"google-chrome-stable" "google-chrome") $
    introduceBrowserDependencies (optBrowserToUse (optUserOptions clo)) $
    introduceWebDriver $ do
    Spec.spec


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
