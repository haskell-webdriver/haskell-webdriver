{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.String.Interpolate
import qualified Data.Text as T
import qualified Spec as Spec
import System.Environment
import Test.Sandwich hiding (BrowserToUse(..))
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import TestLib.Contexts.BrowserDependencies
import TestLib.Contexts.WebDriver
import TestLib.Types.Cli
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
    (case optSeleniumJar of Just p -> introduceFile @"selenium.jar" p; Nothing -> introduceFileViaNixPackage' @"selenium.jar" "selenium-server-standalone" tryFindSeleniumJar) $
    (case optChromeBinary of Just p -> introduceFile @"google-chrome-stable" p; Nothing -> introduceBinaryViaNixPackage @"google-chrome-stable" "google-chrome") $
    introduceBrowserDependencies $
    introduceWebDriver $ do
    Spec.spec


tryFindSeleniumJar :: FilePath -> IO FilePath
tryFindSeleniumJar path = (T.unpack . T.strip . T.pack) <$> readCreateProcess (proc "find" [path, "-name", "*.jar"]) ""
