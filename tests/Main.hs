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
import TestLib.Contexts.StaticServer
import TestLib.Contexts.WebDriver
import TestLib.Types.Cli
import UnliftIO.Process


main :: IO ()
main = do
  args <- getArgs
  putStrLn [i|args: #{args}|]

  clo <- parseCommandLineArgs userOptions (return ())

  let CommandLineWebdriverOptions {..} = optWebdriverOptions clo

  runSandwichWithCommandLineArgs' defaultOptions userOptions $
    introduceNixContext (nixpkgsRelease { nixpkgsDerivationAllowUnfree = True }) $
    introduceBinaryViaNixPackage @"java" "jre" $
    (case optSeleniumJar of Just p -> introduceFile @"selenium.jar" p; Nothing -> introduceFileViaNixPackage' @"selenium.jar" "selenium-server-standalone" tryFindSeleniumJar) $
    (case optChromeBinary of Just p -> introduceFile @"google-chrome-stable" p; Nothing -> introduceBinaryViaNixPackage @"google-chrome-stable" "google-chrome") $
    introduceStaticServer $
    introduceBrowserDependencies $
    introduceWebDriver $ do
    Spec.spec


-- | Nixpkgs release 24.05, accessed 11\/9\/2024.
-- You can compute updated values for this release (or others) by running
-- nix-prefetch-github NixOS nixpkgs --rev release-24.05
-- We pin this here rather than using 'nixpkgsReleaseDefault' from sandwich-contexts
-- so that sandwich updates can't break this (especially on aarch64-darwin, where
-- google-chrome isn't available on older versions of release-24.05).
nixpkgsRelease :: NixpkgsDerivation
nixpkgsRelease = NixpkgsDerivationFetchFromGitHub {
  nixpkgsDerivationOwner = "NixOS"
  , nixpkgsDerivationRepo = "nixpkgs"
  , nixpkgsDerivationRev = "bb824c634c812feede9d398c000526401028c0e7"
  , nixpkgsDerivationSha256 = "sha256-xYnWv9kyJyF8rEZ1uJaSek2fmaIowkk/ovE6+MwcP2E="
  , nixpkgsDerivationAllowUnfree = False
  }


tryFindSeleniumJar :: FilePath -> IO FilePath
tryFindSeleniumJar path = (T.unpack . T.strip . T.pack) <$> readCreateProcess (proc "find" [path, "-name", "*.jar"]) ""
