{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad
import Control.Monad.Reader
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Spec as Spec
import System.Environment
import Test.Sandwich hiding (BrowserToUse(..))
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.WebDriver
import TestLib.Contexts.BrowserDependencies
import TestLib.Contexts.Selenium4
import TestLib.Contexts.StaticServer
import TestLib.Contexts.WebDriver
import TestLib.Types
import TestLib.Types.Cli
import UnliftIO.Process


main :: IO ()
main = do
  args <- getArgs
  putStrLn [i|args: #{args}|]

  clo <- parseCommandLineArgs userOptions (return ())

  let introduceSelenium3 :: forall ctx. (HasBaseContext ctx, HasNixContext ctx, HasBrowserDependencies ctx) => SpecFree (LabelValue "driverConfig" DriverConfig :> ctx) IO () -> SpecFree ctx IO ()
      introduceSelenium3 = introduce "Introduce Selenium 3" driverConfig alloc (const $ return ())
        where
          alloc = do
            Just dir <- getCurrentFolder
            java <- getBinaryViaNixPackage @"java" "jre"
            seleniumJar <- getFileViaNixPackage "selenium-server-standalone" tryFindSeleniumJar
            subDrivers <- getSubDrivers dir
            return $ DriverConfigSeleniumJar {
              driverConfigJava = java
              , driverConfigSeleniumJar = seleniumJar
              , driverConfigSubDrivers = subDrivers
              , driverConfigLogDir = dir
              }

  let introduceSelenium4 :: forall ctx. (HasBaseContext ctx, HasNixContext ctx, HasBrowserDependencies ctx) => SpecFree (LabelValue "driverConfig" DriverConfig :> ctx) IO () -> SpecFree ctx IO ()
      introduceSelenium4 = introduce "Introduce Selenium 4" driverConfig alloc (const $ return ())
        where
          alloc = do
            Just dir <- getCurrentFolder
            java <- getBinaryViaNixPackage @"java" "jre"
            seleniumJar <- getFileViaNixDerivation selenium4Derivation tryFindSeleniumJar
            subDrivers <- getSubDrivers dir
            return $ DriverConfigSeleniumJar {
              driverConfigJava = java
              , driverConfigSeleniumJar = seleniumJar
              , driverConfigSubDrivers = subDrivers
              , driverConfigLogDir = dir
              }

  let introduceChromedriver :: forall ctx. (HasBaseContext ctx, HasNixContext ctx) => SpecFree (LabelValue "driverConfig" DriverConfig :> ctx) IO () -> SpecFree ctx IO ()
      introduceChromedriver = introduce "Introduce chromedriver" driverConfig alloc (const $ return ())
        where
          alloc = do
            Just dir <- getCurrentFolder
            chrome <- getBinaryViaNixPackage @"google-chrome-stable" "google-chrome"
            chromedriver <- getBinaryViaNixPackage @"chromedriver" "chromedriver"
            return $ DriverConfigChromedriver {
              driverConfigChromedriver = chromedriver
              , driverConfigChrome = chrome
              , driverConfigLogDir = dir
              }

  let introduceGeckodriver :: forall ctx. (HasBaseContext ctx, HasNixContext ctx) => SpecFree (LabelValue "driverConfig" DriverConfig :> ctx) IO () -> SpecFree ctx IO ()
      introduceGeckodriver = introduce "Introduce geckodriver" driverConfig alloc (const $ return ())
        where
          alloc = do
            Just dir <- getCurrentFolder
            firefox <- getBinaryViaNixPackage @"firefox" "firefox"
            geckodriver <- getBinaryViaNixPackage @"geckodriver" "geckodriver"
            return $ DriverConfigGeckodriver {
              driverConfigGeckodriver = geckodriver
              , driverConfigFirefox = firefox
              , driverConfigLogDir = dir
              }

  let UserOptions {optBrowserToUse} = optUserOptions clo

  runSandwichWithCommandLineArgs' defaultOptions userOptions $
    introduceNixContext nixpkgsRelease $
    introduceStaticServer $
    introduceBrowserDependencies $ do
      describe "Selenium 3" $ introduceSelenium3 $ introduceWebDriverContext $ Spec.spec

      describe "Selenium 4" $ introduceSelenium4 $ introduceWebDriverContext $ Spec.spec

      when (optBrowserToUse == UseChrome) $
        describe "chromedriver direct" $ introduceChromedriver $ introduceWebDriverContext Spec.spec

      when (optBrowserToUse == UseFirefox) $
        describe "geckodriver direct" $ introduceGeckodriver $ introduceWebDriverContext Spec.spec


-- | Nixpkgs release 24.05, accessed 6\/30\/2025.
-- You can compute updated values for this release (or others) by running
-- nix-prefetch-github NixOS nixpkgs --rev release-25.05
-- We pin this here rather than using 'nixpkgsReleaseDefault' from sandwich-contexts
-- so that sandwich updates can't break this (especially on aarch64-darwin, where
-- google-chrome isn't available on older versions of release-24.05). Also, firefox
-- isn't available for macOS on older Nixpkgs releases.
nixpkgsRelease :: NixpkgsDerivation
nixpkgsRelease = NixpkgsDerivationFetchFromGitHub {
  nixpkgsDerivationOwner = "NixOS"
  , nixpkgsDerivationRepo = "nixpkgs"
  , nixpkgsDerivationRev = "32bd9b9bf9dd95eafff1e83da314c96719908657"
  , nixpkgsDerivationSha256 = "sha256-HXDDEjEBMycmwkOiU045bL3yuhOK1+nZZd3zsBh6zsA="
  , nixpkgsDerivationAllowUnfree = True
  }

getSubDrivers :: (MonadReader ctx m, HasBrowserDependencies ctx) => FilePath -> m [DriverConfig]
getSubDrivers dir = getContext browserDependencies >>= \case
  BrowserDependenciesChrome {..} -> return [
    DriverConfigChromedriver {
        driverConfigChromedriver = browserDependenciesChromeChromedriver
        , driverConfigChrome = browserDependenciesChromeChrome
        , driverConfigLogDir = dir
        }]
  BrowserDependenciesFirefox {..} -> return [
    DriverConfigGeckodriver {
        driverConfigGeckodriver = browserDependenciesFirefoxGeckodriver
        , driverConfigFirefox = browserDependenciesFirefoxFirefox
        , driverConfigLogDir = dir
        }]


tryFindSeleniumJar :: FilePath -> IO FilePath
tryFindSeleniumJar path = (T.unpack . T.strip . T.pack) <$> readCreateProcess (proc "find" [path, "-name", "*.jar"]) ""
