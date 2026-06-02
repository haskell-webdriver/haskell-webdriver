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
              , driverConfigLogDir = Just dir
              , driverConfigJavaFlags = []
              , driverConfigJavaExtraEnv = Nothing
              , driverConfigSeleniumVersion = Just Selenium3
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
              , driverConfigLogDir = Just dir
              , driverConfigJavaFlags = []
              , driverConfigJavaExtraEnv = Nothing
              , driverConfigSeleniumVersion = Just Selenium4
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
              , driverConfigLogDir = Just dir
              , driverConfigChromedriverFlags = []
              , driverConfigChromedriverExtraEnv = Nothing
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
              , driverConfigLogDir = Just dir
              , driverConfigGeckodriverFlags = []
              , driverConfigGeckodriverExtraEnv = Nothing
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


-- | Nixpkgs nixos-unstable, accessed 6\/2\/2026 (provides google-chrome 148.0.7778.215).
-- You can compute updated values for this release (or others) by running
-- nix-prefetch-github NixOS nixpkgs --rev nixos-unstable
-- We pin this here rather than using 'nixpkgsReleaseDefault' from sandwich-contexts
-- so that sandwich updates can't break this. Note that 'google-chrome' fetches a
-- versioned .deb from Google, which gets removed once a newer Chrome stable ships, so
-- this rev needs bumping periodically to a Nixpkgs whose google-chrome is current.
nixpkgsRelease :: NixpkgsDerivation
nixpkgsRelease = NixpkgsDerivationFetchFromGitHub {
  nixpkgsDerivationOwner = "NixOS"
  , nixpkgsDerivationRepo = "nixpkgs"
  , nixpkgsDerivationRev = "331800de5053fcebacf6813adb5db9c9dca22a0c"
  , nixpkgsDerivationSha256 = "sha256-x5UQuRsH3MqI0U9afaXSNqzTPSeZlRLvFAav2Ux1pNw="
  , nixpkgsDerivationAllowUnfree = True
  }

getSubDrivers :: (MonadReader ctx m, HasBrowserDependencies ctx) => FilePath -> m [DriverConfig]
getSubDrivers dir = getContext browserDependencies >>= \case
  BrowserDependenciesChrome {..} -> return [
    DriverConfigChromedriver {
        driverConfigChromedriver = browserDependenciesChromeChromedriver
        , driverConfigChrome = browserDependenciesChromeChrome
        , driverConfigLogDir = Just dir
        , driverConfigChromedriverFlags = []
        , driverConfigChromedriverExtraEnv = Nothing
        }]
  BrowserDependenciesFirefox {..} -> return [
    DriverConfigGeckodriver {
        driverConfigGeckodriver = browserDependenciesFirefoxGeckodriver
        , driverConfigFirefox = browserDependenciesFirefoxFirefox
        , driverConfigLogDir = Just dir
        , driverConfigGeckodriverFlags = []
        , driverConfigGeckodriverExtraEnv = Nothing
    }]


tryFindSeleniumJar :: FilePath -> IO FilePath
tryFindSeleniumJar path = (T.unpack . T.strip . T.pack) <$> readCreateProcess (proc "find" [path, "-name", "*.jar"]) ""
