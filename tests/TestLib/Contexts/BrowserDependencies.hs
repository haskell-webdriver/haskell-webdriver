{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TestLib.Contexts.BrowserDependencies where

import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Test.Sandwich hiding (BrowserToUse(..))
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import TestLib.Types.Cli


data BrowserDependencies = BrowserDependenciesChrome {
  browserDependenciesChromeChrome :: FilePath
  , browserDependenciesChromeChromedriver :: FilePath
  }
  | BrowserDependenciesFirefox {
      browserDependenciesFirefoxFirefox :: FilePath
      , browserDependenciesFirefoxGeckodriver :: FilePath
      }
  deriving (Show)

browserDependencies :: Label "browserDependencies" BrowserDependencies
browserDependencies = Label

type HasBrowserDependencies context = HasLabel context "browserDependencies" BrowserDependencies


introduceBrowserDependencies :: forall m context. (
  MonadUnliftIO m, HasBaseContext context, HasNixContext context, HasCommandLineOptions context UserOptions
  ) => SpecFree (LabelValue "browserDependencies" BrowserDependencies :> context) m () -> SpecFree context m ()
introduceBrowserDependencies = introduce "Introduce browser dependencies" browserDependencies alloc (const $ return ())
  where
    alloc = do
      UserOptions {..} <- getUserCommandLineOptions

      deps <- case optBrowserToUse of
        UseChrome ->
          BrowserDependenciesChrome <$> getBinaryViaNixPackage @"google-chrome-stable" "google-chrome"
                                    <*> getBinaryViaNixPackage @"chromedriver" "chromedriver"
        UseFirefox ->
          BrowserDependenciesFirefox <$> getBinaryViaNixPackage @"firefox" "firefox"
                                     <*> getBinaryViaNixPackage @"geckodriver" "geckodriver"
      debug [i|Got browser dependencies: #{deps}|]
      return deps