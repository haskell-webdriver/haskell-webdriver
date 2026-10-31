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
import TestLib.Types
import TestLib.Types.Cli
import UnliftIO.Directory (findExecutable)


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
                                    <*> pure optChromeNoSandbox
        UseFirefox ->
          BrowserDependenciesFirefox <$> getBinaryViaNixPackage @"firefox" "firefox"
                                     <*> getBinaryViaNixPackage @"geckodriver" "geckodriver"
        UseSafari ->
          BrowserDependenciesSafari <$> findSafaridriver optSafariDriver
      debug [i|Got browser dependencies: #{deps}|]
      return deps

    findSafaridriver (Just path) = return path
    findSafaridriver Nothing = findExecutable "safaridriver" >>= \case
      Just path -> return path
      Nothing -> expectationFailure "Couldn't find safaridriver on the PATH. It ships with Safari on macOS."
