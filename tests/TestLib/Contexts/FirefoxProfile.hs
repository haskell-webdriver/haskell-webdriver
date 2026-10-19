{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TestLib.Contexts.FirefoxProfile (
  introduceFirefoxProfile
  ) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath ((</>))
import Test.Sandwich
import Test.Sandwich.Contexts.Nix
import TestLib.Types


introduceFirefoxProfile :: (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m
  )
  => SpecFree (LabelValue "firefoxProfile" FirefoxProfileContext :> context) m ()
  -> SpecFree context m ()
introduceFirefoxProfile = introduce "Introduce Firefox profile" firefoxProfile alloc (const $ return ())
  where
    alloc = do
      out <- buildNixCallPackageDerivation firefoxProfileDerivation
      version <- T.strip <$> liftIO (T.readFile (out </> "firefox-version"))
      return $ FirefoxProfileContext {
        firefoxProfileContextDir = out </> "profile"
        , firefoxProfileContextVersion = version
        }

firefoxProfileDerivation :: Text
firefoxProfileDerivation = [i|
{ pkgs }:

let
  /* Differ from Firefox's own defaults, so Firefox persists them to prefs.js on shutdown. */
  seedUserJs = ''
    user_pref("browser.startup.page", 3);
    user_pref("network.trr.mode", 5);
  '';

  /* Written after Firefox exits, so these only live in user.js, which it reads after prefs.js. */
  userJs = ''
    // Overrides Firefox itself never wrote.
    user_pref("network.trr.mode", 2);

    \# Firefox's preferences parser also accepts shell-style line comments...
    user_pref("ui.systemUsesDarkTheme", 1);

    /* ...and C-style block comments. */
    user_pref("signon.rememberSignons", true);
  '';

  userChromeCss = ''
    @namespace url("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul");
    \#TabsToolbar { visibility: collapse !important; }
  '';
in

pkgs.runCommand "firefox-profile" { nativeBuildInputs = [ pkgs.firefox ]; } ''
  export HOME=$(mktemp -d)
  profile=$HOME/profile
  mkdir -p "$profile/chrome"

  install -m 644 ${pkgs.writeText "userChrome.css" userChromeCss} "$profile/chrome/userChrome.css"
  install -m 644 ${pkgs.writeText "user.js" seedUserJs} "$profile/user.js"

  firefox --headless --profile "$profile" --screenshot "$HOME/screenshot.png" about:blank

  install -m 644 ${pkgs.writeText "user.js" userJs} "$profile/user.js"

  mkdir -p $out
  cp -r "$profile" $out/profile
  firefox --version | sed 's/^Mozilla Firefox //' > $out/firefox-version
''
|]
