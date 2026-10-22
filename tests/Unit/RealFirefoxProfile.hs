module Unit.RealFirefoxProfile (
  tests
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath ((</>))
import System.Info (os)
import Test.Sandwich
import Test.Sandwich.Contexts.Nix
import Test.WebDriver.Profile
import TestLib.Contexts.FirefoxProfile
import TestLib.Types
import UnliftIO.Temporary (withSystemTempDirectory)


-- On macOS, Firefox can't run inside a Nix build, and Firefox 59 is Linux-only.
tests :: NixSpec
tests = when (os == "linux") $ describe "Profiles written by a real Firefox" $ do
  describe "Firefox 151.0.2" $ introduceNixContext nixpkgsFirefox151 $ introduceFirefoxProfile $ do
    realProfileTests "151.0.2"

    it "reads the prefs.js format Firefox 60 and later write" $ do
      preamble <- firstLineOfPrefsJs

      preamble `shouldBe` "// Mozilla User Preferences"

  describe "Firefox 59.0.2" $ introduceNixContext nixpkgsFirefox59 $ introduceFirefoxProfile $ do
    realProfileTests "59.0.2"

    it "reads the prefs.js format Firefox 59 and earlier wrote" $ do
      preamble <- firstLineOfPrefsJs

      preamble `shouldBe` "# Mozilla User Preferences"

realProfileTests :: HasFirefoxProfile context => Text -> SpecFree context IO ()
realProfileTests expectedVersion = do
  it [i|was written by Firefox #{expectedVersion}|] $ do
    FirefoxProfileContext {firefoxProfileContextVersion} <- getContext firefoxProfile
    firefoxProfileContextVersion `shouldBe` expectedVersion

  it "reads the preferences Firefox wrote to prefs.js" $ do
    FirefoxProfileContext {firefoxProfileContextVersion} <- getContext firefoxProfile
    Profile {profilePrefs} <- loadProfile
    HM.lookup "extensions.lastAppVersion" profilePrefs `shouldBe` Just (PrefString firefoxProfileContextVersion)

  it "layers preferences from defaults, prefs.js and user.js" $ do
    Profile {profilePrefs} <- loadProfile
    -- Only in defaultFirefoxProfile
    HM.lookup "app.update.auto" profilePrefs `shouldBe` Just (PrefBool False)
    -- prefs.js overrides the default of 0
    HM.lookup "browser.startup.page" profilePrefs `shouldBe` Just (PrefInteger 3)
    -- user.js overrides the 5 that Firefox persisted to prefs.js
    HM.lookup "network.trr.mode" profilePrefs `shouldBe` Just (PrefInteger 2)
    -- user.js overrides the default of False
    HM.lookup "signon.rememberSignons" profilePrefs `shouldBe` Just (PrefBool True)

  it "stores files relative to the profile root, skipping lock and preference files" $ do
    Profile {profileFiles} <- loadProfile
    let paths = HM.keys profileFiles

    forM_ ["chrome" </> "userChrome.css", "compatibility.ini"] $ \path ->
      paths `shouldContain` [path]
    forM_ ["prefs.js", "user.js", ".parentlock", "lock"] $ \path ->
      paths `shouldNotContain` [path]

  it "round-trips through the capabilities JSON encoding" $ do
    profile <- loadProfile
    A.fromJSON (A.toJSON profile) `shouldBe` A.Success profile

  it "round-trips through saveFirefoxProfile" $ do
    profile <- loadProfile
    withSystemTempDirectory "webdriver-profile" $ \dir -> do
      saveFirefoxProfile profile dir
      loadFirefoxProfile dir >>= (`shouldBe` profile)

loadProfile :: (
  MonadIO m, MonadReader context m, HasFirefoxProfile context
  ) => m (Profile Firefox)
loadProfile = getContext firefoxProfile >>= loadFirefoxProfile . firefoxProfileContextDir

firstLineOfPrefsJs :: (
  MonadIO m, MonadReader context m, HasFirefoxProfile context
  ) => m Text
firstLineOfPrefsJs = do
  FirefoxProfileContext {firefoxProfileContextDir} <- getContext firefoxProfile
  T.takeWhile (/= '\n') <$> liftIO (T.readFile (firefoxProfileContextDir </> "prefs.js"))

-- | Release @26.05@ has Firefox 151.0.2.
nixpkgsFirefox151 :: NixpkgsDerivation
nixpkgsFirefox151 = NixpkgsDerivationFetchFromGitHub {
  nixpkgsDerivationOwner = "NixOS"
  , nixpkgsDerivationRepo = "nixpkgs"
  , nixpkgsDerivationRev = "8c50a710ddca43d7a530fb805ad55bde8d0141c5"
  , nixpkgsDerivationSha256 = "sha256-md0zn0RnwNvPyASas1yG5YUuwQ4ALA6ucL50l0DvqCo="
  , nixpkgsDerivationAllowUnfree = False
  }

-- | Release @18.03@ has Firefox 59.0.2, the last version to write prefs.js in
-- the pre-60 format.
nixpkgsFirefox59 :: NixpkgsDerivation
nixpkgsFirefox59 = NixpkgsDerivationFetchFromGitHub {
  nixpkgsDerivationOwner = "NixOS"
  , nixpkgsDerivationRepo = "nixpkgs"
  , nixpkgsDerivationRev = "120b013e0c082d58a5712cde0a7371ae8b25a601"
  , nixpkgsDerivationSha256 = "sha256-jtxfSXmc07z8U3l0+TuUCSHcd8BgkapvUzjUN7fwZEI="
  , nixpkgsDerivationAllowUnfree = False
  }
