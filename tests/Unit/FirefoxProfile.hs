
module Unit.FirefoxProfile where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import System.FilePath ((</>))
import Test.Sandwich
import Test.WebDriver.Profile
import UnliftIO.Exception (IOException, throwIO)
import UnliftIO.Temporary (withSystemTempDirectory)


tests :: TopSpec
tests = describe "Firefox profiles" $ do
  describe "parseFirefoxPrefs" $ do
    it "parses a prefs.js written by Firefox 60 or later" $ do
      prefs <- parseFixture ("firefox-profile" </> "prefs.js")
      HM.size prefs `shouldBe` 18
      HM.lookup "app.normandy.first_run" prefs `shouldBe` Just (PrefBool False)
      HM.lookup "browser.laterrun.bookkeeping.sessionCount" prefs `shouldBe` Just (PrefInteger 3)
      HM.lookup "browser.startup.homepage_override.mstone" prefs `shouldBe` Just (PrefString "131.0")
      HM.lookup "privacy.sanitize.pending" prefs `shouldBe`
        Just (PrefString "[{\"id\":\"newtab-container\",\"itemsToClear\":[],\"options\":{}}]")

    it "parses a prefs.js written by Firefox 59 or earlier" $ do
      prefs <- parseFixture "prefs-firefox-59.js"
      HM.size prefs `shouldBe` 9
      HM.lookup "browser.cache.disk.capacity" prefs `shouldBe` Just (PrefInteger 358400)
      HM.lookup "extensions.lastAppVersion" prefs `shouldBe` Just (PrefString "59.0.2")
      HM.lookup "browser.bookmarks.restore_default_bookmarks" prefs `shouldBe` Just (PrefBool False)

    it "accepts empty and comment-only files" $
      forM_ ["", "\n\n", "// blah", "# blah", "/* blah */", "// one\n# two\n/* three */\n"] $ \input ->
        parse input `shouldBe` Right HM.empty

    it "accepts comments without a trailing newline" $ do
      parse "user_pref(\"a\", 1); // trailing" `shouldBe` Right (HM.singleton "a" (PrefInteger 1))
      parse "user_pref(\"a\", 1);\n//" `shouldBe` Right (HM.singleton "a" (PrefInteger 1))

    it "allows whitespace and comments between tokens" $
      parse "user_pref /* a */ ( \"a\" , // b\n 1 ) ;" `shouldBe` Right (HM.singleton "a" (PrefInteger 1))

    it "accepts single-quoted strings, Firefox's escapes and signed integers" $ do
      parse "user_pref('a', 'it\\'s \\\"q\\\" \\x41\\u00e9\\ud83d\\ude00');"
        `shouldBe` Right (HM.singleton "a" (PrefString "it's \"q\" A\233\128512"))
      parse "user_pref(\"n\", -5);" `shouldBe` Right (HM.singleton "n" (PrefInteger (-5)))

    it "does not treat // inside a string as a comment" $
      parse "user_pref(\"browser.startup.homepage\", \"https://example.com/\");"
        `shouldBe` Right (HM.singleton "browser.startup.homepage" (PrefString "https://example.com/"))

    it "lets later entries override earlier ones" $
      parse "user_pref(\"a\", 1);\nuser_pref(\"a\", 2);\n" `shouldBe` Right (HM.singleton "a" (PrefInteger 2))

    it "reports the file and line of a malformed statement" $
      case parse "user_pref(\"a\", 1);\nuser_pref(\"b\" 2);\nuser_pref(\"c\", 3);\n" of
        Right prefs -> expectationFailure ("Expected a parse error, got " <> show prefs)
        Left (ProfileParseError msg) -> do
          msg `shouldContain` "prefs.js:2:"
          msg `shouldContain` "2);"

    it "rejects values that are not strings, integers or booleans" $
      case parse "user_pref(\"a\", [1, 2]);\n" of
        Right prefs -> expectationFailure ("Expected a parse error, got " <> show prefs)
        Left (ProfileParseError msg) -> do
          msg `shouldContain` "prefs.js:1:"
          msg `shouldContain` "[1, 2]"

  describe "loadFirefoxProfile" $ do
    it "stores files relative to the profile root, skipping lock and preference files" $ do
      Profile {profileFiles} <- loadFixtureProfile
      HM.keys profileFiles `shouldBeSet` ["chrome" </> "userChrome.css", "compatibility.ini", "times.json"]

    it "layers preferences from defaults, prefs.js and user.js" $ do
      Profile {profilePrefs} <- loadFixtureProfile
      -- Only in defaultFirefoxProfile
      HM.lookup "app.update.auto" profilePrefs `shouldBe` Just (PrefBool False)
      -- prefs.js overrides the default of 0
      HM.lookup "browser.startup.page" profilePrefs `shouldBe` Just (PrefInteger 1)
      -- user.js overrides the 5 in prefs.js
      HM.lookup "network.trr.mode" profilePrefs `shouldBe` Just (PrefInteger 2)
      -- user.js overrides the default of False
      HM.lookup "signon.rememberSignons" profilePrefs `shouldBe` Just (PrefBool True)

    it "fails when the directory does not exist" $
      loadFirefoxProfile "/nonexistent/firefox-profile" `shouldThrow` (\(_ :: IOException) -> True)

    it "round-trips through the capabilities JSON encoding" $ do
      profile <- loadFixtureProfile
      A.fromJSON (A.toJSON profile) `shouldBe` A.Success profile

    it "round-trips preference strings with tabs, quotes and newlines" $ do
      let profile = addPref "s" ("a\tb \"c\" \\d\n" :: Text) defaultFirefoxProfile
      A.fromJSON (A.toJSON profile) `shouldBe` A.Success profile

    it "round-trips through saveFirefoxProfile" $ do
      profile <- loadFixtureProfile
      withSystemTempDirectory "webdriver-profile" $ \dir -> do
        saveFirefoxProfile profile dir
        loadFirefoxProfile dir >>= (`shouldBe` profile)

  where
    parse = parseFirefoxPrefs "prefs.js"

    parseFixture name = liftIO (BS.readFile path) >>= either throwIO return . parseFirefoxPrefs path
      where path = fixtures </> name

    loadFixtureProfile = loadFirefoxProfile (fixtures </> "firefox-profile")

    -- cabal and stack both run the test suite from the package directory.
    fixtures = "tests" </> "fixtures"
