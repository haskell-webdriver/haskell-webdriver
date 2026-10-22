module Unit.FirefoxProfileBasics (
  tests
  ) where

import Control.Monad
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Test.Sandwich
import Test.WebDriver.Profile
import UnliftIO.Exception (IOException)


tests :: SpecFree context IO ()
tests = describe "Firefox profile basics" $ do
  describe "parseFirefoxPrefs" $ do
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
    it "fails when the directory does not exist" $
      loadFirefoxProfile "/nonexistent/firefox-profile" `shouldThrow` (\(_ :: IOException) -> True)

    it "round-trips preference strings with tabs, quotes and newlines" $ do
      let profile = addPref "s" ("a\tb \"c\" \\d\n" :: Text) defaultFirefoxProfile
      A.fromJSON (A.toJSON profile) `shouldBe` A.Success profile

  where
    parse = parseFirefoxPrefs "prefs.js"
