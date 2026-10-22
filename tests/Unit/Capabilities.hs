module Unit.Capabilities (
  tests
  ) where

import qualified Data.Aeson as A
import Test.Sandwich
import Test.WebDriver.Capabilities


tests :: SpecFree context IO ()
tests = describe "Capabilities" $ do
  it "encodes SafariOptions as top-level vendor-prefixed capabilities" $
    A.toJSON safariCaps `shouldBe` A.object [
      ("browserName", "safari")
      , ("safari:automaticInspection", A.Bool True)
      , ("safari:automaticProfiling", A.Bool True)
      , ("safari:useSimulator", A.Bool True)
      , ("safari:deviceType", "iPad")
      , ("safari:deviceName", "iPad Pro")
      , ("safari:deviceUDID", "00008030-000A4D0A3E80802E")
      , ("safari:platformVersion", "17")
      , ("safari:platformBuildVersion", "21A329")
      , ("webkit:WebRTC", A.object [
            ("DisableInsecureMediaCapture", A.Bool True)
            , ("DisableICECandidateFiltering", A.Bool False)
            ])
      ]

  it "decodes Safari capabilities" $
    A.fromJSON (A.toJSON safariCaps) `shouldBe` A.Success safariCaps

  it "sends nothing for empty SafariOptions, which decode as Nothing" $ do
    let caps = defaultCaps { _capabilitiesSafariOptions = Just emptySafariOptions }
    A.toJSON caps `shouldBe` A.object []
    A.fromJSON (A.toJSON caps) `shouldBe` A.Success defaultCaps

  it "decodes the capabilities safaridriver returns" $
    fmap _capabilitiesSafariOptions (A.fromJSON safaridriverCaps) `shouldBe` A.Success (Just emptySafariOptions {
      _safariOptionsAutomaticInspection = Just False
      , _safariOptionsAutomaticProfiling = Just False
      , _safariOptionsUseSimulator = Just False
      , _safariOptionsPlatformVersion = Just "26.7.2"
      , _safariOptionsPlatformBuildVersion = Just "25G320"
      , _safariOptionsWebRTC = Just (WebKitWebRTC (Just False) (Just False))
      })

  where
    safariCaps = defaultCaps {
      _capabilitiesBrowserName = Just "safari"
      , _capabilitiesSafariOptions = Just SafariOptions {
          _safariOptionsAutomaticInspection = Just True
          , _safariOptionsAutomaticProfiling = Just True
          , _safariOptionsUseSimulator = Just True
          , _safariOptionsDeviceType = Just "iPad"
          , _safariOptionsDeviceName = Just "iPad Pro"
          , _safariOptionsDeviceUDID = Just "00008030-000A4D0A3E80802E"
          , _safariOptionsPlatformVersion = Just "17"
          , _safariOptionsPlatformBuildVersion = Just "21A329"
          , _safariOptionsWebRTC = Just (WebKitWebRTC (Just True) (Just False))
          }
      }

    -- What safaridriver 27.2 returned for a new session, including keys SafariOptions doesn't model.
    safaridriverCaps = A.object [
      ("acceptInsecureCerts", A.Bool False)
      , ("browserName", "Safari")
      , ("browserVersion", "27.2")
      , ("platformName", "macOS")
      , ("safari:automaticInspection", A.Bool False)
      , ("safari:automaticProfiling", A.Bool False)
      , ("safari:diagnose", A.Bool False)
      , ("safari:experimentalWebSocketUrl", A.Bool False)
      , ("safari:platformBuildVersion", "25G320")
      , ("safari:platformVersion", "26.7.2")
      , ("safari:useSimulator", A.Bool False)
      , ("setWindowRect", A.Bool True)
      , ("strictFileInteractability", A.Bool False)
      , ("webkit:WebRTC", A.object [
            ("DisableICECandidateFiltering", A.Bool False)
            , ("DisableInsecureMediaCapture", A.Bool False)
            ])
      , ("webkit:alwaysAllowAutoplay", A.Bool False)
      ]
