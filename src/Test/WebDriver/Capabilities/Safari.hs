{-# LANGUAGE TemplateHaskell #-}

module Test.WebDriver.Capabilities.Safari where

import Data.Aeson as A
import Data.Aeson.TH
import Lens.Micro.TH
import Test.WebDriver.Capabilities.Aeson


-- | Temporary changes to Safari's WebRTC and media capture policies, passed as
-- the @webkit:WebRTC@ capability. See the safaridriver(1) man page.
data WebKitWebRTC = WebKitWebRTC {
  -- | Allow media capture over insecure connections, such as a local HTTP test
  -- server.
  _webKitWebRTCDisableInsecureMediaCapture :: Maybe Bool
  -- | Send ICE candidates for internal network addresses instead of filtering
  -- them out.
  , _webKitWebRTCDisableICECandidateFiltering :: Maybe Bool
  } deriving (Show, Eq)
-- Unlike every other capability, the keys are capitalised: DisableInsecureMediaCapture.
deriveJSON (baseOptions { A.fieldLabelModifier = dropPrefix "_webKitWebRTC" }) ''WebKitWebRTC
makeLenses ''WebKitWebRTC

-- | Safari has no options object, so these settings are sent as the top-level
-- @safari:@ and @webkit:@ capabilities described in the safaridriver(1) man
-- page and at
-- <https://developer.apple.com/documentation/webkit/about-webdriver-for-safari>.
data SafariOptions = SafariOptions {
  -- | Preload the Web Inspector and JavaScript debugger in the background. A
  -- @debugger;@ statement in the test page then pauses execution and opens the
  -- Debugger tab.
  --
  -- safaridriver has treated the key's presence as true, so leave this
  -- 'Nothing' rather than setting it to @Just False@.
  _safariOptionsAutomaticInspection :: Maybe Bool
  -- | Preload the Web Inspector and start a timeline recording in the
  -- background, to view from the Develop menu. Presence counts as true, as for
  -- '_safariOptionsAutomaticInspection'.
  , _safariOptionsAutomaticProfiling :: Maybe Bool
  -- | Only create the session on an iOS Simulator host ('True') or never on one
  -- ('False'). Simulator hosts need an Xcode installation.
  , _safariOptionsUseSimulator :: Maybe Bool
  -- | Only create the session on an @"iPhone"@ or @"iPad"@ device or
  -- simulator. Compared case-insensitively.
  , _safariOptionsDeviceType :: Maybe String
  -- | Only create the session on a host whose device name matches. Compared
  -- case-insensitively.
  , _safariOptionsDeviceName :: Maybe String
  -- | Only create the session on a host whose device UDID matches. Compared
  -- case-insensitively.
  , _safariOptionsDeviceUDID :: Maybe String
  -- | Only create the session on a host whose OS version starts with this:
  -- @"12"@ matches 12.0 and 12.1 but not 10.12.
  , _safariOptionsPlatformVersion :: Maybe String
  -- | Only create the session on a host whose OS build version matches, for
  -- example @"18E193"@ as shown by @sw_vers@.
  , _safariOptionsPlatformBuildVersion :: Maybe String
  -- | Temporary changes to Safari's WebRTC and media capture policies.
  , _safariOptionsWebRTC :: Maybe WebKitWebRTC
  } deriving (Show, Eq)
deriveJSON safariOptionsOptions ''SafariOptions
makeLenses ''SafariOptions

-- | No settings. These send no capabilities at all, so they read back as
-- 'Nothing'.
emptySafariOptions :: SafariOptions
emptySafariOptions = SafariOptions {
  _safariOptionsAutomaticInspection = Nothing
  , _safariOptionsAutomaticProfiling = Nothing
  , _safariOptionsUseSimulator = Nothing
  , _safariOptionsDeviceType = Nothing
  , _safariOptionsDeviceName = Nothing
  , _safariOptionsDeviceUDID = Nothing
  , _safariOptionsPlatformVersion = Nothing
  , _safariOptionsPlatformBuildVersion = Nothing
  , _safariOptionsWebRTC = Nothing
  }

-- | Same as 'emptySafariOptions'.
defaultSafariOptions :: SafariOptions
defaultSafariOptions = emptySafariOptions
