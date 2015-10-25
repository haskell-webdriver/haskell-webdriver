{-# OPTIONS_HADDOCK not-home #-}
module Test.WebDriver.Types
       ( -- * WebDriver sessions
         WD(..), WDSession(..), SessionId(..), SessionHistory
         -- * WebDriver configuration
       , WDConfig'(..), defaultConfig, SessionHistoryConfig, WebDriverConfig(..), WebDriverConfigConstraint 
         -- * Capabilities
       , Capabilities, CapabilityName(..), W3C, LegacyWireProtocol, CapabilityField(..), CapabilityKey, Capability(..), CapabilityFamily
       , CapsAll, FieldsAll, KeysHaveText, CapsAreParseable
         -- ** Browser-specific capabilities
       , BrowserType(..),
         -- ** Default settings for browsers
         firefox, chrome, ie, opera, iPhone, iPad, android
         -- ** Other browser capability types
       , PlatformType(..), ProxyType(..), UnexpectedAlertBehavior(..), LogLevel(..), IELogLevel(..), IEElementScrollBehavior(..)
         -- * WebDriver objects and command-specific types
       , Element(..)
       , WindowHandle(..), currentWindow
       , Selector(..)
       , JSArg(..)
       , FrameSelector(..)
       , Cookie(..), mkCookie
       , Orientation(..)
       , MouseButton(..)
       , WebStorageType(..)
       , LogType, LogEntry(..)
       , ApplicationCacheStatus(..)
         -- * Exceptions
       , InvalidURL(..), NoSessionId(..), BadJSON(..)
       , HTTPStatusUnknown(..), HTTPConnError(..)
       , UnknownCommand(..), ServerError(..)
       , FailedCommand(..), FailedCommandType(..)
       , FailedCommandInfo(..), StackFrame(..)
       , mkFailedCommandInfo, failedCommand
       ) where

import Test.WebDriver.Monad
import Test.WebDriver.Session
import Test.WebDriver.Config
import Test.WebDriver.Commands
import Test.WebDriver.Exceptions
import Test.WebDriver.Capabilities
