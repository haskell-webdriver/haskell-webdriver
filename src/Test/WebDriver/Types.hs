{-# OPTIONS_HADDOCK not-home #-}
module Test.WebDriver.Types
       ( -- * WebDriver sessions
         WD(..), WDSession(..), SessionId(..), SessionHistory
         -- * WebDriver configuration
       , WDConfig(..), defaultConfig, SessionHistoryConfig
         -- * Capabilities
       , Capabilities(..), defaultCaps, allCaps
       , Platform(..), ProxyType(..), UnexpectedAlertBehavior(..)
         -- ** Browser-specific capabilities
       , Browser(..),
         -- ** Default settings for browsers
         firefox, chrome, ie, opera, iPhone, iPad, android
       , LogLevel(..), IELogLevel(..), IEElementScrollBehavior(..)
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
