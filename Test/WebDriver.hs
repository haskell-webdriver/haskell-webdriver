module Test.WebDriver
       ( module Test.WebDriver.Types
       , module Test.WebDriver.Commands
       , module Test.WebDriver.Commands.Wait
       , module Test.WebDriver
       ) where

import Test.WebDriver.Types
import Test.WebDriver.Commands
import Test.WebDriver.Commands.Wait

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Control.Exception.Lifted
import qualified Control.Exception as IO

import Prelude hiding (catch)

runWD :: WDSession -> WD a -> IO a
runWD sess (WD wd) = evalStateT wd sess

runSession :: WDSession -> Capabilities -> WD a -> IO a
runSession s caps wd = runWD s $ createSession caps >> wd  <* closeSession

withSession :: WDSession -> WD a -> WD a
withSession s' (WD wd) = WD . lift $ evalStateT wd s'

closeOnException :: WD a -> WD a
closeOnException wd = wd `onException` closeSession

finallyClose:: WD a -> WD a 
finallyClose wd = closeOnException wd <* closeSession
