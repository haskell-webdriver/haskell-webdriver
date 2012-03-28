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

import Prelude hiding (catch)



runWD :: WDSession -> WD a -> IO (Either WDError a)
runWD = (runErrorT .) . tryWD

maybeWD :: WDSession -> WD a -> IO (Maybe a)
maybeWD = (fmap eitherToMaybe .) . runWD
  where eitherToMaybe = either (const Nothing) Just

tryWD :: WDSession -> WD a -> ErrorT WDError IO a
tryWD sess (WD wd) = evalStateT wd sess

withSession :: WDSession -> WD a -> WD a
withSession s' (WD wd) =
  WD $ do
    s <- get
    withStateT (const s') wd 
      `catchError` (\err -> do put s
                               throwError err
                   )
      `catch` ( \(SomeException err) -> do put s
                                           throwIO err
              )  
      <* put s

finallyClose :: WD a -> WD a
finallyClose wd = wd
                  `catchError` (\ e -> do closeSession
                                          throwError e
                               )
                  `catch`      (\(SomeException e) -> do closeSession
                                                         throwIO e
                               )

runSession :: WDSession -> Capabilities -> WD a -> IO (Either WDError a)
runSession = ((runErrorT .) .) . trySession

trySession :: WDSession -> Capabilities ->  WD a -> ErrorT WDError IO a
trySession s caps wd = tryWD s $ createSession caps >> wd <* closeSession
                            `catchError` handler
  where handler = const closeSession
