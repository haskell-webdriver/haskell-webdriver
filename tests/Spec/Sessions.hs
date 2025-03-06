
module Spec.Sessions where

import Data.String.Interpolate
import Test.Sandwich
import Test.WebDriver.Commands
import TestLib.Contexts.Session
import TestLib.Contexts.StaticServer
import TestLib.Types


tests :: SessionSpec
tests = introduceSession $ describe "Sessions" $ before "Open test page" openSimpleTestPage $ do
  -- createSession is already tested in introduceSession

  it "status" $ do
    status <- serverStatus
    info [i|Got status: #{status}|]

  it "closeSession" $ do
    closeSession

  -- TODO: test closeSession

  -- it "sessions" $ do
  --   xs <- sessions
  --   info [i|Got sessions: #{xs}|]

  -- it "getActualCaps" $ do
  --   caps <- getActualCaps
  --   info [i|Got actual caps: #{caps}|]
