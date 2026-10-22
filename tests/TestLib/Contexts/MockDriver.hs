module TestLib.Contexts.MockDriver (
  mockDriverDerivation
  ) where

import Data.String.Interpolate
import Data.Text (Text)


-- | A driver that can't bind its port on its first start, and fails for another reason after
-- that. Each start appends its arguments to the file named by @MOCK_DRIVER_STARTS@.
mockDriverDerivation :: Text
mockDriverDerivation = [i|
{ pkgs }:

pkgs.writeShellScriptBin "mock-driver" ''
  if [ -e "$MOCK_DRIVER_STARTS" ]; then
    echo "$@" >> "$MOCK_DRIVER_STARTS"
    echo "mock-driver: failed for another reason"
    exit 2
  fi
  echo "$@" > "$MOCK_DRIVER_STARTS"
  echo "Unable to start the server: Address already in use"
  exit 1
''
|]
