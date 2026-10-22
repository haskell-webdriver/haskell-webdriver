module TestLib.Contexts.MockDriver (
  mockDriverDerivation
  , mockSafaridriverDerivation
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

-- | A safaridriver that answers @/status@ and fails its first new-session request in the way named
-- by @MOCK_SAFARIDRIVER_FAILURE@: "exit" exits without answering, and "connect-timeout" returns
-- Safari's connection timeout error. Later requests create a session. Each start appends its
-- arguments to the file named by @MOCK_DRIVER_STARTS@, and each new-session request appends a line
-- to the file named by @MOCK_DRIVER_SESSIONS@.
mockSafaridriverDerivation :: Text
mockSafaridriverDerivation = [i|
{ pkgs }:

pkgs.writeScriptBin "mock-safaridriver" ''
  \#!${pkgs.python3}/bin/python3
  import http.server
  import json
  import os
  import sys

  port = int(sys.argv[sys.argv.index("--port") + 1])
  with open(os.environ["MOCK_DRIVER_STARTS"], "a") as f:
      f.write(" ".join(sys.argv[1:]) + "\\n")
  sessions = os.environ["MOCK_DRIVER_SESSIONS"]

  class Handler(http.server.BaseHTTPRequestHandler):
      def reply(self, code, value):
          body = json.dumps({"value": value}).encode()
          self.send_response(code)
          self.send_header("Content-Type", "application/json")
          self.send_header("Content-Length", str(len(body)))
          self.end_headers()
          self.wfile.write(body)

      def do_GET(self):
          self.reply(200, {"ready": True, "message": ""})

      def do_POST(self):
          self.rfile.read(int(self.headers["Content-Length"]))
          first = not os.path.exists(sessions)
          with open(sessions, "a") as f:
              f.write("POST /session\\n")
          if first and os.environ["MOCK_SAFARIDRIVER_FAILURE"] == "exit":
              os._exit(1)
          if first:
              self.reply(500, {"error": "session not created", "message": "Could not create a session: The session timed out while connecting to a Safari instance."})
          else:
              self.reply(200, {"sessionId": "mock-session", "capabilities": {}})

      def do_DELETE(self):
          self.reply(200, None)

      def log_message(self, *args):
          pass

  http.server.HTTPServer(("127.0.0.1", port), Handler).serve_forever()
''
|]
