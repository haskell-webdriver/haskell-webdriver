#!/bin/bash
# Temporary diagnostic: does safaridriver answer Fullscreen Window on this machine, and from which
# window states? Usage: safari-fullscreen-probe.sh [path to safaridriver]
set -u
DRIVER="${1:-/usr/bin/safaridriver}"
OUT="$PWD/probe-out"
mkdir -p "$OUT"
exec > >(tee -a "$OUT/probe.txt") 2>&1

case "$DRIVER" in
  *"Safari Technology Preview.app"*)
    APP="/Applications/Safari Technology Preview.app"; BROWSER="Safari Technology Preview"; PROC="Safari Technology Preview" ;;
  *)
    APP="/Applications/Safari.app"; BROWSER="safari"; PROC="Safari" ;;
esac

now() { perl -MTime::HiRes=time -e 'printf "%.3f", time'; }
stamp() { perl -MTime::HiRes=time -MPOSIX=strftime -e '$t=time; printf "%s.%03d", strftime("%H:%M:%S", localtime $t), ($t - int $t) * 1000'; }

echo "== environment"
sw_vers
uname -m
echo "browser: $APP $(defaults read "$APP/Contents/Info" CFBundleShortVersionString) ($(defaults read "$APP/Contents/Info" CFBundleVersion))"
"$DRIVER" --version || true
system_profiler SPDisplaysDataType
xcrun swift -e 'import CoreGraphics
if let d = CGSessionCopyCurrentDictionary() as? [String: Any] {
  for (k, v) in d.sorted(by: { $0.key < $1.key }) { print("session \(k) = \(v)") }
} else { print("no CoreGraphics session") }' || true
echo "-- com.apple.spaces"; defaults read com.apple.spaces 2>&1 | head -40
echo "-- com.apple.dock"; defaults read com.apple.dock 2>&1 | grep -i -E 'mru-spaces|expose|workspaces|autohide|launchanim'
echo "-- pmset"; pmset -g 2>&1 | head -20

echo "== enable automation"
sudo "$DRIVER" --enable

PORT=""
DPID=""
start_driver() {
  PORT=$(python3 -c 'import socket; s = socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1]); s.close()')
  "$DRIVER" --port "$PORT" >> "$OUT/safaridriver.log" 2>&1 &
  DPID=$!
  for _ in $(seq 100); do
    curl -s -o /dev/null "http://127.0.0.1:$PORT/status" && return 0
    sleep 0.1
  done
  echo "safaridriver did not come up"
  return 1
}

stop_all() {
  kill "$DPID" 2>/dev/null
  wait "$DPID" 2>/dev/null
  pkill -x "$PROC" 2>/dev/null
  sleep 3
}

# req LABEL METHOD PATH BODY MAX_SECONDS
req() {
  local t0 t1 out rc
  t0=$(now)
  if [ -n "$4" ]; then
    out=$(curl -s -m "$5" -X "$2" -H 'Content-Type: application/json' -d "$4" "http://127.0.0.1:$PORT$3" -w ' [http %{http_code}]')
  else
    out=$(curl -s -m "$5" -X "$2" "http://127.0.0.1:$PORT$3" -w ' [http %{http_code}]')
  fi
  rc=$?
  t1=$(now)
  printf '%s %-20s curl rc=%-2s %6.2fs %s\n' "$(stamp)" "$1" "$rc" "$(perl -e "print $t1 - $t0")" "$(echo "$out" | tr -d '\n' | cut -c1-200)"
}

setup_normal() { req "set rect 600x500" POST "/session/$SESSION/window/rect" '{"x":50,"y":50,"width":600,"height":500}' 30; }
setup_maximized() { req "maximize" POST "/session/$SESSION/window/maximize" '{}' 30; }
setup_minimized() {
  req "maximize" POST "/session/$SESSION/window/maximize" '{}' 30
  req "minimize" POST "/session/$SESSION/window/minimize" '{}' 30
}

# scenario NAME SETUP
scenario() {
  local name="$1" setup="$2" since shots
  echo "== scenario: fullscreen from $name window"
  since=$(date '+%Y-%m-%d %H:%M:%S')
  start_driver || return
  SESSION=$(curl -s -m 60 -X POST -H 'Content-Type: application/json' \
    -d "{\"capabilities\":{\"alwaysMatch\":{\"browserName\":\"$BROWSER\"}}}" \
    "http://127.0.0.1:$PORT/session" | python3 -c 'import json, sys; print(json.load(sys.stdin)["value"]["sessionId"])')
  if [ -z "$SESSION" ]; then
    echo "no session"
    stop_all
    return
  fi
  echo "session $SESSION"
  req "navigate" POST "/session/$SESSION/url" '{"url":"about:blank"}' 30
  "$setup"
  (
    sleep 2; screencapture -x "$OUT/$name-at-2s.png" 2>&1 | head -2
    sleep 18; screencapture -x "$OUT/$name-at-20s.png" 2>&1 | head -2
  ) &
  shots=$!
  req "fullscreen" POST "/session/$SESSION/window/fullscreen" '{}' 60
  wait "$shots"
  req "getWindowRect" GET "/session/$SESSION/window/rect" '' 10
  req "DELETE session" DELETE "/session/$SESSION" '' 20
  stop_all
  log show --start "$since" --style compact \
    --predicate "process == \"$PROC\" OR process == \"safaridriver\" OR process == \"WindowServer\" OR process == \"Dock\" OR process == \"com.apple.WebKit.WebContent\"" \
    > "$OUT/log-$name.txt" 2>&1
  grep -i -E 'full ?screen|space|transition' "$OUT/log-$name.txt" | head -300 > "$OUT/log-$name-fullscreen.txt"
}

scenario normal setup_normal
scenario maximized setup_maximized
scenario minimized setup_minimized
echo "== done"
