#!/bin/bash
# Temporary diagnostic, now running the safaridriver launch stress test in
# safari-launch-stress.py (the workflow still calls this script by its old name).
# Usage: safari-fullscreen-probe.sh [path to safaridriver]
set -u
DRIVER="${1:-/usr/bin/safaridriver}"
OUT="$PWD/probe-out"
mkdir -p "$OUT"
exec > >(tee -a "$OUT/setup.txt") 2>&1

case "$DRIVER" in
  *"Safari Technology Preview.app"*) BROWSER="Safari Technology Preview" ;;
  *) BROWSER=safari ;;
esac

sw_vers
"$DRIVER" --version
sudo "$DRIVER" --enable

JAVA=""
for v in JAVA_HOME_21_arm64 JAVA_HOME_21_X64 JAVA_HOME_17_arm64 JAVA_HOME_17_X64; do
  d="${!v:-}"
  if [ -n "$d" ] && [ -x "$d/bin/java" ]; then JAVA="$d/bin/java"; break; fi
done
if [ -z "$JAVA" ]; then
  mkdir -p jdk
  curl -sSLf "https://api.adoptium.net/v3/binary/latest/21/ga/mac/aarch64/jdk/hotspot/normal/eclipse" | tar -xz -C jdk
  JAVA="$(find "$PWD/jdk" -path '*/Contents/Home/bin/java' | head -1)"
fi
"$JAVA" -version

mkdir -p jars
curl -sSLf -o jars/selenium-3.jar https://github.com/SeleniumHQ/selenium/releases/download/selenium-3.141.59/selenium-server-standalone-3.141.59.jar
curl -sSLf -o jars/selenium-4.29.jar https://github.com/SeleniumHQ/selenium/releases/download/selenium-4.29.0/selenium-server-4.29.0.jar
curl -sSLf -o jars/selenium-4.51.jar https://github.com/SeleniumHQ/selenium/releases/download/selenium-4.51.0/selenium-server-4.51.0.jar
ls -la jars

# The job times out at 25 minutes, so stop starting sessions after 16.
python3 .github/diagnostics/safari-launch-stress.py "$OUT" \
  --rounds 40 --per-round 4 --max-minutes 16 --modes bare-low,bare-eph,sel3,sel4,sel4new \
  --safaridriver "$DRIVER" --browser "$BROWSER" --java "$JAVA" \
  --sel3-jar jars/selenium-3.jar --sel4-jar jars/selenium-4.29.jar --sel4new-jar jars/selenium-4.51.jar \
  --selenium-timeout 75
