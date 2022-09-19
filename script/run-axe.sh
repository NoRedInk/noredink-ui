#!/usr/bin/env bash
set -euo pipefail

# start a web server in the background and tear it down when exiting
./script/serve.sh public &
SERVER_PID=$!
cleanup() {
    kill "$SERVER_PID"
}
trap cleanup EXIT INT

# Wait for the python server to launch since we're launching it async
>&2 echo "Waiting for web server to launch on port 8000..."
while ! nc -z localhost 8000 &>/dev/null; do
  >&2 echo "Web server not up yet... waiting some more..."
  sleep 1
done
>&2 echo "Web server launched"


node script/axe-puppeteer.js http://localhost:8000
