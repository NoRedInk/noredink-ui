#!/usr/bin/env bash
set -euo pipefail

# start a web server in the background and tear it down when exiting
./script/serve.sh public &

# Wait for the python server to launch since we're launching it async
sleep 3

SERVER_PID=$!
cleanup() {
    kill "$SERVER_PID"
}
trap cleanup EXIT INT

node script/axe-puppeteer.js http://localhost:8000
