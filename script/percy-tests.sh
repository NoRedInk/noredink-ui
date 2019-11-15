#!/usr/bin/env bash
set -euo pipefail

# start a web server in the background and tear it down when exiting
./script/serve.sh public &
SERVER_PID=$!
cleanup() {
    kill "$SERVER_PID"
}
trap cleanup EXIT INT

node script/percy-tests.js
