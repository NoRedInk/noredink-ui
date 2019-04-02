#!/usr/bin/env bash
set -euo pipefail

# start a web server
./scripts/serve.sh public &
SERVER_PID=$!

# start a watcher
find src styleguide-app -type f -not -ipath '*elm-stuff*' | entr make public &
WATCHER_PID=$!

# wait for an interrupt and kill both of them
cleanup() {
    kill $SERVER_PID $WATCHER_PID
}
trap cleanup EXIT INT

while : ; do sleep 10; done
