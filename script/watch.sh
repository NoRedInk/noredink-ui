#!/usr/bin/env bash
set -euo pipefail

if ! test -d public; then make public; fi

declare SERVER_PID

# wait for an interrupt and kill both of them
cleanup() {
    if ! test -z "$SERVER_PID"; then kill "$SERVER_PID"; fi
}
trap cleanup EXIT INT

# start a web server
./script/serve.sh public &
SERVER_PID=$!

# start a watcher. This loops forever, so we don't need to loop ourselves.
find src styleguide-app -type f -not -ipath '*elm-stuff*' | entr -c -p make public
