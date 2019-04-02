#!/usr/bin/env bash
set -euo pipefail

TARGET="${1:-}"
if test -z "$TARGET"; then
    echo "USAGE: ${0:-} directory-to-serve"
    exit 1
fi

if ! test -d "$TARGET"; then
    echo 'cannot serve a non-directory, exiting!'
    exit 1
fi

if ! which python > /dev/null; then
    echo 'need a python installed, exiting!'
    exit 1
fi

cd "$TARGET"

if python --version | grep -qE '^Python 3'; then
    exec python -m http.server
else
    exec python -m SimpleHTTPServer
fi
