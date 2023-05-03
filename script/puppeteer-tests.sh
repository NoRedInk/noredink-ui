#!/usr/bin/env bash
set -euo pipefail

export ONLYDOODAD=${1:-default}

if test -n "${PERCY_TOKEN:-}"; then
  npx percy exec -- mocha script/puppeteer-tests.js --timeout 100000 --exit
else
  echo "PERCY_TOKEN not set, so skipping visual diff testing."
  npx mocha script/puppeteer-tests.js --timeout 25000 --exit
fi
