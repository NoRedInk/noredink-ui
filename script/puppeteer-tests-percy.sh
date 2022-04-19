#!/usr/bin/env bash
set -euo pipefail
env ONLYDOODAD=${1-default} npx percy exec -- mocha script/puppeteer-tests.js --timeout 50000 --exit
