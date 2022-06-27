#!/usr/bin/env bash
set -euo pipefail
env ONLYDOODAD=${1-default} npx mocha script/puppeteer-tests.js --timeout 20000 --exit