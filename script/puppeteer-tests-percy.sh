#!/usr/bin/env bash
set -euo pipefail
npx percy exec -- mocha script/puppeteer-tests.js --timeout 50000 --exit
