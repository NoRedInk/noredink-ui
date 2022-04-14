#!/usr/bin/env bash
set -euo pipefail
npx percy exec -- mocha script/puppeteer-tests.js --exit
