#!/usr/bin/env bash
set -euo pipefail
npx mocha script/puppeteer-tests.js --timeout 15000 --exit