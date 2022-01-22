#!/usr/bin/env bash
set -euo pipefail
npx percy exec -- mocha script/percy-tests.js --exit
