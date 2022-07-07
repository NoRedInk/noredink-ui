#!/usr/bin/env bash
set -euo pipefail

git ls-files | grep -E '.js$' | xargs node_modules/.bin/prettier --write
