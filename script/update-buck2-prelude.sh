#!/usr/bin/env bash
set -euo pipefail

cd $(git rev-parse --show-toplevel)
git subtree pull --prefix prelude git@github.com:facebookincubator/buck2-prelude.git main --squash
