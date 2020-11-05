#!/usr/bin/env bash
set -euo pipefail

# start a watcher. This loops forever, so we don't need to loop ourselves.
git ls-files | entr shake public
