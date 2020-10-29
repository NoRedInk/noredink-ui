#!/usr/bin/env bash
set -euo pipefail

JSON_FILE="${1:-}"
if test -z "$JSON_FILE"; then
    echo "Please specify a report JSON file as the first argument."
    exit 1
fi

jq -r -f script/axe-report.jq "$JSON_FILE"

NUM_ERRORS="$(jq '.violations | map(.nodes | length) | add' "$JSON_FILE")"
if test "$NUM_ERRORS" != 4;
then
    echo "$NUM_ERRORS accessibility errors"
    exit 1
fi
