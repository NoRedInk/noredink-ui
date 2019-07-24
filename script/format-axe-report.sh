#!/usr/bin/env bash
set -euo pipefail

JSON_FILE="${1:-}"
if test -z "$JSON_FILE"; then
    echo "Please specify a report JSON file as the first argument."
    exit 1
fi

jq -r -f script/axe-report.jq "$JSON_FILE"

# ideally we'd fail on any failures, but we have had a bunch build up over time!
# So right now, we need to fail if the error count is not exactly what we
# expect. This failure reminds us to come back and ratchet down the number of
# failures to the correct value.
NUM_ERRORS="$(jq '.violations | map(.nodes | length) | add' "$JSON_FILE")"
TARGET_ERRORS=155
if test "$NUM_ERRORS" -ne "$TARGET_ERRORS"; then
    echo "got $NUM_ERRORS errors, but expected $TARGET_ERRORS."
    echo
    echo 'If it went down, hooray!'
    echo "Check out ${0:-} and change the count to the reported value above."
    echo
    echo "If it went up, let's fix it instead."
    echo "Since there are so many errors right now, a decent debugging strategy is:"
    echo
    echo " 1. save tests/axe-report.log somewhere ('mv tests/axe-report.log tests/axe-report.log.failing' is one way)"
    echo " 2. undo your changes ('git stash' or 'checkout master')"
    echo " 3. regenerate the log with 'make tests/axe-report.log'"
    echo " 4. compare the output with 'diff -u tests/axe-report.log tests/axe-report.log.failing'"
fi
