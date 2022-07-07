#!/usr/bin/env bash
set -euo pipefail

if test -d public; then
    rm -rf public
fi

SHAKE_TARGET="${1:-public}"

shake --compact "$SHAKE_TARGET"

cat <<EOF
== 👋 Hello! ==================================================================

I'm watching files in styleguide, styleguide-app, and src for changes. If you make any
changes, I'll try to be smart about what should change (things end up in the
"public" directory if you want to check my work.) If you remove a file and it's
still showing up, delete the "public" directory and restart me.

To quit, hit ctrl-c.

== thaaat's it from me! =======================================================

EOF

# start a web server in the background and tear it down when exiting
./script/serve.sh public &
SERVER_PID=$!
cleanup() {
    kill "$SERVER_PID"
}
trap cleanup EXIT INT

# start a watcher. This loops forever, so we don't need to loop ourselves.
watchexec --clear --postpone -- shake --compact "$SHAKE_TARGET"
