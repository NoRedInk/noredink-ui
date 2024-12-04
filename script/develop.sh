#!/usr/bin/env bash
set -euo pipefail

if test -d public; then
    rm -rf public
fi

SHAKE_TARGET="${1:-public}"

shake --compact "$SHAKE_TARGET"

cat <<EOF
== 👋 Hello! ==================================================================

I'm watching files in component-catalog and src for changes. If you make any
changes, I'll try to be smart about what should change (things end up in the
"public" directory if you want to check my work.) If you remove a file and it's
still showing up, delete the "public" directory and restart me.

To quit, hit ctrl-c.

== thaaat's it from me! =======================================================

EOF

(cd ./component-catalog; elm-live ./src/Main.elm --pushstate --hot --dir=../$SHAKE_TARGET -- --output=../$SHAKE_TARGET/elm.js)
