#!/usr/bin/env bash
set -xeuo pipefail

# Netlify doesn't support building stuff via Haskell or
# Nix. Those things are vaguely on the horizon (check the issues at
# https://github.com/netlify/build-image) but for now it's way simpler to just
# accept that Netlify will need separate instructions.

# get our dependencies (--ignore-scripts=false is needed for puppeteer)
npm install --ignore-scripts=false
npm install elm

# make sure we're building into a clean folder
if test -d public; then rm -rf public; fi
mkdir public

# build the interactive parts
(cd styleguide-app && npx elm make Main.elm --output ../public/elm.js)
npx browserify --entry styleguide-app/manifest.js --outfile public/bundle.js

# copy assets
cp -r styleguide-app/assets public/assets
