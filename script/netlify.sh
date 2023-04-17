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
(cd component-catalog && npx elm make src/Main.elm --output ../public/elm.js)
npx browserify --entry component-catalog/manifest.js --outfile public/bundle.js

# copy static files
cp component-catalog/index.html public/index.html
cp component-catalog/favicon.svg public/favicon.svg
cp component-catalog/elm.json public/application.json
cp elm.json public/package.json
