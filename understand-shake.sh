#!/usr/bin/env bash

# not really a script file, just logging my commands as i go through my learning Shakefile.hs

shake single-dep
# rule prints

shake single-dep
# rule doesn't print, caching yay

rm elm.json
shake single-dep
# same error we get in ci!

shake all-elms
# rule prints

shake all-elms
# rule is cached

rm src/CheckboxIcons.elm
shake all-elms
# rule doesn't break

echo "bla" > src/EventExtras.elm
shake all-elms
# rule won't rebuild!

git checkout src
shake all-elm-contents
# rule prints

rm src/CheckboxIcons.elm
shake all-elm-contents
# rule rebuilds, no errors

git co src
printf "src/CheckboxIcons.elm" > bla.elm
shake dependencies
# rule prints

rm src/CheckboxIcons.elm
printf "src/EventExtras.elm" > bla.elm
shake dependencies
# rule prints

git co src
printf "src/CheckboxIcons.elm" > bla.elm
shake unsafe-dependencies
# rule prints

rm src/CheckboxIcons.elm
printf "src/EventExtras.elm" > bla.elm
shake unsafe-dependencies
# it still works wth?? i totes expected it to fail same as ci

git co src
shake dirfiles
# rule prints

rm src/CheckboxIcons.elm
shake dirfiles
# still works!! why???

git co src

