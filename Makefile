NPM_PREFIX=$(realpath .)/node_modules
PATH:=${NPM_PREFIX}/.bin:${PATH}
SHELL:=env PATH=${PATH} /bin/sh

.PHONY: test
test: elm-stuff tests/elm-stuff node_modules
	elm-test

.PHONY: diff
diff: node_modules elm-stuff
	if (elm-package diff | tee /dev/stderr | grep -q MAJOR); then echo "MAJOR changes are not allowed!"; exit 1; fi

.PHONY: format
format: node_modules
	elm-format --validate src tests styleguide-app

.PHONY: clean
clean:
	rm -rf node_modules styleguide-app/elm.js $(shell find . -type d -name 'elm-stuff')

styleguide-app/elm.js: styleguide-app/elm-stuff styleguide-app/**/*.elm
	cd styleguide-app; elm-make Main.elm --output=$(@F)

# plumbing

node_modules: package.json
	npm install
	touch -m $@

.NOTPARALLEL: elm-stuff
elm-stuff: elm-package.json node_modules
	elm-package install --yes
	touch -m $@

.NOTPARALLEL: %/elm-stuff
%/elm-stuff: %/elm-package.json node_modules
	cd $(@D); elm-package install --yes
	touch -m $@

# special targets for travis, but anyone can use them, really.

.PHONY: setup
setup: node_modules elm-stuff tests/elm-stuff styleguide-app/elm-stuff

.PHONY: ci
ci: test format diff styleguide-app/elm.js
