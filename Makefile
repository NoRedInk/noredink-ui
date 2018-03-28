NPM_PREFIX=$(realpath node_modules)
PATH:=${NPM_PREFIX}/.bin:${PATH}

.PHONY: test
test: setup styleguide-app/elm.js
	elm-package diff
	elm-format --validate src tests styleguide-app
	elm-test

.PHONY: test
clean:
	rm -rf node_modules styleguide-app/elm.js $(shell find . -type d -name 'elm-stuff')

styleguide-app/elm.js: styleguide-app/elm-stuff styleguide-app/**/*.elm
	cd styleguide-app; elm-make Main.elm --output=elm.js

# plumbing

.PHONY: setup
setup: node_modules elm-stuff tests/elm-stuff styleguide-app/elm-stuff

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
