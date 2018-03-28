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

styleguide-app/elm.js: styleguide-app/elm-stuff $(glob styleguide-app/*.elm styleguide-app/**/*.elm)
	cd styleguide-app; elm-make Main.elm --output=elm.js

# plumbing

.PHONY: setup
setup: node_modules elm-stuff tests/elm-stuff styleguide-app/elm-stuff

node_modules: package.json
	npm install
	touch -m $@

elm-stuff: package.json node_modules
	elm-package install --yes
	touch -m $@

tests/elm-stuff: tests/elm-package.json node_modules
	elm-package install --yes
	touch -m $@

styleguide-app/elm-stuff: styleguide-app/elm-package.json node_modules
	cd styleguide-app; elm-package install --yes
	touch -m $@
