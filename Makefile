NPM_PREFIX=node_modules

.PHONY: test
test: setup styleguide-app/elm.js
	${NPM_PREFIX}/.bin/elm-package diff
	${NPM_PREFIX}/.bin/elm-format --validate src tests styleguide-app
	${NPM_PREFIX}/.bin/elm-test

.PHONY: test
clean:
	rm -rf node_modules

styleguide-app/elm.js: styleguide-app/elm-stuff $(glob styleguide-app/*.elm styleguide-app/**/*.elm)
	cd styleguide-app; ../${NPM_PREFIX}/.bin/elm-make Main.elm --output=elm.js

# plumbing

.PHONY: setup
setup: node_modules elm-stuff tests/elm-stuff styleguide-app/elm-stuff

node_modules: package.json
	npm install
	touch -m $@

elm-stuff: package.json node_modules
	${NPM_PREFIX}/.bin/elm-package install --yes
	touch -m $@

tests/elm-stuff: tests/elm-package.json node_modules
	cd tests; ../${NPM_PREFIX}/.bin/elm-package install --yes
	touch -m $@

styleguide-app/elm-stuff: styleguide-app/elm-package.json node_modules
	cd styleguide-app; ../${NPM_PREFIX}/.bin/elm-package install --yes
	touch -m $@
