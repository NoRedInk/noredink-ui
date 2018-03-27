NPM_PREFIX=node_modules

.PHONY: test
test: setup
	${NPM_PREFIX}/.bin/elm-package diff
	${NPM_PREFIX}/.bin/elm-format --validate src tests styleguide-app
	${NPM_PREFIX}/.bin/elm-test
	cd styleguide-app; ../${NPM_PREFIX}/.bin/elm-make Main.elm --yes --output=elm.js

.PHONY: test
clean:
	rm -rf node_modules

# plumbing

.PHONY: setup
setup: node_modules elm-stuff tests/elm-stuff

node_modules: package.json
	npm install
	touch -m $@

elm-stuff: package.json node_modules
	${NPM_PREFIX}/.bin/elm-package install --yes
	touch -m $@

tests/elm-stuff: tests/elm-package.json
	cd tests; ../${NPM_PREFIX}/.bin/elm-package install --yes
	touch -m $@
