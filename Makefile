NPM_PREFIX=$(realpath .)/node_modules
PATH:="${NPM_PREFIX}/.bin:${PATH}"
SHELL:=env PATH=${PATH} /bin/sh

.PHONY: test
test: elm-stuff tests/elm-stuff node_modules
	elm-test

.PHONY: checks
checks:
	scripts/check-exposed.py

.PHONY: diff
diff: node_modules elm-stuff
	if (elm-package diff | tee /dev/stderr | grep -q MAJOR); then echo "MAJOR changes are not allowed!"; exit 1; fi

.PHONY: format
format: node_modules
	elm-format --validate src && elm-format --validate --elm-version=0.18 tests styleguide-app

.PHONY: clean
clean:
	rm -rf node_modules styleguide-app/elm.js styleguide-app/javascript.js $(shell find . -type d -name 'elm-stuff')

documentation.json: node_modules
	elm-make --docs $@

styleguide-app/javascript.js: lib/index.js
	npx browserify --entry lib/index.js --outfile styleguide-app/javascript.js

styleguide-app/elm.js: styleguide-app/javascript.js styleguide-app/elm-stuff $(shell find src styleguide-app -type f -name '*.elm')
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
ci: checks test format documentation.json diff styleguide-app/elm.js
