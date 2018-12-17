NPM_PREFIX=$(realpath .)/node_modules
PATH:="${NPM_PREFIX}/.bin:${PATH}"
SHELL:=env PATH=${PATH} /bin/sh

.PHONY: test
test: node_modules
	elm-test

.PHONY: checks
checks:
	scripts/check-exposed.py

.PHONY: diff
diff: node_modules
	if (elm diff | tee /dev/stderr | grep -q MAJOR); then echo "MAJOR changes are not allowed!"; exit 1; fi

.PHONY: format
format: node_modules
	elm-format --validate src && elm-format --validate --elm-version=0.19 tests styleguide-app

.PHONY: clean
clean:
	rm -rf node_modules styleguide-app/elm.js styleguide-app/bundle.js $(shell find . -type d -name 'elm-stuff')

.PHONY: styleguide-app
styleguide-app: styleguide-app/elm.js
	@echo "Visit http://localhost:8000/index.html to see the styleguide app in your browser"
	cd styleguide-app && elm reactor

documentation.json: node_modules
	elm make --docs $@

styleguide-app/bundle.js: lib/index.js node_modules
	npx browserify --entry styleguide-app/manifest.js --outfile styleguide-app/bundle.js

styleguide-app/elm.js: styleguide-app/bundle.js $(shell find src styleguide-app -type f -name '*.elm')
	cd styleguide-app; elm make Main.elm --output=$(@F)

# plumbing

node_modules: package.json
	npm install
	touch -m $@

# special targets for travis, but anyone can use them, really.

.PHONY: setup
setup: node_modules

.PHONY: ci
ci: checks test format documentation.json diff styleguide-app/elm.js
