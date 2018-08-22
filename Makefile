NPM_PREFIX=$(realpath .)/node_modules
PATH:=${NPM_PREFIX}/.bin:${PATH}
SHELL:=env PATH=${PATH} /bin/sh

.PHONY: test
test: elm-stuff tests/elm-stuff node_modules
	elm-test

.PHONY: diff
diff: node_modules elm-stuff
	if (elm-package diff | tee /dev/stderr | grep -q MAJOR); then echo "MAJOR changes are not allowed!"; exit 1; fi

.PHONY: sync_assets
sync_assets: elm-stuff
	exit 1

.PHONY: format
format: node_modules
	elm-format --validate src tests styleguide-app

.PHONY: clean
clean:
	rm -rf node_modules styleguide-app/elm.js styleguide-app/javascript.js $(shell find . -type d -name 'elm-stuff')

documentation.json: node_modules
	elm-make --docs $@

styleguide-app/javascript.js: lib/index.js
	npx browserify --entry lib/index.js --outfile styleguide-app/javascript.js

styleguide-app/elm.js: styleguide-app/javascript.js styleguide-app/elm-stuff src/Nri/Ui/Assets.elm $(shell find src styleguide-app -type f -name '*.elm')
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

.NOTPARALLEL: src/Nri/Ui/Assets.elm
src/Nri/Ui/Assets.elm: $(shell find styleguide-app/assets/images -type f -name '*.svg')
	npx svgo $^ --multipass --disable=removeViewBox
	mkdir -p tmp/
	node scripts/svgToElm.js $^ > tmp/Assets.elm
	elm-format tmp/Assets.elm --output $@

# special targets for travis, but anyone can use them, really.

.PHONY: setup
setup: node_modules elm-stuff tests/elm-stuff styleguide-app/elm-stuff

.PHONY: ci
ci: test format documentation.json diff styleguide-app/elm.js
