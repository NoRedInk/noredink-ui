SHELL:=env PATH=${PATH} /bin/sh

.PHONY: test
test: node_modules
	npx elm-test

.PHONY: checks
checks:
	scripts/check-exposed.py

.PHONY: diff
diff: node_modules
	if (npx elm diff | tee /dev/stderr | grep -q MAJOR); then echo "MAJOR changes are not allowed!"; exit 1; fi

.PHONY: format
format: node_modules
	npx elm-format --validate src && npx elm-format --validate --elm-version=0.19 tests styleguide-app

.PHONY: clean
clean:
	rm -rf node_modules styleguide-app/elm.js styleguide-app/bundle.js $(shell find . -type d -name 'elm-stuff') public

.PHONY: styleguide-app
styleguide-app: styleguide-app/elm.js
	@echo "the styleguide-app command is deprecated; use serve-public."
	@exit 1

documentation.json: node_modules
	npx elm make --docs $@

styleguide-app/bundle.js: lib/index.js styleguide-app/manifest.js styleguide-app/assets/generated_svgs.js styleguide-app/assets/clipboard-setup.js node_modules
	npx browserify --entry styleguide-app/manifest.js --outfile styleguide-app/bundle.js

styleguide-app/elm.js: styleguide-app/bundle.js $(shell find src styleguide-app -type f -name '*.elm')
	cd styleguide-app; npx elm make Main.elm --output=$(@F)

# for publishing styleguide

# We don't want to have to generate new rules for every single asset, so we find
# all the ones that exist (`STYLEGUIDE_ASSETS`) then replace the roots
# (`PUBLIC_ASSETS`). The `%` wildcard works like it does in `public/%` below.
STYLEGUIDE_ASSETS=$(shell find styleguide-app/assets -type f)
PUBLIC_ASSETS=$(STYLEGUIDE_ASSETS:styleguide-app/assets/%=public/assets/%)

public: public/index.html public/elm.js public/bundle.js $(PUBLIC_ASSETS)
	touch -m $@

# wildcard rule: % on the left-hand side will be matched and replaced on the
# right-hand side. So `public/index.html` depends on `styleguide-app/index.html`
#
# - automatic variables: `$@` is the target (left-hand side of the rule.) `$<`
#   is the first dependency.
# - about the leading `@` in `mkdir`: leading `@` turns off echoing the
#   command. We're just reducing log spam here.
# - about `$(@D)`: $@ gets the target (left-hand side of the rule). Any
#   automatic variable plus `D` gets the directory of that file, so `$(@D)` is
#   the target's directory.
public/%: styleguide-app/%
	@mkdir -p $(@D)
	cp $< $@

.PHONY: serve-public
serve-public: public
	./scripts/serve.sh public

# plumbing

node_modules: package.json
	npm install
	touch -m $@

# special targets for travis, but anyone can use them, really.

.PHONY: setup
setup: node_modules

.PHONY: ci
ci: checks test format documentation.json diff styleguide-app/elm.js
