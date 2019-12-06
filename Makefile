SHELL:=env PATH=${PATH} /bin/sh
export DEPRECATED_MODULES=Html,Accessibility,Accessibility.Aria,Accessibility.Key,Accessibility.Landmark,Accessibility.Live,Accessibility.Role,Accessibility.Style,Accessibility.Widget

.PHONY: test
test: node_modules
	npx elm-test
	npx elm-verify-examples --run-tests
	make axe-report
	make percy-tests
	make deprecated-imports-report

tests/axe-report.json: public script/run-axe.sh script/axe-puppeteer.js
	script/run-axe.sh > $@

.PHONY: axe-report
axe-report: tests/axe-report.json script/format-axe-report.sh script/axe-report.jq
	script/format-axe-report.sh $<

.PHONY: percy-tests
percy-tests:
	script/percy-tests.sh

tests/deprecated-imports-report.txt: $(shell find src -type f) script/deprecated-imports.py
	script/deprecated-imports.py report > $@

script/deprecated-imports.csv: $(shell find src -type f) script/deprecated-imports.py
	script/deprecated-imports.py --imports-file $@ update 

.PHONY: deprecated-imports-report
deprecated-imports-report: tests/deprecated-imports-report.txt script/deprecated-imports.py
	@cat tests/deprecated-imports-report.txt
	@script/deprecated-imports.py --check-message-fix-command='make script/deprecated-imports.csv' check

.PHONY: checks
checks:
	script/check-exposed.py

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
styleguide-app:
	./script/develop.sh

documentation.json: node_modules
	npx elm make --docs $@

styleguide-app/bundle.js: lib/index.js styleguide-app/manifest.js node_modules
	npx browserify --entry styleguide-app/manifest.js --outfile styleguide-app/bundle.js

styleguide-app/elm.js: styleguide-app/bundle.js $(shell find src styleguide-app -type f -name '*.elm')
	cd styleguide-app && npx elm make Main.elm --output=$(@F)

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

# plumbing

node_modules: package.json
	npm install
	touch -m $@

# special targets for travis, but anyone can use them, really.

.PHONY: setup
setup: node_modules

.PHONY: ci
ci: checks test format documentation.json diff styleguide-app/elm.js
