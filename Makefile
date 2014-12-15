SHELL := /bin/bash

.PHONY: all deps build test run lint clean

all: build

deps:
	cabal install --only-dependencies --enable-tests
	sudo cabal install --global graphmod
	apt-get install -y xdot

build:
	@cabal build --ghc-options=$(GHC_OPTS)

test:
	@cabal test --show-details=always --test-options="--color"

test-loop:
	@res=0; while [[ $$res == 0 ]]; do echo "=== Test ==="; make test; res=$$?; sleep 1; done

run:
	@cabal run main

lint:
	@hlint . \
		--ignore="Eta reduce" \
		--ignore="Reduce duplication" \
		--ignore="Use camelCase"

graphmod:
	@find src -name '*.hs' | xargs graphmod -a -q | xdot

graphmod-all:
	@find . -name '*.hs' | xargs graphmod -a -q | xdot

todo:
	@find . -name '*.hs' \
		| xargs grep -i \
			"todo\|fixme\|not implemented" \
			--color=always -n \
			|| exit 0

clean:
	@find . -name "*.o" -type f -delete
	@find . -name "*.hi" -type f -delete
	@find . -name "*.tix" -type f -delete
	@cabal clean
