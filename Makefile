.PHONY: all build clean configure haddock hpc install repl run test

all: install configure build haddock test hpc

build:
	touch library/Sweetroll/Conf.hs
	cabal build

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

configure:
	cabal configure --enable-tests --enable-coverage -v2

haddock:
	cabal haddock --hyperlink-source
	open dist/doc/html/sweetroll/index.html

hpc:
	hpc markup --destdir=tmp dist/hpc/tix/tests/tests.tix
	open tmp/hpc_index.html

install:
	cabal sandbox init
	cabal install --enable-tests --only-dependencies --reorder-goals

repl:
	cabal repl lib:sweetroll --ghc-options="-fno-hpc"

run:
	touch library/Sweetroll/Conf.hs
	rm sweetroll.tix || true
	cabal run sweetroll

test:
	touch library/Sweetroll/Conf.hs
	cabal test examples tests --show-details=always --test-option=--color
