GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XNoCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.5

.PHONY: all clean doc install

all: report.html doc dist/build/routeGenerator/routeGenerator dist/routeGenerator-$(VERSION).tar.gz

install: dist/build/routeGenerator/routeGenerator
	cabal install

report.html: routeGenerator.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/routeGenerator/index.html README

README: route-generator.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/routeGenerator/index.html: dist/setup-config routeGenerator.hs
	-cabal haddock --hyperlink-source --executables

dist/setup-config: route-generator.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist dist-ghc

dist/build/routeGenerator/routeGenerator: route-generator.cabal dist/setup-config routeGenerator.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/routeGenerator-$(VERSION).tar.gz: route-generator.cabal dist/setup-config README routeGenerator.hs
	cabal check
	cabal sdist
