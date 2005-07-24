#	A Makefile for the HSFFIG-1.0 Package

all: programs/Template.hs cabal-setup

cabal-setup:
	ghc -cpp --make Setup.hs -o cabal-setup
	@echo The 'cabal-setup' program has been built
	@echo Use it for building and installing the package
	@echo Type 'cabal-setup --help' for help

clean:
	find . -name '*.o' -exec rm -f \{\} \;
	find . -name '*.hi' -exec rm -f \{\} \;
	rm -f programs/Template.hs
	rm -f mkhstmpl
	-cabal-setup clean
	rm -f cabal-setup

programs/Template.hs: include/template-hsffig.h mkhstmpl
	mkhstmpl <include/template-hsffig.h >programs/Template.hs

mkhstmpl: mkhstmpl.hs
	ghc --make mkhstmpl.hs -o mkhstmpl

