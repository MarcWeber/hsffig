#	A Makefile for the HSFFIG-1.1 Package

all: programs/Template.hs

clean:
	find . -name '*.o' -exec rm -f \{\} \;
	find . -name '*.hi' -exec rm -f \{\} \;
	rm -f programs/Template.hs
	rm -f mkhstmpl
	runghc Setup.hs clean
	rm -f cabal-setup

programs/Template.hs: include/template-hsffig.h mkhstmpl
	mkhstmpl <include/template-hsffig.h >programs/Template.hs

mkhstmpl: mkhstmpl.hs
	ghc --make mkhstmpl.hs -o mkhstmpl

