#	A Makefile for the HSFFIG-1.1 Package

all: programs/Template.hs programs/Makefile.hs programs/Setupfile.hs programs/SetupfileNew.hs

clean:
	find . -name '*.o' -exec rm -f \{\} \;
	find . -name '*.hi' -exec rm -f \{\} \;
	rm -f programs/Template.hs
	rm -f programs/Makefile.hs
	rm -f programs/Setupfile.hs
	rm -f programs/SetupfileNew.hs
	rm -f mktmpl
	runghc Setup.hs clean
	rm -f cabal-setup

programs/Template.hs: include/template-hsffig.h mktmpl
	./mktmpl Template <include/template-hsffig.h >programs/Template.hs

mktmpl: mktmpl.hs
	ghc --make mktmpl.hs -o mktmpl

programs/Makefile.hs: include/template-makefile mktmpl
	./mktmpl Makefile <include/template-makefile >programs/Makefile.hs

programs/Setupfile.hs: include/template-setup mktmpl
	./mktmpl Setupfile <include/template-setup >programs/Setupfile.hs

programs/SetupfileNew.hs: include/template-setup-new mktmpl
	./mktmpl SetupfileNew <include/template-setup-new >programs/SetupfileNew.hs

