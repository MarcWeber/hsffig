#	A Makefile for the HSFFIG-1.0 Package

all: cabal-setup

cabal-setup:
	ghc -O -cpp --make Setup.hs -o cabal-setup
	echo 'The \'cabal-setup\' program has been built'
	echo 'Use it for building and installing the package'
	echo 'Type \'cabal-setup --help\' for help'


