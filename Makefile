# Commands:

name := foo

.PHONY: build init test clean doc deploy stage

build: 
	ghc --make -O -o $(name) tic-tac-iffffy.hs

prof:
	ghc --make -prof -o $(name) tic-tac-iffffy.hs

all: build

# Cleaning commands:
clean:
	rm -f $(name)
	rm -f *.hi
	rm -f *.o
