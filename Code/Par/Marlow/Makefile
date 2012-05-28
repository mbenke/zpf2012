BINFILES=sudoku1
GHCOPTS=-O2 -rtsopts -threaded

all: $(BINFILES)

sudoku1: sudoku1.hs
	ghc $(GHCOPTS) -o $@ --make $^


clean:
	-rm -f *.hi *.o *~

veryclean: clean
	-rm -f $(BINFILES)

.PHONY: clean veryclean
