BINFILES=parfib
GHCOPTS=-O -rtsopts -threaded

all: $(BINFILES)

parfib: parfib.hs
	ghc $(GHCOPTS) -o $@ --make $^


clean:
	-rm -f *.hi *.o *~

veryclean: clean
	-rm -f $(BINFILES)

.PHONY: clean veryclean
