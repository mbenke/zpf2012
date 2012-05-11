BINFILES=pm1
GHCOPTS=-O -rtsopts

all: $(BINFILES)

pm1: PlusMinus1.hs App1.hs Applicative.hs
	ghc $(GHCOPTS) -o $@ --make $^

clean:
	-rm -f *.hi *.o

veryclean: clean
	-rm -f $(BINFILES)