BINFILES=pmparsec pm2a
GHCOPTS=-O -rtsopts

all: $(BINFILES)

pmparsec: PlusMinusParsec.hs
	ghc $(GHCOPTS) -o $@ --make $^

pm2a: PlusMinus2a.hs
	ghc $(GHCOPTS) -o $@ --make $^

clean:
	-rm -f *.hi *.o

veryclean: clean
	-rm -f $(BINFILES)