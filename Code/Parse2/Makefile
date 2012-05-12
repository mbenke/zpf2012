BINFILES=pmuu pm1 pm2
GHCOPTS=-O -rtsopts

all: $(BINFILES)

pmuu: PlusMinusUU.hs 
	ghc $(GHCOPTS) -o $@ --make $^

pm1: PlusMinus1.hs App1.hs Applicative.hs
	ghc $(GHCOPTS) -o $@ --make $^

pm2: PlusMinus2.hs App2.hs Applicative.hs
	ghc $(GHCOPTS) -o $@ --make $^

clean:
	-rm -f *.hi *.o

veryclean: clean
	-rm -f $(BINFILES)