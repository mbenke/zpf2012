BINFILES=pmuu pm1 pm2 pm4 pm4g
GHCOPTS=-O -rtsopts

all: $(BINFILES)

pmuu: PlusMinusUU.hs 
	ghc $(GHCOPTS) -o $@ --make $^

pm1: PlusMinus1.hs App1.hs Applicative.hs
	ghc $(GHCOPTS) -o $@ --make $^

pm2: PlusMinus2.hs App2.hs Applicative.hs
	ghc $(GHCOPTS) -o $@ --make $^

pm3: PlusMinus3.hs App3.hs Applicative.hs
	ghc $(GHCOPTS) -o $@ --make $^

pm4: PlusMinus4.hs App4.hs Applicative.hs
	ghc $(GHCOPTS) -o $@ --make $^

pm4g: PlusMinus4g.hs App4g.hs Applicative.hs
	ghc $(GHCOPTS) -o $@ --make $^

clean:
	-rm -f *.hi *.o

veryclean: clean
	-rm -f $(BINFILES)