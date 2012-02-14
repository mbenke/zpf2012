MDFILE := $(word 1, $(basename $(wildcard *.md)))
L ?= $(MDFILE)
PANDOC=~/.cabal/bin/pandoc
all: $(L).html $(L)-slides.html
.PHONY: all

$(L).html: $(L).md
	@test -f $<
	$(PANDOC) -s -t html --mathjax -o $@ $<

$(L)-slides.html: $(L).md Makefile $(wildcard ./pandoc/slidy/*)
	@test -f $<
	$(PANDOC) --data-dir=./pandoc --offline -s -t slidy --mathjax -o $@ $<
