MDFILE := $(word 1, $(basename $(wildcard *.md)))
L ?= $(MDFILE)
PANDOC=~/.cabal/bin/pandoc
all: $(L).html $(L)-slides.html
.PHONY: all

$(L).html: $(L).md
	@test -f $<
	$(PANDOC) -s -t html -o $@ $<

$(L)-slides.html: $(L).md #$(wildcard ../pandoc/slidy/*)
	@test -f $<
	$(PANDOC) --offline -s -t slidy -o $@ $<
