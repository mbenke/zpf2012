# Makefile borrowed from github.com/bos/stanford-cs240h
DESTDIR=../../www
MDFILE := $(word 1, $(basename $(wildcard *.md)))

L ?= $(MDFILE)
PANDOC=~/.cabal/bin/pandoc
all: $(L).html $(L)-slides.html
.PHONY: all echo

$(L).html: $(L).md
	@test -f $<
	$(PANDOC) -s -t html --mathjax -o $(DESTDIR)/$@ $<

$(L)-slides.html: $(L).md Makefile $(wildcard ./pandoc/slidy/*)
	@test -f $<
	$(PANDOC) --data-dir=../pandoc --offline -s -t slidy --mathjax -o $(DESTDIR)/$@ $<

echo: 
	echo $(MDFILE)