MDFILES := $(wildcard *.md)
TARGETS := $(patsubst %.md,%.html,$(MDFILES))
PANDOC=~/.cabal/bin/pandoc

all: $(TARGETS)
.PHONY: all

%.html: %.md
	$(PANDOC) --data-dir=../Slides/pandoc --css=../style.css \
	-s -t html -o $@ $<

clean:
	rm -f *~
.PHONY: clean