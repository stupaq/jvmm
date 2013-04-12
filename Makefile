DOCS := $(patsubst %.md, %.pdf, $(wildcard docs/*.md))

PDFLATEX := pdflatex -interaction=batchmode

all: $(DOCS) Lang

$(DOCS): %.pdf : %.md
	pandoc $< -o $@

Lang: docs/Jvmm.cf
	bnfc -p $@ $<
	happy -gca $@/ParJvmm.y
	alex -g $@/LexJvmm.x
	(cd $@/; $(PDFLATEX) DocJvmm.tex; )
	ghc -w --make $@/TestJvmm.hs -o $@/TestJvmm

clean:
	-rm -f Lang/*.{log,aux,hi,o}

distclean: clean
	-rm -f $(DOCS)
	-rm -rf Lang

.PHONY: clean distclean
