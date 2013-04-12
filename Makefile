DOCS := $(patsubst %.md, %.pdf, $(wildcard docs/*.md))
JVMM_EXT := .jv

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

test-grammar: Lang
	@echo CORRECT SYNTAX:
	@$(foreach f, $(shell ls examples*/*$(JVMM_EXT)), echo -n $(f) " : "; Lang/TestJvmm $(f) | grep -q "Parse Successful!" && echo OK || echo FAIL;)
	@echo SYNTAX ERRORS:
	@$(foreach f, $(shell ls examples*/*.txt), echo -n $(f) " : "; Lang/TestJvmm $(f) | grep -q "Parse Successful!" && echo FAIL || echo OK;)

clean:
	-rm -f Lang/*.{log,aux,hi,o}

distclean: clean
	-rm -f $(DOCS)
	-rm -rf Lang

.PHONY: clean distclean test-grammar
