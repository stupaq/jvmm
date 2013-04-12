DOCS := $(patsubst %.md, %.pdf, $(wildcard docs/*.md))
BNFC := $(wildcard docs/*.cf)
EXGOOD := examples_good
EXBAD := examples_bad
LMODULE := Syntax
JVMM_EXT := .jv

PDFLATEX := pdflatex -interaction=batchmode

all: grammar

docs: $(DOCS)
$(DOCS): %.pdf : %.md
	pandoc $< -o $@

grammar: $(LMODULE)
$(LMODULE): $(BNFC)
	bnfc -p $@ $<
	@-rm -f $@/*.bak
	happy -gca $@/ParJvmm.y
	alex -g $@/LexJvmm.x
	(cd $@/; $(PDFLATEX) DocJvmm.tex; )
	ghc -w --make $@/TestJvmm.hs -o $@/TestJvmm

test-grammar: grammar
	@echo CORRECT SYNTAX:
	@$(foreach f, $(shell ls $(EXGOOD)/*$(JVMM_EXT) $(EXBAD)/*$(JVMM_EXT)), echo -n $(f) " : "; $(LMODULE)/TestJvmm $(f) | grep -q "Parse Successful!" && echo OK || { echo FAIL; exit 1; } ;)
	@echo SYNTAX ERRORS:
	@$(foreach f, $(shell ls $(EXBAD)/*.txt), echo -n $(f) " : "; $(LMODULE)/TestJvmm $(f) | grep -q "Parse Successful!" && { echo FAIL; exit 1; } || echo OK;)

stage1: grammar docs test-grammar
	zip -j $@.zip $(DOCS) $(LMODULE)/DocJvmm.pdf

clean:
	-rm -f $(LMODULE)/*.{log,aux,hi,o}

distclean: clean
	-rm -f $(DOCS)
	-rm -rf $(LMODULE)
	-rm -rf *.zip

.PHONY: clean distclean test-grammar
