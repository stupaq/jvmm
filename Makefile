DOCS := $(patsubst %.md, %.pdf, $(wildcard docs/*.md))
BNFC := $(wildcard docs/*.cf)
JVMM_EXT := .jv
EXGOOD := examples_good
EXBAD := examples_bad
SYNTAX := Syntax
MAIN := Interpreter/Main Syntax/TestJvmm

PDFLATEX := pdflatex -interaction=batchmode

all: main

main: $(MAIN)
$(MAIN): % : %.hs grammar
	ghc -w --make $< -o $@

docs: $(DOCS)
$(DOCS): %.pdf : %.md
	pandoc $< -o $@

Syntax/TestJvmm.hs: grammar
grammar: Syntax
Syntax: $(BNFC)
	bnfc -p $@ $<
	@-rm -f $@/*.bak
	happy -gca $@/ParJvmm.y
	alex -g $@/LexJvmm.x
	(cd $@/; $(PDFLATEX) DocJvmm.tex; )

test-grammar: Syntax/TestJvmm.hs
	@echo CORRECT SYNTAX:
	@$(foreach f, $(shell ls $(EXGOOD)/*$(JVMM_EXT) $(EXBAD)/*$(JVMM_EXT)), echo -n $(f) " : "; Syntax/TestJvmm $(f) | grep -q "Parse Successful!" && echo OK || { echo FAIL; exit 1; } ;)
	@echo SYNTAX ERRORS:
	@$(foreach f, $(shell ls $(EXBAD)/*.txt), echo -n $(f) " : "; Syntax/TestJvmm $(f) | grep -q "Parse Successful!" && { echo FAIL; exit 1; } || echo OK;)

stage1: grammar docs test-grammar
	zip -j $@.zip $(DOCS) Syntax/DocJvmm.pdf

clean:
	-rm -f Syntax/*.{log,aux,hi,o}

distclean: clean
	-rm -f $(DOCS) $(MAIN) *.zip
	-rm -rf Syntax

.PHONY: clean distclean test-grammar
