DOCS := $(patsubst %.md, %.pdf, $(wildcard docs/*.md))
BNFC := $(wildcard docs/*.cf)
JVMM_EXT := .jv
EXGOOD := examples_good
EXBAD := examples_bad
MAIN := Interpreter/Main Syntax/TestJvmm

PDFLATEX := pdflatex -interaction=batchmode
GHCMAKE := ghc -w -fwarn-incomplete-patterns --make -outputdir ghc-make

all: $(MAIN)

$(MAIN): % : %.hs
	$(GHCMAKE) $< -o $@

$(DOCS): %.pdf : %.md
	pandoc $< -o $@

# This is an ugly hack, ghc --make sucks at parallel compilation
Interpreter/Main: Syntax/TestJvmm

Syntax/TestJvmm.hs: Syntax
Syntax: $(BNFC) Syntax.diff
	bnfc -p $@ $<
	(cd $@/; patch -p1 -i ../Syntax.diff)
	@-rm -f $@/*.bak
	happy -gca $@/ParJvmm.y
	alex -g $@/LexJvmm.x
	(cd $@/; $(PDFLATEX) DocJvmm.tex; )

test-grammar: Syntax/TestJvmm
	@echo CORRECT SYNTAX:
	@$(foreach f, $(shell ls $(EXGOOD)/*$(JVMM_EXT) $(EXBAD)/*$(JVMM_EXT)), echo -n $(f) " : "; Syntax/TestJvmm $(f) | grep -q "Parse Successful!" && echo OK || { echo FAIL; exit 1; } ;)
	@echo SYNTAX ERRORS:
	@$(foreach f, $(shell ls $(EXBAD)/*.txt), echo -n $(f) " : "; Syntax/TestJvmm $(f) | grep -q "Parse Successful!" && { echo FAIL; exit 1; } || echo OK;)

clean:
	-rm -f Syntax/*.{log,aux,hi,o}
	-rm -rf ghc-make

distclean: clean
	-rm -f $(DOCS) $(MAIN) *.zip
	-rm -rf Syntax

.PHONY: clean distclean test-grammar $(MAIN)
