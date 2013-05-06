DOCS := $(patsubst %.md, %.pdf, $(wildcard docs/*.md))
BNFC := $(wildcard docs/*.cf)
JVMM_EXT := .jv
EXGOOD := examples_good
EXBAD := examples_bad
MAIN := Interpreter/Main Syntax/TestJvmm

PDFLATEX := pdflatex -interaction=batchmode
GHCMAKE := ghc -w -fwarn-incomplete-patterns --make -outputdir ghc-make #-rtsopts -prof

all: $(MAIN)
	ln -sf Interpreter/Main ./interpreter

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

test-examples: all
	@./run_examples.sh

clean:
	-rm -f test.{err,out}
	-rm -f Syntax/*.{log,aux,hi,o}
	-rm -rf ghc-make

distclean: clean
	-rm -f $(DOCS) $(MAIN) *.zip ./interpreter
	-rm -rf Syntax

.PHONY: clean distclean test-examples $(MAIN)
