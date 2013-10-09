BNFC := $(wildcard ../docs/*.cf)
MAIN := Interpreter/Main Syntax/TestJvmm

PDFLATEX := pdflatex -interaction=batchmode
GHCMAKE := ghc -w -fwarn-incomplete-patterns --make -outputdir ghc-make #-rtsopts -prof

all: $(MAIN)

$(MAIN): % : %.hs
	$(GHCMAKE) $< -o $@

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
	-rm -f Syntax/*.{log,aux,hi,o}
	-rm -rf ghc-make

distclean: clean
	-rm -f $(MAIN)
	-rm -rf Syntax

.PHONY: clean distclean test-examples $(MAIN)
