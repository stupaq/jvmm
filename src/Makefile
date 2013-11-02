BNFC := $(wildcard ../docs/*.cf)
MAIN := Interpreter/Main Syntax/TestJvmm

PDFLATEX := pdflatex -interaction=batchmode
GHCMAKE := ghc --make -outputdir ghc-make #-rtsopts -prof

all: $(MAIN)

$(MAIN): % : %.hs Syntax
	$(GHCMAKE) $< -o $@

Syntax/TestJvmm.hs: Syntax
Syntax: $(BNFC) Syntax.diff
	bnfc -p $@ $<
	(cd $@/; patch -p1 -i ../Syntax.diff)
	@-rm -f $@/*.bak
	happy --info -gca $@/ParJvmm.y
	alex -g $@/LexJvmm.x
	(cd $@/; $(PDFLATEX) DocJvmm.tex; )

clean:
	-rm -f Syntax/*.{log,aux,hi,o}
	-rm -rf ghc-make

distclean: clean
	-rm -f $(MAIN)
	-rm -rf Syntax

.PHONY: clean distclean $(MAIN)
