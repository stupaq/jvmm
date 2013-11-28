BNFC := $(wildcard ../docs/*.cf)
MAIN := Interpreter/Main

PDFLATEX := pdflatex -interaction=batchmode
GHCMAKE := ghc --make -outputdir ghc-make -w #-rtsopts -prof

all: $(MAIN)

$(MAIN): % : %.hs Syntax
	$(GHCMAKE) $< -o $@

Syntax: $(BNFC) Syntax.diff
	bnfc -p $@ $<
	(cd $@/; patch -p1 -i ../Syntax.diff)
	@-rm -f $@/*.bak
	happy --info -gca $@/ParJvmm.y
	alex -g $@/LexJvmm.x
	(cd $@/; $(PDFLATEX) DocJvmm.tex; )

parserclean:
	-rm -f Syntax/*.{log,aux,hi,o,x,y,info,hs.orig}

clean: parserclean
	-rm -rf ghc-make

distclean: clean
	-rm -f Syntax/*.{tex,txt,pdf,x,y,info}
	-rm -f $(MAIN)

.PHONY: clean distclean $(MAIN)
