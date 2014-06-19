BNFC := $(wildcard ../docs/*.cf)

PDFLATEX := pdflatex -interaction=batchmode

all: Syntax

Syntax: $(BNFC) Syntax.diff
	bnfc -p $@ $<
	happy --info -gca $@/ParJvmm.y
	alex -g $@/LexJvmm.x
	(cd $@/; patch -p1 -i ../Syntax.diff)
	(cd $@/; $(PDFLATEX) DocJvmm.tex; )

clean:
	-rm -f Syntax/*.{bak,log,aux,hi,o,x,y,info,hs.orig}

distclean: clean
	-rm -f Syntax/*.{tex,txt,pdf,x,y,info}

.PHONY: clean distclean $(MAIN)
