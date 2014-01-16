BNFC := $(wildcard ../docs/*.cf)
MAIN := Jvmm/Main

PDFLATEX := pdflatex -interaction=batchmode
GHCWARN := -W \
	-Wall \
	-fwarn-tabs \
	-fwarn-monomorphism-restriction \
	-fwarn-unrecognised-pragmas \
	-fwarn-auto-orphans \
	-fno-warn-unused-do-bind
GHCPROF := #-rtsopts -prof
GHCMAKE := ghc --make -outputdir ghc-make $(GHCWARN) $(GHCPROF)

all: $(MAIN)

$(MAIN): % : %.hs Syntax
	$(GHCMAKE) $< -o $@

lint:
	find `dirname $(MAIN)` -name "*.hs" -exec hlint "{}" \;

Syntax: $(BNFC) Syntax.diff
	bnfc -p $@ $<
	happy --info -gca $@/ParJvmm.y
	alex -g $@/LexJvmm.x
	(cd $@/; patch -p1 -i ../Syntax.diff)
	(cd $@/; $(PDFLATEX) DocJvmm.tex; )

parserclean:
	-rm -f Syntax/*.{bak,log,aux,hi,o,x,y,info,hs.orig}

clean: parserclean
	-rm -rf ghc-make

distclean: clean
	-rm -f Syntax/*.{tex,txt,pdf,x,y,info}
	-rm -f $(MAIN)

.PHONY: lint parserclean clean distclean $(MAIN)
