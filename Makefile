DOCS := $(patsubst %.md, %.pdf, $(wildcard docs/*.md))
TESTSUITES := $(wildcard test-*)

PDFLATEX := pdflatex -interaction=batchmode

all:
	$(MAKE) -C src/ all
	javac lib/Runtime.java
	ln -sf src/Jvmm/Main ./latc

docs: $(DOCS)
$(DOCS): %.pdf : %.md
	pandoc $< -o $@

$(TESTSUITES): % : all
	@./$@/test-run.sh

clean:
	$(MAKE) -C src/ clean
	-rm -f lib/Runtime.class
	-rm -f test.{err,out}

distclean: clean
	$(MAKE) -C src/ distclean
	-rm -f $(DOCS) *.zip ./latc ./latc_syntax

.PHONY: clean distclean docs
