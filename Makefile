DOCS := $(patsubst %.md, %.pdf, $(wildcard docs/*.md))
TESTSUITES := $(wildcard test-*)

PDFLATEX := pdflatex -interaction=batchmode

all: runtime-jvm
	$(MAKE) -C src/ all
	ln -sf src/Jvmm/Main ./latc

runtime-jvm: lib/Runtime.java
	javac $<

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
