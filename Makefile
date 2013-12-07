DOCS := $(patsubst %.md, %.pdf, $(wildcard docs/*.md))
TESTSUITES := $(wildcard test-*)
JVM_RUNTIME := lib/Runtime.class

PDFLATEX := pdflatex -interaction=batchmode

all: $(JVM_RUNTIME)
	$(MAKE) -C src/ all
	ln -sf src/Jvmm/Main ./latc

$(JVM_RUNTIME): %.class: %.java
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
