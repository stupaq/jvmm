DOCS := $(patsubst %.md, %.pdf, $(wildcard docs/*.md))

PDFLATEX := pdflatex -interaction=batchmode

all:
	$(MAKE) -C src/ all
	ln -sf src/Interpreter/Main ./latc
	ln -sf src/Syntax/TestJvmm ./latc_syntax

$(DOCS): %.pdf : %.md
	pandoc $< -o $@

test-jvmm: all
	@./test-jvmm/test-run.sh

clean:
	$(MAKE) -C src/ clean
	-rm -f test.{err,out}

distclean: clean
	$(MAKE) -C src/ distclean
	-rm -f $(DOCS) *.zip ./latc ./latc_syntax

.PHONY: clean distclean test-examples
