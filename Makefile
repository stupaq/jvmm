DOCS := $(patsubst %.md, %.pdf, $(wildcard docs/*.md))
TESTSUITES := $(wildcard test-*)
JVM_RUNTIME := lib/Runtime.class
LLVM_RUNTIME := lib/runtime.bc

PDFLATEX := pdflatex -interaction=batchmode

all: $(JVM_RUNTIME) $(LLVM_RUNTIME)
	$(MAKE) -C src/ all
	ln -sf ./jvmmc_jvm ./latc
	ln -sf ./jvmmc_llvm ./latc_llvm

$(JVM_RUNTIME): %.class: %.java
	javac $<

$(LLVM_RUNTIME): %.bc: %.c
	clang -W -Wall -Wextra -pedantic -std=c99 -S -emit-llvm -o $*.ll $<
	llvm-as -o $@ $*.ll

docs: $(DOCS)
$(DOCS): %.pdf : %.md
	pandoc $< -o $@

lint:
	$(MAKE) -C src/ lint

$(TESTSUITES): % : all
	@./$@/test-run.sh -A

clean:
	-$(MAKE) -C src/ clean
	-rm -f $(JVM_RUNTIME)
	-rm -f $(LLVM_RUNTIME) $(LLVM_RUNTIME:.bc=.ll)
	-rm -f {compile,exec}.{err,out}
	-find ./ -path "*test-*" -a \( \
		   -name "*.class" \
		-o -name "*.j" \
		-o -name "*.jar" \
		-o -name "*.ll" \
		-o -name "*.bc" \
		-o -name "*.s" \
		-o -name "a.out" \
		\) -delete

distclean: clean
	-$(MAKE) -C src/ distclean
	-rm -f $(DOCS) ./latc

.PHONY: clean distclean docs lint
