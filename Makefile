DOCS := $(patsubst %.md, %.pdf, $(wildcard docs/*.md))
TESTSUITES := $(wildcard test-*)
JVM_RUNTIME := lib/Runtime.class
LLVM_RUNTIME := lib/runtime.bc
DEFAULT := jvm

PDFLATEX := pdflatex -interaction=batchmode

all: $(JVM_RUNTIME) $(LLVM_RUNTIME)
	$(MAKE) -C src/ all
	ln -sf ./jvmmc_$(DEFAULT) ./latc

$(JVM_RUNTIME): %.class: %.java
	javac $<

$(LLVM_RUNTIME): %.bc: %.ll
	llvm-as -o $@ $<

docs: $(DOCS)
$(DOCS): %.pdf : %.md
	pandoc $< -o $@

$(TESTSUITES): % : all
	@./$@/test-run.sh -A

clean:
	-$(MAKE) -C src/ clean
	-rm -f $(JVM_RUNTIME)
	-rm -f $(LLVM_RUNTIME)
	-rm -f {compile,exec}.{err,out}
	-find ./ -path "*test-*" -a \( -name "*.class" -o -name "*.j" -o -name "*.jar" \) -delete

distclean: clean
	-$(MAKE) -C src/ distclean
	-rm -f $(DOCS) ./latc

.PHONY: clean distclean docs
