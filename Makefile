DOCS			:= $(patsubst %.md, %.pdf, $(wildcard docs/*.md))
JVM_RUNTIME		:= lib/Runtime.class
LLVM_RUNTIME	:= lib/runtime.bc
JASMIN_JAR		:= lib/jasmin.jar
TEST_SUITES		:= run-test-jvmm run-test-latte run-test-mrjp
TEST_RUNNER		:= test-run.sh

PDFLATEX := pdflatex -interaction=batchmode

all: $(JVM_RUNTIME) $(LLVM_RUNTIME) $(JASMIN_JAR)
	$(MAKE) -C src/ all
	ln -sf ./jvmmc_jvm ./latc
	ln -sf ./jvmmc_llvm ./latc_llvm

$(JVM_RUNTIME): %.class: %.java
	javac $<

$(LLVM_RUNTIME): %.bc: %.c
	clang -pedantic -std=c99 -S -emit-llvm \
		-W -Wall -Wextra -Wno-implicit-function-declaration \
		-o $*.ll $<
	llvm-as -o $@ $*.ll

docs: $(DOCS)
$(DOCS): %.pdf : %.md
	pandoc $< -o $@

lint:
	$(MAKE) -C src/ lint

$(TEST_SUITES): run-% : % all
	@./$</$(TEST_RUNNER) -A

test-jvmm:
test-mrjp:
test-latte:
	mkdir $@
	curl 'http://www.mimuw.edu.pl/%7Eben/Zajecia/Mrj2012/Latte/lattests121017.tgz' | tar -xzf - -C $@ --strip-components=1
	@echo '#!/bin/bash' > $@/$(TEST_RUNNER)
	@echo 'tests_root="./test-latte/"' >> $@/$(TEST_RUNNER)
	@echo '. generic-test-runner.sh' >> $@/$(TEST_RUNNER)
	chmod +x $@/$(TEST_RUNNER)
	for d in $@/extensions/*/; do mkdir $$d/good/; mv $$d/*.{lat,output} $$d/good/; done
	for f in $@/bad/bad00{1,2,4,5}; do mv $$f.lat $$f.txt; done

$(JASMIN_JAR):
	curl 'http://heanet.dl.sourceforge.net/project/jasmin/jasmin/jasmin-2.4/jasmin-2.4.zip' -o 'jasmin-2.4.zip'
	unzip -q jasmin*.zip
	mv jasmin*/jasmin.jar $(JASMIN_JAR)
	rm -rf jasmin*

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
	-rm -rf test-latte
	-rm -rf $(JASMIN_JAR)

.PHONY: clean distclean docs lint
