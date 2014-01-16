JVMM - a Java-like interpreted and compiled language
====================================================

INTRODUCTION
------------
The goal of this project is to develop Java-like programming language environment (interpreter and compiler) that
follows compiler design best practices.

You can find detailed description of the language and BNF grammar under `docs/` subdirectory.

QUICK START
-----------
Build-time requirements include `haskell-platform` as well as several Haskell packages `bnfc`, `pandoc`, `pretty-show`,
`llvm-general-pure` and `llvm-general`.
You can easily obtain them with `cabal install <pkg_name>`, be sure to upgrade Cabal itself beforehand with `cabal
install cabal`.
You will also need some Unix tools that you should already have.

In order to compile JVMM run `make all`, if you wish to render documentation in PDF format run `make docs`.
In project's root folder you can find scripts which run different parts of the JVMM stack, namely:
+   `./jvmmc_parse <source_file>` - parses provided source and shows any syntactic errors
+   `./jvmmc_check <source_file>` - performs static analysis (type checking and more) and shows any errors
+   `./jvmmc_jvm <source_file>` - emits Jasmin assembler, compiles it into .class file and packages into JAR
    together with JVMM runtime library
+   `./jvmmc_llvm <source_file>` - emits LLVM IR, compiles it into native binary
+   `./jvmmi <source_file>` - interprets program in the same process that performed parsing and static analysis

RUNNING TEST SUITES
-------------------
JVMM comes with three test suites: `test-jvmm`, `test-latte` and `test-mrjp`, each containing multiple test cases.
To test lexer, parser and semantic analyser (more to come) run `make <test_suite>` where `<test_suite>` is a name of
test suite.

If a test suite uses generic test runner, you can also run a single test with `./<test_suite>/test-run.sh <test_case>
-A` or even append `-Av` to output more debug information, e.g. test case code, expected output, test input. There are
many more options to each `test-run.sh` script, explore `generic-test-runner.sh` for more info.
For example to test interpreter back-end only use `-I` or `-Iv` respectively.

SOURCES AND ACKNOWLEDGEMENTS
----------------------------
Test programs under `test-latte/` come from official package: http://www.mimuw.edu.pl/~ben/Zajecia/Mrj2012/Latte/.

Test programs under `test-mrjp/` come from: https://github.com/tomwys/mrjp-tests.

Jasmin binary is provided in this package
