JVMM - a Java-like interpreted and compiled language
====================================================

INTRODUCTION
------------
The goal of this project is to develop Java-like programming language environment (interpreter and
compiler) that follows compiler design best practices.

You can find detailed description of the language and BNF grammar under `docs/` subdirectory.

QUICK START
-----------
Build-time requirements include `haskell-platform`, `bnfc`, `pandoc` and `pretty-show` plus some
unix tools that you should already have.

In order to compile JVMM run `make all`, if you wish to render documentation in pdf format run `make
all docs`.

JVMM comes with three test suites: `test-jvmm`, `test-latte` and `test-mrjp`, each containing
multiple test cases.
To test lexer, parser and semantic analyser (more to come) run `make <test_suite>` where
`<test_suite>` is a name of test suite.
If a test suite uses generic test runner, you can also run a single test with
`./<test_suite>/test-run.sh <test_case>` or even append `-v` to output more debug information, e.g.
test case code, expected output, test input.

SOURCES
-------
Test programs under `test-latte` come from official package:
http://www.mimuw.edu.pl/~ben/Zajecia/Mrj2012/Latte/.

Test programs under `test-mrjp` and partially `test-jvmm` come from:
https://github.com/tomwys/mrjp-tests.
