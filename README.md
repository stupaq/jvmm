jvmm - a Java-like interpreted and compiled language
====================================================

INTRODUCTION
------------
The goal of this project is to develop Java-like programming language environment (interpreter and
compiler) with full support for object-oriented design and easily swappable backend.

USAGE
-----
Build-time requirements:
+ `ghc`
+ `ghc-mtl`
+ `bnfc`

In order to compile `jvmm`:
```
make
```

You can run entire test suite or a single test case using:
```
make test-latte
./test-latte/test-run.sh <example name>
```

SOURCES
-------
Test programs under `test-latte` come from official package: http://www.mimuw.edu.pl/~ben/Zajecia/Mrj2012/Latte/
Test programs under `test-mrjp` and partially `test-jvmm` come from: https://github.com/tomwys/mrjp-tests
