jvmm - a Java-like interpreted and compiled language
====================================================

INTRODUCTION
------------
The goal of this project is to develop Java-like programming language
environment (interpreter and compiler) with full support for object-oriented
design and easily swappable backend. Project priorities are state-of-art
interpreter/compiler design and robust implementation in Haskell.

USAGE
-----
Build-time requirements
+ `ghc`
+ `ghc-mtl`
+ `bnfc`

In order to compile and test interpreter
```
make
make test-examples
./run_examples.sh <example name>
```

You can run an arbitrary code in interpreter
```
./interpreter <source file>
```

SOURCES
-------
Many of test programs come from test packages freely available at:
+ http://www.mimuw.edu.pl/~ben/Zajecia/Mrj2012/Latte/
+ https://github.com/tomwys/mrjp-tests
