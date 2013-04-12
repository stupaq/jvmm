*Jvmm* : Java-like, minimalistic, imperative, interpreted language
==================================================================

Introduction
------------
Language *Jvmm* (Java - -) is an imperative, interpreted programming language
with strong typing. It is intended to be a (proper) subset of Java language
(but it is not, since in Java everything is a class and there is no such
concept as a class in *Jvmm* (yet)).

Program structure
-----------------
Program written in *Jvmm* is a list of function definitions. Each function is
defined by return value type, arguments list (each argument with specified
type) and function body. Functions cannot be redefined in global scope. Each
program must define _main()_ function with return type _int_ and no arguments,
which is an entry point of a program. Function can be used before declaration
as long as it is declared in global scope. Function with type other than _void_
*must* explicitly return value (of a corresponding type). Arguments are passed
and returned by value (as in Java). Some types (as described below) are always
accessed by a reference and a thing that is passed to or returned from a
function is the reference.

Statements
----------
Instructions: _if_, _while_, _for_, _return_, _;_, _++_, _--_, _=_, have
natural semantics. Operator precedence is almost compatible with those of Java
and is described in included BNF grammar.
The only l-values are variables and subscripted arrays (if one wants to assign
a value to some cell in an array).

Declarations
------------
Variables are bound statically and can be declared anywhere inside a block, but
usage of undeclared variable is forbidden. Declared and uninitialized
variables are automatically initialized with default values, that is _int_ = 0,
_boolean_ = false, _String_ = "", _char_ = 'x'.
Scope of a local variable is limited to the block containing its declaration,
variables from outer blocks (and global scope) can be hidden by local variable
declaration, but names inside one block must be unique.

Types
-----
There is no implicit casts! Types _int_, _boolean_, _void_, _char_ are defined
as in Java.
_String_ is an immutable _char_ sequence and can exist as a literal or
variable, can be passed to and returned from a function. Concatenation operator
_+_ is overloaded for strings. One can access (read-only) characters of a
string using member-like function _String.charAt(int i)_.
An array (as in Java) is actually a reference, which can be passed to and
returned from a function. Arrays must be created explicitly using _new[int
len]_ operator. Length of an array (of type _int_) is provided on creation and
cannot be altered and can be obtained as a _length_ attribute (using
dot-notation).
Both strings and arrays are indexed starting from 0.
Memory allocated for an array using _new[int len]_ operator is managed
automatically.

Expressions
-----------
Arithmetic and logical expressions have natural semantics. Logical expressions
are evaluated lazily and have _boolean_ type. There is no implicit conversion
between _boolean_ and _int_.

Exceptions
----------
An exception in *Jvmm* has a _String_ type. It can be thrown with _throw s;_
(where s has _String_ type) instruction and is caught by first _try {}
catch(String e) {}_ block reached when going up the stack (or block structure).
Variable declared in _catch_ is assigned with a value of caught exception and
execution of following block continues. Above definition is very informal,
exceptions should work like in Java, but there is no finally, nor
multiple-catch constructions.

Built-in functions
------------------
- _void printString(String s)_ -- prints given string to stdout
- _String readLine()_ -- reads and returns one line from stdin

Sources & bibliography
----------------------
- [LBNF Java 1.5 Grammar, example .cf file in bnfc-cabal](https://bnfc-cabal.googlecode.com/svn-history/r2/trunk/Examples/java.cf)

