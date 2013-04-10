*jvmm* : Java-like, minimalistic, imperative, interpreted language
==================================================================

Introduction
------------
Language *jvmm* (Java - -) is an imperative, interpreted programming language
with strong typing. It is intended to be a (proper) subset of Java language
(but it is not, since in Java everything is a class and there is no such
concept as a class in *jvmm* (yet)).

Program structure
-----------------
Program written in *jvmm* is a list of function definitions. Each function is
defined by return value type, arguments list (each argument with specified
type) and function body. Functions cannot be redefined in global scope. Each
program must define _main()_ function with return type _int_ and no arguments,
which is an entry point of a program. Function can be used before declaration
as long as it is declared in global scope. Function with type other than _void_
*must* explicitly return value (of a corresponding type). Arguments are passed
by value (as in Java).

Statements
----------
Instructions: _if_, _while_, _for_ (foreach), _return_, _;_, _++_, _--_, _=_
have natural semantics. The only l-values are variables.

Declarations
------------
Variables are bound statically and can be declared anywhere inside a block, but
usage of undeclared variables is forbidden. Declared and uninitialized
variables are automatically initialized with default values, that is _int_ = 0,
_bool_ = false, _String_ = "", _char_ = '\\0'.
Scope of a local variable is limited to the block containing its declaration,
variables from outer blocks (and global scope) can be hidden by local variable
declaration, but names inside one block must be unique.

Types
-----
There is no implicit cast! Types _int_, _bool_, _void_, _char_ are defined as
in Java.
_String_ is an immutable _char_ sequence and can exist as a literal or
variable, can be passed to and returned from a function. Concatenation operator
_+_ is overloaded for strings.
An array (as in Java) is actually a reference, which can be passed to and
returned from function. Arrays must be created explicitly using _new_ operator.
Length of an array (of type _int_) is provided on creation, cannot be altered
and can be obtained as a _length_ attribute (using dot-notation). Memory
allocated for an array must be deallocated using _delete_ operator.

Expressions
-----------
Arithmetic and logical expressions have natural semantics. Logical expressions
are evaluated lazily and have _bool_ type. There if no implicit conversion
between _bool_ and _int_.

Exceptions
----------
An exception in *jvmm* has a _String_ type. It can be thrown with _throw s;_ (s
is of type _String_) instruction and is catched by first _try {} catch(String
e) {}_ block reached when going up the stack (or block structure). Variable
declared in _catch_ is assigned with a value of catched exception and execution
of following block continues. Above definition is very informal, exceptions
should work like in Java, but there is no finally, nor multiple-catch
constructions.

Built-in functions
------------------
- _void printString(String s)_ -- prints given string to stdout
- _String readLine()_ -- reads and returns one line from stdin

Sources & bibliography
----------------------
- [Gramatyka języka 'Latte'](http://www.mimuw.edu.pl/~ben/Zajecia/Mrj2012/Latte/Latte.cf)

TODO:

- char
- catch
- string -> String
- array and new/delete operator
- for loop
- consider garbage collection
