*Jvmm* : Java-like, minimalistic, imperative, interpreted language
==================================================================

Introduction
------------
Language *Jvmm* (Java - -) is an imperative, interpreted programming language
with strong, static typing. It is intended to be a (proper) subset of Java
language (but it is not, since in Java everything is a class and there is no
such concept as a class in *Jvmm* (yet)).

Program structure
-----------------
Program written in *Jvmm* is a list of function definitions. Each function is
defined by return value type, arguments list (each argument with specified
type) and function body. Functions cannot be redefined in global scope, but
they can be used before declaration. Each program must define _main()_ function
with return type _int_ and no arguments, which is an entry point of a program.
Function with type other than _void_ *must* explicitly return value (of a
corresponding type).

Passing style and memory management
-----------------------------------
Arguments are *always* passed to and returned from a function by value.
Objects of some types (as described below) are only accessed by a reference
and a thing that is passed to or returned from a function (therefore copied) is
the reference. For such objects, when the last reference referring the object
falls out of its scope the object is disposed automatically.
A reference can have a value _null_, which means it does not refer to any object.
Note that for current language specification reference-counting is sufficient
for garbage collection, as there is no way to create a cycle of references.

Statements
----------
Instructions: _if_, _while_, _for_, _return_, _;_, _++_, _--_, _=_, _+=_, _-=_,
_*=_, _/=_, _%=_, have Java-like semantics. Operator precedence is described in
attached BNF grammar.

The only l-values are variables, references and subscripted arrays (in case one
wants to assign a value to some cell in an array). Note that assignment to a
reference changes which object is referenced, not underlying object itself (and
may or may not change reference count for underlying object).

Declarations
------------
Variables and references are bound statically and can be declared anywhere
inside a block, but usage of undeclared variable is forbidden. Declared and
uninitialized variables are automatically initialized with default values, that
is _int_ = 0, _boolean_ = false, _String_ = "", _char_ = 'J', uninitialized
references are initialized with _null_.

Scope of a local variable is limited to the block containing its declaration,
variables from outer blocks (and global scope) can be hidden by local variable
declaration, but names inside one block must be unique.

Types
-----
There is no implicit cast at all! Primitive types _int_, _boolean_, _char_ are
defined as in Java, with exception that _char_ is limited as described in
attached grammar.

_String_ is an immutable character sequence accessible by a reference.
For convenience _string_ is an alias for _String_. Concatenation operator _+_
is overloaded for strings. One can access (read-only) characters of a string
using member-like function _s.charAt(int i)_, positions in string are indexed
from 0. Length of a string _s_ can be obtained using member-like notation
_s.length_.

An array is a sequence of variables of primitive type and is accessible through
a reference. Arrays must be created explicitly using _new[int len]_ operator.
Length of an array (of type _int_) is provided on creation and cannot be
altered, one can access length of an array _a_ using member-like notation
_a.length_. Elements of an array are indexed starting from 0 and accessed by
_[]_ operator.

Expressions
-----------
Arithmetic and logical expressions have natural semantics. Logical expressions
are evaluated lazily and have _boolean_ type, for other expressions we have
greedy evaluation. There is no implicit conversion between _boolean_ and _int_.

Built-in functions
------------------
- _void print(String s)_ -- prints given string to stdout
- _void printLine(String s)_ -- prints given string and a new line character to stdout
- _String readLine()_ -- reads and returns one line from stdin (without terminal newline character)

Example code
------------

```{.java}
// Example program in Jvmm

int[] foo(int n) {
  if (n > 0) {
    int[] arr = new int[n];
    while (n > 0) {
      n--;
      arr[n] = comp(n);
    }
    return arr;
  } else
    return null;
}

int bar(int[] arr) {
  if (arr == null)
    return 0;
  int s; // equivalent to int s = 0;
  for (int a : arr) {
    s += a;
  }
  return s;
}

/* multi
   line
   comment */
int main () {
  printLine("Your name: ");
  String s = readLine();
  printLine("Hello " + s + "!");
  int c = bar(foo(5));
  if (s.length > 0 && s.charAt(0) == 'M')
    print("OK");
  else
    print("nope");
  return c + c*c - 1;
}

int comp(int a) {
  return 2 *a -1;
}
```

Optional extensions
-------------------
Here are some possible extensions to core *Jvmm*, which can be implemented if
really needed. Sorted from the most preferred.

### Exceptions
Exceptions of type _String_. Exception can be thrown with _throw s;_ (where _s_
has a _String_ type) instruction and is caught by first _try {} catch(String e)
{}_ block reached when going up the stack (or block structure). Reference
declared in _catch_ is assigned with a value of caught exception and execution
of following block continues. Above definition is very informal, exceptions
should work like in Java, but there is no finally, nor multiple-catch
constructions.

### Structures
C-like structures (fields only, no methods) accessed by a reference. Allocated
using _new_ operator.

Sources & bibliography
----------------------
- [LBNF Java 1.5 Grammar, example .cf file in
  bnfc-cabal](https://bnfc-cabal.googlecode.com/svn-history/r2/trunk/Examples/java.cf)
- [Przykładowe programy w języku "Latte", projekt zaliczeniowy MRJP na wydziale
  MIM UW](http://www.mimuw.edu.pl/~ben/Zajecia/Mrj2012/Latte/)
- [Lecture 4: Implementing Lexers and Parsers Programming Languages
  Course](http://www.cse.chalmers.se/edu/year/2011/course/TIN321/lectures/proglang-04.html)

