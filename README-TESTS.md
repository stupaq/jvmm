REMARKS ABOUT PROVIDED TEST SUITES
----------------------------------
Test suites `test-mrjp` and `test-jvmm` are meant as a progress indicators, rather than actual correctness evaluation,
i.e. they do not need to succeed.

Since language extensions are graded in the last stage of the project, some of them are deliberately disabled as they
are not finished or not officially recognized.
Tests that are relevant to the current implementation are located in good/, bad/ or extension/ directories as requested.

Test cases that MUST FAIL in current implementation:
+   JVM backend
    *   test-latte
        -   counter
        -   linked
        -   points
        -   queue
        -   shapes
        -   list
    *   test-jvmm
        -   dfs
        -   exceptions001
        -   exceptions002
        -   exceptions003
        -   gchiddenstack
        -   non-static_prec
        -   this_semantics
    *   test-mrjp
        -   bfs
        -   calculator
        -   lista2Kierunkowa
        -   mergeSort
        -   tail_call_optimization
        -   polymorphism
+   INTERPRETER backend
    *   test-mrjp
        -   tail_call_optimization

All other test cases MUST PASS with current implementation.
