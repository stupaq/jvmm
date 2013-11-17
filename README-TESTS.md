REMARKS ABOUT PROVIDED TEST SUITES
----------------------------------
Test suites `test-mrjp` and `test-jvmm` are meant as a progress indicators right now, rather than
actual correctness evaluation, i.e. they do not need to succeed.

Since language extensions are graded in the last stage of the project, some of them are deliberately
disabled as they are not finished or not officially recognized.
Tests that are relevant to the current implementation are located in good/, bad/ or extension/
directories as requested.

Test cases that MUST FAIL in current implementation:
+ test-jvmm (the following tests fail due to disabled exceptions syntax)
  - exceptions001
  - exceptions002
  - gchiddenstack
+ test-mrjp (the following exceptions fail due to different member access syntax)
  - polymorphism
  - lista2Kierunkowa

All other test cases MUST PASS with current implementation.
