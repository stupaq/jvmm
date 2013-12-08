#!/bin/bash

# configuration
pattern_good="*/good/*.lat"
pattern_bad_check="*/bad/*.lat"
pattern_bad_parse="*/bad/*.txt"

compile_jvm="./jvmmc_jvm"
compile_jvm_opts="-p"
compile_check="./jvmmc_check"
compile_parse="./jvmmc_parse"

run_jvm="java -jar"

env_memlimit=60000
env_verbosity=1

compile_output="test.err"
exec_output="test.out"

command_find="find $tests_root ! -name *.output -a ! -name *.input -a -type f"

# test cases
tests_good=`$command_find -path "$pattern_good" | sort | uniq`
tests_bad_check=`$command_find -path "$pattern_bad_check" | sort | uniq`
tests_bad_exec=`$command_find -path "$pattern_bad_parse" | sort | uniq`

# assertions
function fatal() {
    echo "FATAL: $1"
    exit 1
}

function negate() {
    [ $1 -ne 0 ]
    return $?
}

function assert_status() {
    if [[ $1 -ne 0 ]]; then
      tput setaf 1
      echo -ne "\t[FAILED]"
    else
      tput setaf 2
      echo -ne "\t[OK]"
    fi
    tput sgr0
    return $1;
}

function assert_compile() {
    diff <(head -n1 $compile_output) <(echo -ne "$1\n") &>/dev/null
    assert_status $?
}

function assert_exec() {
    diff $exec_output "$1" &>/dev/null
    assert_status $?
}

function test_result() {
    echo
    return $1
}

# reporting
function show_results() {
    if [[ $2 == -*c* ]] || [[ $2 == -*v* ]]; then
        echo -e ":: COMPILER:"
        cat "$compile_output" 2>/dev/null
    fi
    if [[ $2 == -*o* ]] || [[ $2 == -*v* ]]; then
        echo -e ":: OUTPUT (actual):"
        cat "$exec_output" 2>/dev/null
        echo -e ":: OUTPUT (expected):"
        cat "${1%.*}.output" 2>/dev/null
    fi
    if [[ $2 == -*i* ]] || [[ $2 == -*v* ]]; then
        echo -e ":: INPUT:"
        cat "${1%.*}.input" 2>/dev/null
    fi
    if [[ $2 == -*s* ]] || [[ $2 == -*v* ]]; then
        echo -e ":: SOURCE:"
        cat "$1"
    fi
    if [[ $2 == -*j* ]] || [[ $2 == -*v* ]]; then
        echo -e "\n:: JASMIN:"
        cat "${1%.*}.j" 2>/dev/null
    fi
}

# testers
function testcase_jvm() {
  Input="${1%.*}.input"
  [[ -f $Input ]] || Input=/dev/null
  Output="${1%.*}.output"
  [[ -f $Output ]] || Output=/dev/null

  echo -ne "JVM\t$1: "
  $compile_jvm $1 $compile_jvm_opts &>$compile_output
  $run_jvm ${1%.*}.jar <$Input &>$exec_output
  Status=$?
  if [[ $1 == $pattern_bad_exec ]]; then
    negate $Status
    assert_status $?
    assert_exec $Output
  else
    assert_status $Status
    assert_exec $Output
  fi
  test_result $?
}

function testcase_check() {
  echo -ne "CHECK\t$1: "
  $compile_check $1 2>$compile_output
  Status=$?
  if [[ $1 == $pattern_bad_check ]]; then
    negate $Status
    assert_status $?
    assert_compile ERROR
  else
    assert_status $Status
    assert_compile OK
  fi
  test_result $?
}

function testcase_parse() {
  echo -ne "PARSE\t$1: "
  $compile_parse $1 2>$compile_output
  Status=$?
  if [[ $1 == $pattern_bad_parse ]]; then
    negate $Status
    assert_status $?
    assert_compile ERROR
  else
    assert_status $Status
    assert_compile OK
  fi
  test_result $?
}


# main()
if [[ $# -eq 0 ]]; then
  TotalCount=0
  function incTotal() {
    TotalCount=`expr $TotalCount + 1`
  }
  FailsCount=0
  function incFails() {
    FailsCount=`expr $FailsCount + 1`
  }

  # run all found tests
  for f in $tests_bad_parse; do
    incTotal
    testcase_parse $f || incFails
  done
  for f in $tests_bad_check; do
    incTotal
    testcase_parse $f
    testcase_check $f || incFails
  done
  for f in $tests_good; do
    incTotal
    testcase_parse $f
    testcase_check $f
    testcase_jvm $f || incFails
  done

  echo -e "FAILED: $FailsCount\tTOTAL:  $TotalCount"
  exit $FailsCount
else
  File=
  for f in $1 `$command_find -path "*$1*"`; do
    [[ -f $f ]] && File=$f;
  done
  if [[ -z $File ]]; then
    fatal "cannot find test: $1"
  fi

  # run found test
  testcase_parse $File
  testcase_check $File
  testcase_jvm $File
  show_results $File $2
fi

# EOF
