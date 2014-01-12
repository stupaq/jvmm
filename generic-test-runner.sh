#!/bin/bash

# configuration
pattern_good="*/good/*.lat"
pattern_bad_exec="*/errors/*.lat"
pattern_bad_check="*/bad/*.lat"
pattern_bad_parse="*/bad/*.txt"

compile_llvm="./jvmmc_llvm"
compile_jvm="./jvmmc_jvm"
compile_jvm_opts="-p"
compile_check="./jvmmc_check"
compile_parse="./jvmmc_parse"

run_interpreter="./jvmmi "
run_jvm="java -jar"

env_memlimit=60000
env_verbosity=1

compile_output="compile.err"
exec_output="exec.out"
exec_error="exec.err"

command_find="find $tests_root -type f \
        ! -name *.output \
    -a  ! -name *.input \
    -a  ! -name *.class \
    -a  ! -name *.j \
    -a  ! -name *.jar \
    -a  ! -name *.ll \
    -a  ! -name *.s \
    -a  ! -name *.bc \
    "

# test cases
tests_exec=`$command_find -path "$pattern_good" | sort | uniq`
tests_check=`$command_find -path "$pattern_bad_check" | sort | uniq`
tests_parse=`$command_find -path "$pattern_bad_parse" | sort | uniq`

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

TotalCount=0
FailsCount=0
function test_result() {
    echo
    TotalCount=`expr $TotalCount + 1`
    if [[ $1 -ne 0 ]]; then
      FailsCount=`expr $FailsCount + 1`
    fi
    return $1
}

function test_prepare() {
    rm -f $compile_output $exec_output $exec_error
}

# reporting
function show_results() {
    if [[ $2 == -*c* || $2 == -*v* ]]; then
        echo -e ":: COMPILER:"
        cat "$compile_output" 2>/dev/null
    fi
    if [[ $2 == -*o* || $2 == -*v* ]]; then
        echo -e ":: OUTPUT (difference):"
        diff "$exec_output" "${1%.*}.output"
    fi
    if [[ $2 == -*oo* || $2 == -*v* ]]; then
        echo -e ":: OUTPUT (actual):"
        cat "$exec_output" 2>/dev/null
        echo -e ":: OUTPUT (expected):"
        cat "${1%.*}.output" 2>/dev/null
    fi
    if [[ $2 == -*i* || $2 == -*v* ]]; then
        echo -e ":: INPUT:"
        cat "${1%.*}.input" 2>/dev/null
    fi
    if [[ $2 == -*s* || $2 == -*v* ]]; then
        echo -e ":: SOURCE:"
        cat "$1"
    fi
    if [[ $2 == -*j* || $2 == -*v* ]]; then
        echo -e "\n:: JASMIN:"
        cat "${1%.*}.j" 2>/dev/null
    fi
    if [[ $2 == -*l* || $2 == -*v* ]]; then
        echo -e "\n:: LLVM:"
        cat "${1%.*}.ll" 2>/dev/null
    fi
}

# testers
function testcase_llvm() {
  [[ $2 == *L* || $2 == *A* ]] || return 0
  test_prepare

  Input="${1%.*}.input"
  [[ -f $Input ]] || Input=/dev/null
  Output="${1%.*}.output"
  [[ -f $Output ]] || Output=/dev/null

  echo -ne "LLVM\t$1: "
  $compile_llvm $1 &>$compile_output
  Status=$?
  if [[ $Status -eq 0 ]]; then
    `dirname $1`/a.out <$Input 1>$exec_output 2>$exec_error
    Status=$?
  fi
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

function testcase_interpreter() {
  [[ $2 == *I* || $2 == *A* ]] || return 0
  test_prepare

  Input="${1%.*}.input"
  [[ -f $Input ]] || Input=/dev/null
  Output="${1%.*}.output"
  [[ -f $Output ]] || Output=/dev/null

  echo -ne "INTERP\t$1: "
  (ulimit -Sv $env_memlimit; $run_interpreter $1 <$Input 1>$exec_output 2>$exec_error)
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

function testcase_jvm() {
  [[ $2 == *J* || $2 == *A* ]] || return 0
  test_prepare

  Input="${1%.*}.input"
  [[ -f $Input ]] || Input=/dev/null
  Output="${1%.*}.output"
  [[ -f $Output ]] || Output=/dev/null

  echo -ne "JVM\t$1: "
  $compile_jvm $1 $compile_jvm_opts &>$compile_output
  Status=$?
  if [[ $Status -eq 0 ]]; then
    $run_jvm ${1%.*}.jar <$Input 1>$exec_output 2>$exec_error
    Status=$?
  fi
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
  [[ $2 == *C* || $2 == *A* ]] || return 0
  test_prepare

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
  [[ $2 == *P* || $2 == *A* ]] || return 0
  test_prepare

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
if [[ $# -eq 0 || $1 == -* ]]; then
  # run all found tests
  for f in $tests_parse; do
    testcase_parse $f $1
  done
  for f in $tests_check; do
    testcase_parse $f $1
    testcase_check $f $1
  done
  for f in $tests_exec; do
    testcase_parse $f $1
    testcase_check $f $1
    testcase_jvm $f $1
    testcase_interpreter $f $1
    testcase_llvm $f $1
  done

  echo -e "FAILED: $FailsCount\tTOTAL:  $TotalCount"
  exit $FailsCount
else
  # find test
  File=
  for f in $1 `$command_find -path "*$1*"`; do
    [[ -f $f ]] && File=$f;
  done
  if [[ -z $File ]]; then
    fatal "cannot find test: $1"
  fi

  # run found test
  testcase_parse $File $2
  testcase_check $File $2
  testcase_jvm $File $2
  testcase_interpreter $File $2
  testcase_llvm $File $2
  show_results $File $2
fi

# EOF
