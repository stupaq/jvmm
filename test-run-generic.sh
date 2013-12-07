#!/bin/bash

# configuration
pattern_good="*/good/*.lat"
pattern_bad_check="*/bad/*.lat"
pattern_bad_parse="*/bad/*.txt"

compile_jvm="./latc"
compile_check="./latc_check"
compile_parse="./latc_parse"

env_memlimit=60000
env_verbosity=1

out_stdout="test.out"
out_stderr="test.err"

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

function assert_output() {
    diff <(head -n1 $out_stderr) <(echo -ne "$1\n") &>/dev/null
    assert_status $?
}

function test_result() {
  echo
  return $1
}

# reporting
function show_results() {
  echo ":: STDERR (actual):"
  cat "$out_stderr"
  echo ":: STDOUT (actual):"
  cat "$out_stdout"
  if [[ $verbosity -ge 1 ]]; then
    echo ":: STDOUT (expected):"
    cat "${1%.*}.output"
  fi
  if [[ $verbosity -ge 2 ]]; then
    echo ":: SOURCE:"
    cat "$1"
    echo ":: JASMIN:"
    cat "${1%.*}.j"
  fi
}

# testers
# FIXME make it work once we have the first backend
function run_example() {
input="${1%$EXECEXT}.input"
[[ -f $input ]] || input=/dev/null
output="${1%$EXECEXT}.output"
[[ -f $output ]] || output=/dev/null

echo -ne "TEST\t$1: "
ulimit -Sv $MEMLIMIT
$EXEC $1 <$input 1>$out_stdout 2>$out_stderr && diff $out_stdout $output &>/dev/null
status=$?
if [[ $1 == */good* ]]; then
  assert_status $status
else
  negate $status
  assert_status $?
fi
test_result $?
}

function testcase_check() {
  echo -ne "CHECK\t$1: "
  $compile_check $1 1>$out_stdout 2>$out_stderr
  status=$?
  if [[ $1 == $pattern_bad_check ]]; then
    negate $status
    assert_status $?
    assert_output ERROR
  else
    assert_status $status
    assert_output OK
  fi
  test_result $?
}

function testcase_parse() {
  echo -ne "PARSE\t$1: "
  $compile_parse $1 1>$out_stdout 2>$out_stderr
  status=$?
  if [[ $1 == $pattern_bad_parse ]]; then
    negate $status
    assert_status $?
    assert_output ERROR
  else
    assert_status $status
    assert_output OK
  fi
  test_result $?
}


# main()
case $2 in
  -v) env_verbosity=3 ;;
  -q) env_verbosity=0 ;;
esac

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
    testcase_check $f || incFails
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
  if [[ $? -ne 0 ]] || [[ $2 == -v* ]]; then
    show_results $File
  fi
fi

# EOF
