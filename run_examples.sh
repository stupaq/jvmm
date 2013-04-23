#!/bin/bash

PROGEXT=".jv"
EXEC="./interpreter"
PARSE="./Syntax/TestJvmm"
MEMLIMIT=60000

GOOD=`find ./examples_good/ -name "*$PROGEXT" | sort`
BAD_EXEC=`find ./examples_bad/ -name "*$PROGEXT" | sort`
BAD_PARSE=`find ./examples_bad/ -name "*.txt" | sort`

OUT="test.out"
ERR="test.err"
VERB=1

case $2 in
  -v) VERB=3 ;;
  -q) VERB=0 ;;
  -vd) VERB=3; EXEC="$EXEC +RTS -xc -RTS" ;;
  -qd) VERB=0; EXEC="$EXEC +RTS -xc -RTS" ;;
esac

function cleanup() {
  rm -f $OUT $ERR
}

function neg() {
  status=$1; [[ $status -ne 0 ]] && return 0 || return 1;
}

function check() {
  status=$1;
  if [[ $status -ne 0 ]]; then 
    tput setaf 1;
    echo -e "\t[FAILED]";
  else
    tput setaf 2;
    echo -e "\t[OK]";
  fi
  tput sgr0;
  return $status;
}

function show() {
  echo "ERROR:"
  cat $ERR
  echo "OUTPUT:"
  cat $OUT
  if [[ $VERB -ge 1 ]]; then
    echo "EXPECTED:"
    cat "${1%$PROGEXT}.output"
    echo "CODE:"
    cat $1
  fi
}

function fatal() {
  echo "FATAL: $1"
  exit 1
}

function run_example() {
  input="${1%$PROGEXT}.input"
  [[ -f $input ]] || input=/dev/null
  output="${1%$PROGEXT}.output"
  [[ -f $output ]] || output=/dev/null

  echo -ne "TEST\t$1: "
  ulimit -Sv $MEMLIMIT
  $EXEC $1 <$input 1>$OUT 2>$ERR && diff $OUT $output &>/dev/null
  status=$?
  if [[ $1 == *examples_good* ]]; then
    check $status
  else
    neg $status
    check $?
  fi
}

function parse_example() {
  echo -ne "PARSE\t$1: "
  $PARSE $1 | grep "Parse Successful!" &>/dev/null
  status=$?
  if [[ $1 == *.jv ]]; then
    check $status
  else
    neg $status
    check $?
  fi
}


if [[ $# -eq 0 ]]; then
  fail=0
  for f in ${GOOD}; do
    parse_example $f
    run_example $f || fail=`expr $fail + 1`
  done
  for f in ${BAD_EXEC}; do
    parse_example $f
    run_example $f || fail=`expr $fail + 1`
  done
  for f in ${BAD_PARSE}; do
    parse_example $f || fail=`expr $fail + 1`
  done
  echo "TOTAL FAILED: $fail"
  exit $fail
else
  for f in $1 "./examples_good/${1%.*}$PROGEXT" "./examples_bad/${1%.*}$PROGEXT"; do
    [[ -f $f ]] && file=$f;
  done
  [[ -z $file ]] && fatal "cannot find test: $1"

  parse_example $file
  run_example $file
  if [[ $? -ne 0 ]] || [[ $2 == -v* ]]; then
    show $file
  fi
fi
