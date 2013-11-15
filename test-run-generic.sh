#!/bin/bash

# default configuration
PARSEEXT=".txt"

# what to run
EXEC="./latc"
PARSE="./latc -p"

# memory limit (gc testing)
MEMLIMIT=60000

# testcase out for debugging
OUT="test.out"
ERR="test.err"

# default verbosity
VERB=1

# test procedure
GOOD=`find "$TESTROOT" -path "*good*" -name "*$EXECEXT" -type f | sort | uniq`
BAD_EXEC=`find "$TESTROOT" -path "*bad*" -name "*$EXECEXT" -type f | sort | uniq`
BAD_PARSE=`find "$TESTROOT" -path "*bad*" -name "*$PARSEEXT" -type f | sort | uniq`

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
    cat "${1%$EXECEXT}.output"
    echo "CODE:"
    cat $1
  fi
}

function fatal() {
  echo "FATAL: $1"
  exit 1
}

function run_example() {
  input="${1%$EXECEXT}.input"
  [[ -f $input ]] || input=/dev/null
  output="${1%$EXECEXT}.output"
  [[ -f $output ]] || output=/dev/null

  echo -ne "TEST\t$1: "
  ulimit -Sv $MEMLIMIT
  $EXEC $1 <$input 1>$OUT 2>$ERR && diff $OUT $output &>/dev/null
  status=$?
  if [[ $1 == */good* ]]; then
    check $status
  else
    neg $status
    check $?
  fi
}

function check_example() {
  echo -ne "TEST\t$1: "
  $EXEC $1 1>$OUT 2>$ERR
  status=$?
  if [[ $1 == */good* ]]; then
    check $status
  else
    neg $status
    check $?
  fi
}

function parse_example() {
  echo -ne "PARSE\t$1: "
  $PARSE $1 2>&1 | grep "OK" &>/dev/null
  status=$?
  if [[ $1 == *$EXECEXT ]]; then
    check $status
  else
    neg $status
    check $?
  fi
}


if [[ $# -eq 0 ]]; then
  fail=0

  for f in ${BAD_PARSE}; do
    parse_example $f || fail=`expr $fail + 1`
  done
  for f in ${BAD_EXEC}; do
    parse_example $f
    # TODO this performs compilation only
    check_example $f || fail=`expr $fail + 1`
  done
  for f in ${GOOD}; do
    parse_example $f
    # TODO this performs compilation only
    check_example $f || fail=`expr $fail + 1`
  done

  echo "TOTAL FAILED: $fail"
  exit $fail
else
  for f in $1 `find "$TESTROOT" -name "$1$PARSEEXT" -type f` `find "$TESTROOT" -name "$1$EXECEXT" -type f`; do
    [[ -f $f ]] && file=$f;
  done
  [[ -z $file ]] && fatal "cannot find test: $1"

  parse_example $file
  # TODO this performs compilation only
  check_example $file
  if [[ $? -ne 0 ]] || [[ $2 == -v* ]]; then
    show $file
  fi
fi
