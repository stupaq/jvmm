#!/bin/bash

EXT=.jv
RUN=./interpreter
PARSE=./Syntax/TestJvmm
OUT=test.out
ERR=test.err
GOOD=./examples_good/
MEMLIMIT=50000

function cleanup() {
  rm -f $OUT $ERR
}

function neg() {
  status=$?; [[ $status -ne 0 ]] && return 0 || return 1;
}

function check() {
  status=$?;
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
  if [[ $# -eq 1 ]]; then
    echo "EXPECTED:"
    cat "${1%$EXT}.output"
    echo "CODE:"
    cat $1
  fi
}

function fatal() {
  echo "FATAL: $1"
  exit 1
}

function run_example() {
  input="${1%$EXT}.input"
  [[ -f $input ]] || input=/dev/null
  output="${1%$EXT}.output"
  [[ -f $output ]] || output=/dev/null

  echo -ne "TEST\t$1: "
  ulimit -Sv $MEMLIMIT
  $RUN $1 <$input 1>$OUT 2>$ERR && diff $OUT $output &>/dev/null
}

function parse_example() {
  echo -ne "PARSE\t$1: "
  $PARSE $1 | grep -q "Parse Successful!"
}

if [[ $# -eq 0 ]]; then
  fail=0
  for f in ${GOOD}*$EXT; do
    parse_example $f; check;
    run_example $f; check || fail=`expr $fail + 1`
  done
  echo "TOTAL FAILED: $fail"
  exit $fail
else
  file=
  [[ -f $1 ]] && file=$1
  [[ -f ${GOOD}${1}${EXT} ]] && file=${GOOD}${1}${EXT}
  [[ -f ${GOOD}${1} ]] && file=${GOOD}${1}
  
  [[ -z $file ]] && fatal "Can't find file $1."
  run_example $file; check;
  if [[ $2 == -v* ]]; then show $file; else show; fi
fi
