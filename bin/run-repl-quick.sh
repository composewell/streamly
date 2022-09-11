#!/usr/bin/env bash

# XXX Having a repl-ish option on packcheck can be very useful for quick type
# checking. Running that in a loop in a smart way will work like ghcid.

# The idea is to have an extremely light version of run-ci.sh
# This can be very helpful on any refactoring change.

# This script should be majorly improved but it solves my use case for the time
# being.

set -e

SCRIPT_DIR=$(dirname $0)

STREAMLY_VERSION=0.8.3
BENCH_REPORT_DIR=benchmark/bench-report
source $SCRIPT_DIR/targets.sh

#------------------------------------------------------------------------------
# Script
#------------------------------------------------------------------------------

print_help () {
  echo "Usage: $0"
  echo
  echo "Very dumb script."
  echo "Currently runs everything in the repl and exits."
  echo "Useful for type checking and displaying warnings."
  echo
  exit
}

#------------------------------------------------------------------------------
# Helper functions
#------------------------------------------------------------------------------

run_build_repl () {
  local prefix=$1
  local targets=$2
  local COMPONENTS
  local c

  for c in $targets
  do
    CABAL_BUILD_OPTIONS=$(dev_build "--flag dev")
    echo "|--------------------------------------------------------------------"
    echo "|" $CABAL_EXECUTABLE repl "$prefix:$c" $CABAL_BUILD_OPTIONS <<< :q
    echo "|--------------------------------------------------------------------"
    run_verbose $CABAL_EXECUTABLE repl "$prefix:$c" $CABAL_BUILD_OPTIONS <<< :q
  done
}

#------------------------------------------------------------------------------
# Setup
#------------------------------------------------------------------------------

source $BENCH_REPORT_DIR/bin/build-lib.sh

USE_GIT_CABAL=0
set_common_vars

#------------------------------------------------------------------------------
# Library
#------------------------------------------------------------------------------

# XXX Ideally run for all the valid flag combinations
CABAL_BUILD_OPTIONS=""
run_verbose $CABAL_EXECUTABLE repl $CABAL_BUILD_OPTIONS <<< :q

CABAL_BUILD_OPTIONS="--flag dev"
run_verbose $CABAL_EXECUTABLE repl $CABAL_BUILD_OPTIONS <<< :q

CABAL_BUILD_OPTIONS="--flag streamk"
run_verbose $CABAL_EXECUTABLE repl $CABAL_BUILD_OPTIONS <<< :q

CABAL_BUILD_OPTIONS="--flag use-c-malloc"
run_verbose $CABAL_EXECUTABLE repl $CABAL_BUILD_OPTIONS <<< :q

#------------------------------------------------------------------------------
# Benchmarks
#------------------------------------------------------------------------------

RUNNING_BENCHMARKS=y
RUNNING_TESTS=

RUNNING_DEVBUILD=
targets
TARGETS="$(all_grp)"
run_build_repl "bench" "$TARGETS"


RUNNING_DEVBUILD=y
targets
TARGETS="$(all_grp)"
run_build_repl "bench" "$TARGETS"

#------------------------------------------------------------------------------
# Tests
#------------------------------------------------------------------------------

RUNNING_TESTS=y
RUNNING_BENCHMARKS=

RUNNING_DEVBUILD=
targets
TARGETS="$(all_grp)"
run_build_repl "test" "$TARGETS"

RUNNING_DEVBUILD=y
targets
TARGETS="$(all_grp)"
run_build_repl "test" "$TARGETS"
