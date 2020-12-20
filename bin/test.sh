#!/usr/bin/env bash

#------------------------------------------------------------------------------
# Script
#------------------------------------------------------------------------------

SCRIPT_DIR=$(dirname $0)

print_help () {
  echo "Usage: $0 "
  echo "       [--targets <"target1 target2 ..." | help>]"
  echo "       [--slow]"
  echo "       [--quick]"
  echo "       [--cabal-build-options <option>]"
  echo "       -- <hspec options or test names>"
  echo
  echo "--targets: targets to run, use 'help' for list of targets"
  echo "--slow: take longer but run more tests"
  echo "--quick: faster  but run fewer tests"
  echo "--cabal-build-options: Pass any cabal build options to be used for build"
  echo
  echo "Any arguments after a '--' are passed directly to hspec"
  exit
}

#-----------------------------------------------------------------------------
# Read command line
#-----------------------------------------------------------------------------

RUNNING_TESTS=y
source $SCRIPT_DIR/build-lib.sh

set_common_vars

# XXX add a bisect option
while test -n "$1"
do
  case $1 in
    -h|--help|help) print_help ;;
    # options with arguments
    --targets) shift; TARGETS=$1; shift ;;
    --cabal-build-options) shift; CABAL_BUILD_OPTIONS=$1; shift ;;
    --rtsopts) shift; RTS_OPTIONS=$1; shift ;;
    # flags
    --slow) SLOW=1; shift ;;
    --quick) QUICK_MODE=1; shift ;;
    --dev-build) RUNNING_DEVBUILD=1; shift ;;
    --) shift; break ;;
    -*|--*) echo "Unknown flags: $*"; echo; print_help ;;
    *) break ;;
  esac
done
TARGET_EXE_ARGS=$*

#-----------------------------------------------------------------------------
# Determine targets
#-----------------------------------------------------------------------------

# Requires RUNNING_DEVBUILD var
source $SCRIPT_DIR/targets.sh

if test "$(has_item "$TARGETS" help)" = "help"
then
  list_target_groups
  list_targets
  exit
fi

DEFAULT_TARGETS="$(all_grp)"
TARGETS=$(set_targets)

echo "Using targets [$TARGETS]"

#-----------------------------------------------------------------------------
# Build targets
#-----------------------------------------------------------------------------

test_exe_rts_opts () {
  case "$1" in
    *) echo -n "-K8M -M64M" ;;
  esac
}

# $1: bench name
# $2: bench executable
target_exe_extra_args () {
  local bench_name=$1
  local bench_prog=$2

  echo "+RTS \
    $(test_exe_rts_opts $(basename $bench_prog)) \
    $RTS_OPTIONS \
    -RTS"
}

BUILD_TEST="$CABAL_EXECUTABLE v2-build $CABAL_BUILD_OPTIONS --enable-tests"
run_build "$BUILD_TEST" streamly-tests test "$TARGETS"

#-----------------------------------------------------------------------------
# Run targets
#-----------------------------------------------------------------------------

run_targets streamly-tests "$TARGETS" target_exe_extra_args
