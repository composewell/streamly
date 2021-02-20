#!/usr/bin/env bash

# TODO: (1) Detect if nix is available otherwise run with plain cabal,
# (2) Detect the platform and run tests applicable to the platform, (3)
# add a test for windows/msys

SCRIPT_DIR=$(cd `dirname $0`; pwd)
source $SCRIPT_DIR/build-lib.sh

#------------------------------------------------------------------------------
# GHC 8.10
#------------------------------------------------------------------------------

# build-all, Werror, test, inspection, fusion-plugin. Note, inspection
# requires fusion-plugin.

basic () {
  nix-shell \
    --argstr compiler "ghc8101" \
    --argstr c2nix "--flag inspection" \
    --run "\
      packcheck cabal-v2 \
      GHCVER=8.10.1 \
      CABAL_DISABLE_DEPS=y \
      CABAL_CHECK_RELAX=y \
      DISABLE_SDIST_BUILD=y \
      CABAL_PROJECT=cabal.project.ci \
      CABAL_BUILD_OPTIONS=\"--flag inspection --flag fusion-plugin\""
# chart deps do not build with ghc-8.10 on nix
#  nix-shell \
#    --argstr compiler "ghc8101" \
#    --argstr c2nix "--flag dev" \
#    --run "cabal build chart --flag dev"
#  nix-shell \
#    --argstr compiler "ghc8101" \
#    --argstr c2nix "--flag inspection" \
#    --run "\
#      bin/bench.sh \
#        --quick \
#        --cabal-build-options \"--cabal-project cabal.project.ci --flag inspection --flag fusion-plugin\""
  }

#------------------------------------------------------------------------------
# hlint
#------------------------------------------------------------------------------

# XXX avoid rebuilding if nothing has changed in the source
# XXX run hlint only on changed files
hlint () {
  packcheck cabal \
    HLINT_OPTIONS="lint --cpp-include=src --cpp-include=test"  \
    HLINT_TARGETS="src test benchmark"
}

#------------------------------------------------------------------------------
# coverage
#------------------------------------------------------------------------------

coverage () {
  nix-shell \
    --argstr compiler "ghc8101" \
    --run "bin/test.sh --coverage"
}

# To upload the results to coveralls.io using hpc-coveralls
# hpc-coveralls --repo-token="$REPO_TOKEN" --coverage-mode=StrictlyFullLines

#------------------------------------------------------------------------------
# GHC 8.8
#------------------------------------------------------------------------------

# chart builds from the nix cache for 8.8, 8.10 requires building
# XXX Use a separate build-dir for this
# build-all only, throw in most flags except fusion-plugin, inspection, opt
flags () {
  nix-shell \
    --argstr compiler "ghc883" \
    --argstr c2nix "--flag dev" \
    --run "\
      packcheck cabal-v2 \
      GHCVER=8.8.3 \
      CABAL_DISABLE_DEPS=y \
      CABAL_CHECK_RELAX=y \
      DISABLE_SDIST_BUILD=y \
      DISABLE_TEST=y \
      DISABLE_DOCS=y \
      DISABLE_BENCH=y \
      CABAL_BUILD_OPTIONS=\"--flag streamk --flag debug --flag use-c-malloc --flag dev\""
}

# build-all only
# $1 883
# $2 8.8.3
ghc () {
  nix-shell \
    --argstr compiler "ghc$1" \
    --run "\
      packcheck cabal-v2 \
      GHCVER=$2 \
      CABAL_DISABLE_DEPS=y \
      CABAL_CHECK_RELAX=y \
      DISABLE_SDIST_BUILD=y \
      DISABLE_TEST=y \
      DISABLE_DOCS=y "
}

ghc883 () { ghc 883 8.8.3; }
ghc865 () { ghc 865 8.6.5; }
ghc844 () { ghc 865 8.4.4; }

#------------------------------------------------------------------------------
# ghcjs
#------------------------------------------------------------------------------

#nix-shell \
#  --argstr compiler "ghcjs" \
#  --run "\
#    packcheck cabal-v2 \
#    GHCVER=8.6.0 \
#    CABAL_DISABLE_DEPS=y \
#    CABAL_CHECK_RELAX=y \
#    DISABLE_SDIST_BUILD=y \
#    DISABLE_TEST=y \
#    DISABLE_DOCS=y \
#    ENABLE_GHCJS=y " || exit 1

#-----------------------------------------------------------------------------
# Read command line
#-----------------------------------------------------------------------------

ALL_TARGETS="all basic hlint coverage flags ghc883 ghc864 ghc844"

print_targets () {
  echo "Available targets: $ALL_TARGETS"
}

print_help () {
  echo "Usage: $0 --targets <space separated target names>"
  print_targets
  exit
}

while test -n "$1"
do
  case $1 in
    -h|--help|help) print_help ;;
    # options with arguments
    --targets) shift; TARGETS=$1; shift ;;
    --) shift; break ;;
    -*|--*) echo "Unknown flags: $*"; echo; print_help ;;
    *) break ;;
  esac
done

if test "$(has_item "$TARGETS" help)" = "help"
then
  print_targets
  exit
fi

if test "$(has_item "$TARGETS" help)" = "all"
then
  TARGETS="$ALL_TARGETS"
fi

if test -z "$TARGETS"
then
  print_help
fi

for i in $TARGETS
do
    $i || { echo "$i failed."; exit 1; }
done
