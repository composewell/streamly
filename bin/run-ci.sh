#!/usr/bin/env bash

# TODO: (1) Detect if nix is available otherwise run with plain cabal,
# (2) Detect the platform and run tests applicable to the platform, (3)
# add a test for windows/msys

#------------------------------------------------------------------------------
# GHC 8.10
#------------------------------------------------------------------------------

# build-all, Werror, test, inspection, fusion-plugin. Note, inspection
# requires fusion-plugin.
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
    CABAL_BUILD_OPTIONS=\"--flag inspection --flag fusion-plugin\"" || exit 1

#------------------------------------------------------------------------------
# hlint
#------------------------------------------------------------------------------

# XXX avoid rebuilding if nothing has changed in the source
packcheck cabal \
  HLINT_OPTIONS="lint --cpp-include=src --cpp-include=test"  \
  HLINT_TARGETS="src test benchmark" || exit 1

#------------------------------------------------------------------------------
# coverage
#------------------------------------------------------------------------------

nix-shell \
  --argstr compiler "ghc8101" \
  --argstr c2nix "--flag inspection" \
  --run "bin/test.sh --coverage" || exit 1

# To upload the results to coveralls.io using hpc-coveralls
# hpc-coveralls --repo-token="$REPO_TOKEN" --coverage-mode=StrictlyFullLines

#------------------------------------------------------------------------------
# GHC 8.8
#------------------------------------------------------------------------------

# chart builds from the nix cache for 8.8, 8.10 requires building
# XXX Use a separate build-dir for this
# build-all only, throw in most flags except fusion-plugin, inspection, opt
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
    CABAL_BUILD_OPTIONS=\"--flag streamk --flag debug --flag use-c-malloc --flag dev\"" || exit 1

# build-all only
#nix-shell \
#  --argstr compiler "ghc883" \
#  --run "\
#    packcheck cabal-v2 \
#    GHCVER=8.8.3 \
#    CABAL_DISABLE_DEPS=y \
#    CABAL_CHECK_RELAX=y \
#    DISABLE_SDIST_BUILD=y \
#    DISABLE_TEST=y \
#    DISABLE_DOCS=y " || exit 1

#------------------------------------------------------------------------------
# GHC 8.6
#------------------------------------------------------------------------------

nix-shell \
  --argstr compiler "ghc865" \
  --run "\
    packcheck cabal-v2 \
    GHCVER=8.6.5 \
    CABAL_DISABLE_DEPS=y \
    CABAL_CHECK_RELAX=y \
    DISABLE_SDIST_BUILD=y \
    DISABLE_TEST=y \
    DISABLE_DOCS=y " || exit 1

#------------------------------------------------------------------------------
# GHC 8.4
#------------------------------------------------------------------------------

nix-shell \
  --argstr compiler "ghc844" \
  --run "\
    packcheck cabal-v2 \
    GHCVER=8.4.4 \
    CABAL_DISABLE_DEPS=y \
    CABAL_CHECK_RELAX=y \
    DISABLE_SDIST_BUILD=y \
    DISABLE_TEST=y \
    DISABLE_DOCS=y " || exit 1

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
