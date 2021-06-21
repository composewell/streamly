# Continuous integration tests

This file documents the required CI test matrix so that we do not accidentally
remove or miss any tests when making changes to CI configs:

## For prime GHC version:

Distribution:
  * build from source distribution WITHOUT a cabal.project file

Performance:
  * `--flag inspection` + `--flag fusion-plugin`
  * Run `bin/bench.sh --quick --raw`
  * Run `bin/test.sh`
  * -Werror (for lib, test, bench)

Lint:
  * -Werror (for lib, test, bench) (Need a Werror with default flags)
  * hlint

Doctests:
  * Run cabal-docspec

Coverage:
  * `coverage` build

Debug:
  * --flag debug

StreamK:
  * --flag streamk

Windows:
  * Windows + stack

MacOS:
  * MacOS + stack

## Other GHC versions

GHC head:
  * `--flag inspection` + `--flag fusion-plugin` tests to catch any
    issues early.
  * Ideally we should also be running perf tests and compare
    against the prime version.

Other GHC versions:
* build lib, test, bench, docs, run tests, on Linux platform, using
  cabal build

## GHCJS

GHCJS:
* Latest version ghcjs build (lib, test, bench), run tests
