# Continuous integration tests

This file documents the required CI test matrix so that we do not accidentally
remove or miss any tests when making changes to CI configs:

## Mono repo

We host "streamly" and "streamly-core" packages in the same repo. This
creates a problem for the sdist build for the "streamly" package because
it depends on the "streamly-core" package. In the "streamly" sdist build
we use a cabal.project file to pick up "streamly-core" from github
master branch HEAD. If we make changes to both the packages at the same
time we won't be able to do this.

We use one sdist build for "streamly" package and one for "streamly-core". The
sdist build for "streamly" might fail because of the reason described above.

## For prime GHC version:

Distribution:
  * build "streamly" from source distribution WITHOUT a cabal.project file
  * build "streamly-core" from source distribution WITHOUT a cabal.project file

Performance:
  * `--flag inspection` + `--flag fusion-plugin`
  * Run `benchmark/bench-runner --quick --raw`
  * Run `test/test-runner`
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
