This file documents the required CI test matrix so that we do not accidentally
remove or miss any tests when making changes to CI configs:

* Last three major versions of GHC  (build lib, test, bench, docs), run
  tests, on Linux platform, using cabal build
* Only for prime GHC version:
  * Run on Windows and MacOS platforms too
  * stack build
  * Run `bench.sh --quick`
  * -Werror (for lib, test, bench)
  * `coverage` build
  * build from source distribution
  * hlint
  * `inspection` flag + `fusion-plugin` flag
  * --flag streamk
  * --flag debug
  * --flag doctests

* Latest version ghcjs build (lib, test, bench), run tests
