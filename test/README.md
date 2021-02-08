## Using bin/test.sh

`bin/test.sh` (path relative to the top level repo dir) can run selected
test groups, generate coverage report, pass GHC RTS options when
running tests. `test.sh` runs the tests with restricted memory, see
`bin/test.sh` source for details on the amount of memory used or to change it.

To generate coverage report:

```
$ bin/test.sh --coverage
```

To view coverage report, open `./hpc_index.html` in browser.

See `bin/test.sh --help` for more info.

## Build a single test

```
$ cabal build test:Prelude.Serial
```

or:

```
$ cd test; cabal build Prelude.Serial
```

Build with optimizations:

```
$ cabal build --flag opt ...
```

## Build all tests

```
$ cabal build --enable-tests all
```

or:

```
$ cd test; cabal build --enable-tests
```

Or this, note this command does not work as expected when in the "test" dir:

```
$ cabal build --enable-tests streamly-tests
```

## Build and run

Running all test suites, use any of the following:

```
$ cabal test all
```

or:

```
$ cd test; cabal test
```

Or this, note this command does not work as expected when in the "test" dir:

```
$ cabal test streamly-tests
```

## Build and Run a single test suite

To run `Prelude.Serial` test-suite:

```
$ cabal run test:Prelude.Serial
```

or:

```
$ cd test; cabal run Prelude.Serial
```

Note you could use `cabal test Prelude.Serial` but that unfortunately builds
all the test suites before running `Prelude.Serial`.

## Naming of test modules

Tests are organized by source modules. For example, for the source
module `Streamly.Data.Array` and `Streamly.Internal.Data.Array` we have
a test module `Data.Array`. For some modules tests for a source module
are broken into multiple modules. For example, for `Streamly.Prelude` we have
`Streamly.Prelude.Serial`, `Streamly.Prelude.Async` etc.
