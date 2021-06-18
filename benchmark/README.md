# Benchmarking

## Benchmark Drivers

Two benchmark drivers are supported:

* `tasty-bench` (default)
* `gauge` (enabled by `--use-gauge` build flag)

## Build and run benchmarks directly

The benchmark executables are `tasty-bench` executables unless you have
passed `--use-gauge` cabal flag when building in which case it is a
`gauge` executable.

```
$ cabal run bench:Prelude.Serial  # run selected
$ cabal run bench:Prelude.Serial -- --help # help on arguments
$ cabal run bench:Prelude.Serial -- --stdev 100000 # specify arguments
$ cabal run bench:Prelude.Serial --flag fusion-plugin # with fusion-plugin

$ cabal build bench:Prelude.Serial # build selected
$ cabal build --enable-benchmarks streamly-benchmarks # build all
$ cabal build --enable-benchmarks all # build all, alternate method

$ cabal build --flag "-opt" ... # disable optimization, faster build
```

## Building and Running Benchmarks with bench.sh

`<streamly repo>/bin/bench.sh` script is the top level driver for
running benchmarks. It runs the requested benchmarks and then creates a
report from the results using the `bench-show` package.

IMPORTANT NOTE:  The first time you run this script it may take a long
time because it has to build the `bench-report` executable which has a
lot of dependencies.  If you are using nix then use `--use-nix` flag
for the first time so that the `bench-report` executable is built using
nix. That can save a lot of time compiling it. However, once it is built
it will be cached in the `bin` directory of the repo and used from
there every time. You can also build it manually from the cabal file in
`benchmark/bench-report` and install it in the `bin` directory.

## bench.sh: Quick start

Useful commands:

```
$ bin/bench.sh --help
$ bin/bench.sh --quick # run all the benchmark suites
$ bin/bench.sh --benchmarks help # Show available benchmark suites
$ bin/bench.sh --benchmarks serial_grp # Run all serial benchmark suites
$ bin/bench.sh --benchmarks "Prelude.Serial Data.Parser" # run selected suites
$ bin/bench.sh --no-measure # don't run benchmarks just show previous results

# Run all O(1) space complexity benchmarks in `Prelude.Serial` suite
$ bin/bench.sh --benchmarks Prelude.Serial --prefix Prelude.Serial/o-1-space

# Run a specific benchmark in `Prelude.Serial` suite
$ bin/bench.sh --benchmarks Prelude.Serial --prefix Prelude.Serial/o-1-space.generation.unfoldr
```

Note: `bench.sh` enables fusion-plugin by default.

## Comparing results with baseline

```
# Checkout baseline commit
$ bin/bench.sh --quick

# Checkout commit with new changes
$ bin/bench.sh --quick --append

# To add another result to comparisons just repeat the above command on
# desired commit
```

## Comparing benchmark suites

First see the available benchmark suites:

```
$ bin/bench.sh --benchmarks help
```

You will see some benchmark suites end with `_cmp`, these are comparison
groups. If you run a comparison group benchmark, comparison of all the
benchmark suites in that group will be shown in the end. For example to compare
all array benchmark suites:

```
$ bin/bench.sh --benchmarks array_cmp
```

## Reporting without measuring

You can use the `--no-measure` option to report the already measured results in
the benchmarks results file. A results file may collect an arbitrary number of
results by running with `--append` multiple times. Each benchmark has its own
results file, for example the `Prelude.Serial` benchmark has the results file at
`charts/Prelude.Serial/results.csv`.

You can also manually edit the file to remove a set of results if you like or
to append results from previously saved results or from some other results
file. After editing you can run `bench.sh` with the `--no-measure` option to
see the reports corresponding to the results.

## Additional benchmark configuration

### Stream size

You can specify the stream size (default is 100000) to be used for
benchmarking:

```
$ cabal run bench:Prelude.Serial -- --stream-size 1000000
```

### External input file

In the `FileSystem.Handle` benchmark you can specify the input file as an
environment variable:

```
$ export Benchmark_FileSystem_Handle_InputFile=./gutenberg-500.txt
$ cabal run FileSystem.Handle -- FileSystem.Handle/o-1-space/reduce/read/S.splitOnSeq
```

The automatic tests do not test unicode input, this option is useful to specify
a unicode text file manually.

## Benchmarking notes

We run each benchmark in an isolated process to minimize interference
of benchmarks and to be able to control the RTS memory restrictions per
benchmark.

### Gotchas

Gauge forces a GC before and after the measurement. However, we have observed
that sometimes the GC stats may not be accurate when the number of iterations
in the measurement is small (e.g. 1 iteration).  In such cases usually the
number of GCs and GC times would also be 0.
