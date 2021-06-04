# Benchmarking

## Benchmark Dirvers

Two benchmark drivers are supported:

* `tasty-bench` (default)
* `gauge` (enabled by `--use-gauge` build flag)

## Building a single benchmark suite

```
$ cabal build streamly-benchmarks:Prelude.Serial
```

or:

```
$ cabal build --enable-benchmarks bench:Prelude.Serial
```

## Building all benchmarks suites

```
$ cabal build --enable-benchmarks streamly-benchmarks
```

or:

```
$ cabal build --enable-benchmarks all
```

Disable optimization, quick build:

```
$ cabal build --flag "-opt" ...
```

## Build and run single benchmarks:

The benchmark executables are `tasty-bench` executables unless you have
passed `--use-gauge` cabal flag when building in which case it is a
`gauge` executable.

For quick results you may have to use a large value for `--stdev` or use
`bench.sh --quick` as described in the following sections.

```
$ cabal run bench:Prelude.Serial -- --stdev 100000
```

`cabal bench` can be used but you cannot pass arguments (like --stdev):

```
$ cabal bench Prelude.Serial
```

## Build and run all benchmarks

Don't try this, it will take too long, use the `bench.sh` method instead.

```
$ cabal bench all
```

or:

```
$ cd benchmark; cabal bench
```

or this, note this command does not work from "benchmark" dir:

```
$ cabal bench streamly-benchmarks
```

## Building and Running Benchmarks with bench.sh

`<streamly repo>/bin/bench.sh` script is the top level driver for
running benchmarks. It runs the requested benchmarks and then creates a
report from the results using the `bench-show` package. Try `bench.sh
--help` for available options to run it.

## bench.sh: Quick start

Run the default benchmark suites:

```
$ bench.sh --quick
```

You can remove the `--quick` option to run benchmark with lower speed but
better accuracy, or use `--slow` to even further lower the speed and increase
the accuracy a bit.

Show available benchmark suites:

```
$ bench.sh --benchmarks help
```

Run all benchmark suites in the `serial_grp` group:

```
$ bench.sh --benchmarks serial_grp
```

Run `Prelude.Serial` and `Data.Parser` benchmark suites:

```
$ bench.sh --benchmarks "Prelude.Serial Data.Parser"
```

Run all O(1) space complexity benchmarks in `Prelude.Serial` suite:

```
$ bench.sh --benchmarks Prelude.Serial --prefix Prelude.Serial/o-1-space
```

Anything after a `--` is passed to the benchmark executable,
it basically selects all benchmarks starting with
`Prelude.Serial/o-1-space` prefix.

Run a specific benchmark in `Prelude.Serial` suite:

```
$ bench.sh --benchmarks Prelude.Serial --prefix Prelude.Serial/o-1-space/generation/unfoldr
```

Run a benchmark directly instead of running it through `bench.sh`:

```
$ cabal run bench:Prelude.Serial -- Prelude.Serial/o-1-space/generation/unfoldr
```

The options after `--` are the benchmark executable options.

## Comparing results of arbitrary runs

Note: use `--quick` and benchmark selection if you do not intend to wait for a
while.

To compare two sets of results, first run the benchmarks at the baseline
commit:

```
$ bench.sh
```

And then run with the `--append` option at the commit that you want to compare
with the baseline. It will show the comparison with the baseline:

```
$ bench.sh --append
```

Append just adds the next set of results in the same results file. You can keep
appending more results and all of them will be compared with the baseline.

## Comparing results of two commits

Note: use `--quick` and benchmark selection if you do not intend to wait for a
while.

You can use `--compare` to compare the previous commit with the head commit:

```
$ bench.sh --compare
```

To compare the head commit with some other base commit:

```
$ bench.sh --compare --base d918833
```

To compare two arbitrary commits:

```
$ bench.sh --compare --base d918833 --candidate 38aa5f2
```

Note that the above may not always work because the script and the benchmarks
themselves might have changed across the commits. The `--append` method is more
reliable to compare.

## Comparing benchmark suites

First see the available benchmark suites:

```
$ bench.sh --benchmarks help
```

You will see some benchmark suites end with `_cmp`, these are comparison
groups. If you run a comparison group benchmark, comparison of all the
benchmark suites in that group will be shown in the end. For example to compare
all array benchmark suites:

```
$ bench.sh --benchmarks array_cmp
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

### Unicode input

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
