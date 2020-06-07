## Running Benchmarks

`bench.sh` script at the root of the repo is the top level driver for running
benchmarks. It runs the requested benchmarks and then creates a report from the
results using the `bench-show` package. Try `bench.sh --help` for available
options to run it.

## Quick start

You must be in the repo root directory to run these commands.

Run the default benchmark suites:

```
$ ./bench.sh --quick
```

You can remove the `--quick` option to run benchmark with lower speed but
better accuracy, or use `--slow` to even further lower the speed and increase
the accuracy a bit.

Show available benchmark suites:

```
$ ./bench.sh --benchmarks help
```

Run all benchmark suites in the `serial_grp` group:

```
$ ./bench.sh --benchmarks serial_grp
```

Run `Prelude.Serial` and `Data.Parser` benchmark suites:

```
$ ./bench.sh --benchmarks "Prelude.Serial Data.Parser"
```

Run all O(1) space complexity benchmarks in `Prelude.Serial` suite:

```
$ ./bench.sh --benchmarks Prelude.Serial -- Prelude.Serial/o-1-space
```

Anything after a `--` is passed to gauge, it basically selects all benchmarks
starting with `Prelude.Serial/o-1-space` prefix.

Run a specific benchmark in `Prelude.Serial` suite:

```
$ ./bench.sh --benchmarks Prelude.Serial -- Prelude.Serial/o-1-space/generation/unfoldr
```

## Comparing results for regression

To compare two sets of results, first run the benchmarks at the baseline
commit:

```
$ ./bench.sh
```

And then run with the `--append` option at the commit that you want to compare
with the baseline. It will show the comparison with the baseline:

```
$ ./bench.sh --append
```

Append just adds the next set of results in the same results file. You can keep
appending more results and all of them will be compared with the baseline.

You can use `--compare` to compare the previous commit with the head commit:

```
$ ./bench.sh --compare
```

To compare the head commit with some other base commit:

```
$ ./bench.sh --compare --base d918833
```

To compare two arbitrary commits:

```
$ ./bench.sh --compare --base d918833 --candidate 38aa5f2
```

Note that the above may not always work because the script and the benchmarks
themselves might have changed across the commits. The `--append` method is more
reliable to compare.

## Comparing benchmark suites

First see the available benchmark suites:

```
$ ./bench.sh --benchmarks help
```

You will see some benchmark suites end with `_cmp`, these are comparison
groups. If you run a comparsion group benchmark, comparison of all the
benchmark suites in that group will be shown in the end. For example to compare
all array benchmark suites:

```
$ ./bench.sh --benchmarks array_cmp
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
