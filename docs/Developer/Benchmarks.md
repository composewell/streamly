# Benchmarking

## Build and run benchmarks directly

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

## Building and Running Benchmarks with bench-runner

The executable `bench-runner` is the top level driver for
running benchmarks. It runs the requested benchmarks and then creates a
report from the results using the `bench-show` package.

IMPORTANT NOTE:  The first time you run this executable it may take a long
time because it has to build the `bench-report` executable which has a
lot of dependencies.

You can install it once in the root of the repository and use it multiple times.

You can use `cabal.project.report` to install bench-runner like so:
```
$ cabal install bench-runner --project-file=cabal.project.report --installdir=./  --overwrite-policy=always
$ ./bench-runner <bench-runner-args>
```

If you're using nix, you can install bench-runner like so:
```
$ cd benchmark/bench-runner
$ nix-shell --run 'cabal install bench-runner --installdir=../../  --overwrite-policy=always'
$ cd ../../
$ ./bench-runner <bench-runner-args>
```

You can run the `bench-runner` without installing, like so:
```
$ cabal run bench-runner --project-file=cabal.project.report -- <bench-runner-args>
```

## bench-runner: Quick start

Assuming `bench-runner` is the executable. You can replace `./bench-runner` with
`cabal run bench-runner --project-file=cabal.project.report --`

Note, you need to pass two mandatory arguments to bench-runner,
`package-version` and `package-name`. For the streamly-benchmarks package, pass
these as below:

```
bin/bench-runner --package-version 0.0.0 --package-name streamly-benchmarks
```

Useful commands:

```
$ ./bench-runner --help
$ ./bench-runner --quick # run all the benchmark suites
$ ./bench-runner --targets help # Show available benchmark suites
$ ./bench-runner --targets serial_grp # Run all serial benchmark suites
$ ./bench-runner --targets "Prelude.Serial Data.Parser" # run selected suites
$ ./bench-runner --no-measure # don't run benchmarks just show previous results

# Run all O(1) space complexity benchmarks in `Prelude.Serial` suite
$ ./bench-runner --targets Prelude.Serial --prefix Prelude.Serial/o-1-space

# Run a specific benchmark in `Prelude.Serial` suite
$ ./bench-runner --targets Prelude.Serial --prefix Prelude.Serial/o-1-space.generation.unfoldr
```

Note: `bench-runner` enables fusion-plugin by default.

## Comparing results with baseline

```
# Checkout baseline commit
$ ./bench-runner --quick

# Checkout commit with new changes
$ ./bench-runner --quick --append

# To add another result to comparisons just repeat the above command on
# desired commit

# To display the current results without running the benchmarks.
# See "Reporting without measuring" for more info.
$ ./bench-runner --no-measure
```

## Comparing benchmark suites

First see the available benchmark suites:

```
$ ./bench-runner --targets help
```

You will see some benchmark suites end with `_cmp`, these are comparison
groups. If you run a comparison group benchmark, comparison of all the
benchmark suites in that group will be shown in the end. For example to compare
all array benchmark suites:

```
$ ./bench-runner --targets array_cmp
```

## Reporting without measuring

You can use the `--no-measure` option to report the already measured results in
the benchmarks results file. A results file may collect an arbitrary number of
results by running with `--append` multiple times. Each benchmark has its own
results file, for example the `Prelude.Serial` benchmark has the results file at
`charts/Prelude.Serial/results.csv`.

You can also manually edit the file to remove a set of results if you like or
to append results from previously saved results or from some other results
file. After editing you can run `bench-runner` with the `--no-measure` option to
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

## Running benchmarks on valid UTF8 input

To run the unicode benchmarks on valid utf8 input, you can do the following,

```
$ Benchmark_FileSystem_Handle_InputFile=<valid-unicode-filepath> ./bench-runner --benchmarks Unicode.Stream --cabal-build-options "-f include-strict-utf8"
```

## Benchmarking notes

We run each benchmark in an isolated process to minimize interference
of benchmarks and to be able to control the RTS memory restrictions per
benchmark.

### Gotchas

The benchmark driver forces a GC before and after the measurement.
However, we have observed
that sometimes the GC stats may not be accurate when the number of iterations
in the measurement is small (e.g. 1 iteration).  In such cases usually the
number of GCs and GC times would also be 0.

## Diagnosing Performance Issues

### Reproducible comparison

When comparing different compilers we need to make sure that we are
using exactly the same versions of the libraries for apples to apples
comparison. We have seen cases where a change in the "random" library
caused allocations regressions in the new version of compiler because of
the way in which the benchmark code was generated due to the change.

When it is required to reproduce benchmark results precisely across
different systems, it is recommended that you create and use a cabal
freeze file so that the versions of all libraries are pinned.

### Identifying issues

There are two ways to find problematic code:

1. Run performance benchmarks using `bench-runner`, select the benchmarks
   that are taking more than expected time.
2. When making a new change, compare with the baseline and select benchmarks
   with the most regression reported by `bench-runner`.

Number of allocations are the most stable measure that do not vary from
run to run. `cpuTime` and `bytesCopied` may vary. When comparing two
runs for regression the first thing to look at is the difference in
allocations. Also note that allocations may vary from run to run for
concurrent benchmarks.

The next thing to look at is cpuTime. Please note that cpuTime may
fluctuate quite a bit, you may want to run the relevant benchmarks
without the --quick mode for confirming and make sure no other load is
running on the system when measuring.

Usually the increase is cpuTime is proportional to the increase in
allocations but sometimes it may increase independently because more cpu
instructions are being executed. TBD - we should count the instructions
instead.

### Inspection Testing

Before you proceed make sure have to run the benchmarks with
`inspection` flag on. It may catch any obvious issues or regressions.

```
$ cabal build --flag inspection --flag fusion-plugin --enable-benchmarks streamly-benchmarks
```

### Compiling with diagnostics

1. Comment out all other benchmarks in the given benchmark suite, and
   keep only the one you are examining.
2. Edit the file and add the following line on top:

```
{-# OPTIONS_GHC
  -ddump-simpl
  -ddump-to-file
  -dsuppress-all
  -Wmissed-specialisations
  -Wall-missed-specialisations
  -fplugin-opt=Fusion.Plugin:verbose=2
  -fplugin-opt=Fusion.Plugin:dump-core
#-}
```
3. Build the benchmark suite with fusion-plugin enabled:

```
$ cabal build bench:Prelude.Serial --flag fusion-plugin
```

See the `.dump-simpl` file in the cabal build directory. You can find it
like this:

```
$ find dist-newstyle/ -name "*.dump-simpl"
```

Make sure you are looking into the right build dir (`--build-dir` may change
`dist-newstyle` to something else), and check in the appropriate GHC
version dir.

### Compiling standalone example

Sometimes you may want to create a separate program from the benchmark code
removing the benchmarking harness to simplify and isolate the code for better
reasoning and simpler core.

Add the following GHC options at the top of your file, say, example.hs:

```
{-# OPTIONS_GHC
  -ddump-simpl
  -ddump-to-file
  -dsuppress-all
  -Wmissed-specialisations
  -Wall-missed-specialisations
  -fplugin Fusion.Plugin
  -fplugin-opt=Fusion.Plugin:verbose=2
  -fplugin-opt=Fusion.Plugin:dump-core
#-}
```

Do not include the optimization options in OPTIONS_GHC pragma, instead, specify
them on the command line. This is to avoid optimization failing if you import
another module which is not compiled with the same optimization options.

```
$ cabal build # build and write ghc environment file
$ ghc -O2 -fspec-constr-recursive=16 -fmax-worker-args=16 example.hs
```

To pinpoint where the optimization is going wrong you can examine the
plugin generated core files for each optimization pass. The files are
numbered for each optimization pass. You can compare successive files
using side-by-side diff and see what the compiler is doing between each
pass.

### Diagnosing the Problem

#### Specialization Issues

Look for missed specialization messages. When you are comparing against a
baseline, check if something that was specialized before is no longer
specialized.

In the core you have to look for type class dictionaries e.g.

```
exc_r6DD = \ @s_a6ai -> try $fMonadCatchIO $fExceptionSomeException
```

Search for `$f` in the core.

#### Fusion Issues

Look for unfused function warnings emitted by fusion-plugin. You may
want to take a look at the unfused constructors or functions that
fusion-plugin is warning about.  Beware that:

* fusion-plugin emits warnings for unfused stuff in intermediate functions as
  well, those should be ignored.
* the constructors may remain genuinely unfused unless the loop is
  closed. So you should look at the warnings in the file where the loop is
  closed and everything is supposed to be fused.

Also, look at the core for unfused constructors. At times you may need
to look for the boxed primitive type constructors e.g. W8# or I#, these
may not be eliminated, usually, due to strictness issues.

Often it is useful to diff and compare the core without the problem and
the core with the problem especially in cases when the problem is due to
GHC version changes, or smaller changes in the code.

Note, some operations are inherently fusion breaking, those cannot fuse,
they are usually annotated so in their documentation.

### Resolving the problem

Review the problematic code, see [the optimization
guide](/docs/Developer/optimization-guidelines.md) for common problems and how to
solve those. If no obvious issues are found on review, then generate and
examine the core.

You may want to add the `Fuse` annotation on some of those constructors
to make the code fuse. Please note that unnecessary `Fuse` annotations
may cause unnecessary inlining. Also, make sure that the constructor you
are adding fuse annotation is not shared by any other code where you may
not want inlining/fusion.
