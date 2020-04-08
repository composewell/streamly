#!/bin/bash

SERIAL_O_1="linear base"
SERIAL_O_n="serial-o-n-heap serial-o-n-stack serial-o-n-space \
  base-o-n-heap base-o-n-stack base-o-n-space"
FOLD_BENCHMARKS="fold-o-1-space fold-o-n-heap"
UNFOLD_BENCHMARKS="unfold-o-1-space unfold-o-n-space"

SERIAL_BENCHMARKS="$SERIAL_O_1 $SERIAL_O_n $FOLD_BENCHMARKS"
# parallel benchmark-suite is separated because we run it with a higher
# heap size limit.
CONCURRENT_BENCHMARKS="linear-async linear-rate nested-concurrent parallel concurrent adaptive"
ARRAY_BENCHMARKS="array unpinned-array prim-array small-array"

# XXX We can include SERIAL_O_1 here once "base" also supports --stream-size
INFINITE_BENCHMARKS="linear linear-async linear-rate nested-concurrent"
FINITE_BENCHMARKS="$SERIAL_O_n $ARRAY_BENCHMARKS fileio parallel concurrent adaptive"

# Benchmarks that take long time per iteration must run fewer iterations to
# finish in reasonable time.
QUICK_BENCHMARKS="linear-rate concurrent adaptive fileio"
VIRTUAL_BENCHMARKS="array-cmp"

ALL_BENCHMARKS="$SERIAL_BENCHMARKS $CONCURRENT_BENCHMARKS $ARRAY_BENCHMARKS $VIRTUAL_BENCHMARKS"

# RTS options that go inside +RTS and -RTS while running the benchmark.
bench_rts_opts () {
  case "$1" in
    "fold-o-1-space") echo -n "-T -K36K -M16M" ;;
    "fold-o-n-heap") echo -n "-T -K36K -M128M" ;;
    "unfold-o-1-space") echo -n "-T -K36K -M16M" ;;
    "unfold-o-n-space") echo -n "-T -K32M -M64M" ;;
    *) echo -n "" ;;
  esac
}

# The correct executable for the given benchmark name.
bench_exec () {
  case "$1" in
    "fold-o-1-space") echo -n "fold" ;;
    "fold-o-n-heap") echo -n "fold" ;;
    "unfold-o-1-space") echo -n "unfold" ;;
    "unfold-o-n-space") echo -n "unfold" ;;
    *) echo -n "$1" ;;
  esac
}

# Specific gauge options for the given benchmark.
bench_gauge_opts () {
  case "$1" in
    "fold-o-1-space") echo -n "-m prefix o-1-space" ;;
    "fold-o-n-heap") echo -n "-m prefix o-n-heap" ;;
    "unfold-o-1-space") echo -n "-m prefix o-1-space" ;;
    "unfold-o-n-space") echo -n "-m prefix o-n-space" ;;
    *) echo -n "" ;;
  esac
}

list_benches ()  {
  for i in $ALL_BENCHMARKS
  do
    echo -n "|$i"
  done
}

print_help () {
  echo "Usage: $0 "
  echo "       [--benchmarks <ALL|SERIAL|CONCURRENT|ARRAY|INFINITE|FINITE|DEV$(list_benches)>]"
  echo "       [--group-diff]"
  echo "       [--graphs]"
  echo "       [--no-measure]"
  echo "       [--append]"
  echo "       [--long]"
  echo "       [--slow]"
  echo "       [--quick]"
  echo "       [--compare] [--base commit] [--candidate commit]"
  echo "       [--cabal-build-flags]"
  echo "       -- <gauge options or benchmarks>"
  echo
  echo "Multiple benchmarks can be specified as a space separated list"
  echo " e.g. --benchmarks \"linear nested\""
  echo
  echo "--group-diff is used to compare groups within a single benchmark"
  echo " e.g. StreamD vs StreamK in base benchmark."
  echo
  echo "When using --compare, by default comparative chart of HEAD^ vs HEAD"
  echo "commit is generated, in the 'charts' directory."
  echo "Use --base and --candidate to select the commits to compare."
  echo
  echo "Any arguments after a '--' are passed directly to gauge"
  exit
}

# $1: message
die () {
  >&2 echo -e "Error: $1"
  exit 1
}

set_benchmarks() {
  if test -z "$BENCHMARKS"
  then
    echo $DEFAULT_BENCHMARKS
  else
    for i in $(echo $BENCHMARKS)
    do
        case $i in
          ALL) echo -n $ALL_BENCHMARKS ;;
          SERIAL) echo -n $SERIAL_BENCHMARKS ;;
          CONCURRENT) echo -n $CONCURRENT_BENCHMARKS ;;
          ARRAY) echo -n $ARRAY_BENCHMARKS ;;
          INFINITE) echo -n $INFINITE_BENCHMARKS ;;
          FINITE) echo -n $FINITE_BENCHMARKS ;;
          array-cmp) echo -n "$ARRAY_BENCHMARKS array-cmp" ;;
          *) echo -n $i ;;
        esac
        echo -n " "
    done
  fi
}

# $1: benchmark name (linear, nested, base)
find_report_prog() {
    local prog_name="chart"
    hash -r
    local prog_path=$($WHICH_COMMAND $prog_name)
    if test -x "$prog_path"
    then
      echo $prog_path
    else
      return 1
    fi
}

# $1: benchmark name (linear, nested, base)
build_report_prog() {
    local prog_name="chart"
    local prog_path=$($WHICH_COMMAND $prog_name)

    hash -r
    if test ! -x "$prog_path" -a "$BUILD_ONCE" = "0"
    then
      echo "Building bench-graph executables"
      BUILD_ONCE=1
      $BUILD_CHART_EXE || die "build failed"
    elif test ! -x "$prog_path"
    then
      return 1
    fi
    return 0
}

build_report_progs() {
  if test "$RAW" = "0"
  then
      build_report_prog || exit 1
      local prog
      prog=$(find_report_prog) || \
          die "Cannot find bench-graph executable"
      echo "Using bench-graph executable [$prog]"
  fi
}

# We run the benchmarks in isolation in a separate process so that different
# benchmarks do not interfere with other. To enable that we need to pass the
# benchmark exe path to gauge as an argument. Unfortunately it cannot find its
# own path currently.

# The path is dependent on the architecture and cabal version.
# Use this command to find the exe if this script fails with an error:
# find .stack-work/ -type f -name "benchmarks"

stack_bench_prog () {
  local bench_name=$1
  local bench_prog=`stack path --dist-dir`/build/$bench_name/$bench_name
  if test -x "$bench_prog"
  then
    echo $bench_prog
  else
    return 1
  fi
}

cabal_bench_prog () {
  local bench_name=$1
  local bench_prog=`$WHICH_COMMAND $1`
  if test -x "$bench_prog"
  then
    echo $bench_prog
  else
    return 1
  fi
}

bench_output_file() {
    local bench_name=$1
    echo "charts/$bench_name/results.csv"
}

# --min-duration 0 means exactly one iteration per sample. We use a million
# iterations in the benchmarking code explicitly and do not use the iterations
# done by the benchmarking tool.
#
# Benchmarking tool by default discards the first iteration to remove
# aberrations due to initial evaluations etc. We do not discard it because we
# are anyway doing iterations in the benchmarking code and many of them so that
# any constant factor gets amortized and anyway it is a cost that we pay in
# real life.
#
# We can pass --min-samples value from the command line as second argument
# after the benchmark name in case we want to use more than one sample.

run_bench () {
  local bench_name=$1
  local bench_exe=$(bench_exec $bench_name)
  local output_file=$(bench_output_file $bench_name)
  local bench_prog
  local quick_bench=0
  bench_prog=$($GET_BENCH_PROG $bench_exe) || \
    die "Cannot find benchmark executable for benchmark $bench_name"

  mkdir -p `dirname $output_file`

  echo "Running benchmark $bench_name ..."

  for i in $QUICK_BENCHMARKS
  do
    if test "$(has_benchmark $i)" = "$bench_name"
    then
      quick_bench=1
    fi
  done

  local QUICK_OPTS="--quick --time-limit 1 --min-duration 0"
  local SPEED_OPTIONS
  if test "$LONG" -eq 0
  then
    if test "$SLOW" -eq 0
    then
        if test "$QUICK" -eq 0 -a "$quick_bench" -eq 0
        then
          # reasonably quick
          SPEED_OPTIONS="$QUICK_OPTS --min-samples 10"
        else
          # super quick but less accurate
          SPEED_OPTIONS="$QUICK_OPTS --include-first-iter"
        fi
    else
      SPEED_OPTIONS="--min-duration 0"
    fi
  else
      SPEED_OPTIONS="--stream-size 10000000 $QUICK_OPTS --include-first-iter"
  fi

  $bench_prog $SPEED_OPTIONS \
    +RTS $(bench_rts_opts $bench_name) -RTS \
    --csvraw=$output_file \
    -v 2 \
    --measure-with $bench_prog $GAUGE_ARGS \
    $(bench_gauge_opts $bench_name) || die "Benchmarking failed"
}

run_benches() {
    for i in $1
    do
      run_bench $i
    done
}

run_benches_comparing() {
    local bench_list=$1

    if test -z "$CANDIDATE"
    then
      CANDIDATE=$(git rev-parse HEAD)
    fi
    if test -z "$BASE"
    then
      # XXX Should be where the current branch is forked from master
      BASE="$CANDIDATE^"
    fi
    echo "Comparing baseline commit [$BASE] with candidate [$CANDIDATE]"
    echo "Checking out base commit [$BASE] for benchmarking"
    git checkout "$BASE" || die "Checkout of base commit [$BASE] failed"

    $BUILD_BENCH || die "build failed"
    run_benches "$bench_list"

    echo "Checking out candidate commit [$CANDIDATE] for benchmarking"
    git checkout "$CANDIDATE" || \
        die "Checkout of candidate [$CANDIDATE] commit failed"

    $BUILD_BENCH || die "build failed"
    run_benches "$bench_list"
    # XXX reset back to the original commit
}

backup_output_file() {
  local bench_name=$1
  local output_file=$(bench_output_file $bench_name)

  if test -e $output_file -a "$APPEND" != 1
  then
      mv -f -v $output_file ${output_file}.prev
  fi
}

run_measurements() {
  local bench_list=$1

  for i in $bench_list
  do
      backup_output_file $i
  done

  if test "$COMPARE" = "0"
  then
    run_benches "$bench_list"
  else
    run_benches_comparing "$bench_list"
  fi
}

run_reports() {
    local prog
    prog=$(find_report_prog) || \
      die "Cannot find bench-graph executable"
    echo

    for i in $1
    do
        echo "Generating reports for ${i}..."
        $prog $(test "$GRAPH" = 1 && echo "--graphs") \
              $(test "$GROUP_DIFF" = 1 && echo "--group-diff") \
              --benchmark $i
    done
}

#-----------------------------------------------------------------------------
# Execution starts here
#-----------------------------------------------------------------------------

DEFAULT_BENCHMARKS="linear"
GROUP_DIFF=0

COMPARE=0
BASE=
CANDIDATE=

APPEND=0
SLOW=0
QUICK=0
LONG=0
RAW=0
GRAPH=0
MEASURE=1

GAUGE_ARGS=
BUILD_ONCE=0
USE_STACK=0
CABAL_BUILD_FLAGS=""

GHC_VERSION=$(ghc --numeric-version)

cabal_which() {
  find dist-newstyle -type f -path "*${GHC_VERSION}*/$1"
}

#-----------------------------------------------------------------------------
# Read command line
#-----------------------------------------------------------------------------

while test -n "$1"
do
  case $1 in
    -h|--help|help) print_help ;;
    # options with arguments
    --benchmarks) shift; BENCHMARKS=$1; shift ;;
    --base) shift; BASE=$1; shift ;;
    --candidate) shift; CANDIDATE=$1; shift ;;
    --cabal-build-flags) shift; CABAL_BUILD_FLAGS=$1; shift ;;
    # flags
    --slow) SLOW=1; shift ;;
    --quick) QUICK=1; shift ;;
    --compare) COMPARE=1; shift ;;
    --raw) RAW=1; shift ;;
    --append) APPEND=1; shift ;;
    --long) LONG=1; shift ;;
    --group-diff) GROUP_DIFF=1; shift ;;
    --graphs) GRAPH=1; shift ;;
    --no-measure) MEASURE=0; shift ;;
    --) shift; break ;;
    -*|--*) print_help ;;
    *) break ;;
  esac
done
GAUGE_ARGS=$*

BENCHMARKS=$(set_benchmarks)
if test "$LONG" -ne 0
then
  BENCHMARKS=$INFINITE_BENCHMARKS
fi

only_real_benchmarks () {
  for i in $BENCHMARKS
  do
    local SKIP=0
    for j in $VIRTUAL_BENCHMARKS
    do
      if test $i == $j
      then
        SKIP=1
      fi
    done
    if test "$SKIP" -eq 0
    then
      echo -n "$i "
    fi
  done
}

proper_executables () {
  for i in $BENCHMARKS
  do
    echo -n "$(bench_exec $i) "
  done
}


BENCHMARKS_ORIG=$BENCHMARKS
BENCHMARKS=$(only_real_benchmarks)
EXECUTABLES=$(proper_executables)
echo "Using benchmark suites [$BENCHMARKS]"

has_benchmark () {
  for i in $BENCHMARKS_ORIG
  do
    if test "$i" = "$1"
    then
      echo "$i"
      break
    fi
  done
}

if test "$USE_STACK" = "1"
then
  WHICH_COMMAND="stack exec which"
  BUILD_CHART_EXE="stack build --flag streamly:dev"
  GET_BENCH_PROG=stack_bench_prog
  BUILD_BENCH="stack build $STACK_BUILD_FLAGS --bench --no-run-benchmarks"
else
  # XXX cabal issue "cabal v2-exec which" cannot find benchmark/test executables
  #WHICH_COMMAND="cabal v2-exec which"
  WHICH_COMMAND=cabal_which
  BUILD_CHART_EXE="cabal v2-build --flags dev chart"
  GET_BENCH_PROG=cabal_bench_prog
  BUILD_BENCH="cabal v2-build $CABAL_BUILD_FLAGS --enable-benchmarks"
fi

#-----------------------------------------------------------------------------
# Build stuff
#-----------------------------------------------------------------------------

# We need to build the report progs first at the current (latest) commit before
# checking out any other commit for benchmarking.
build_report_progs "$BENCHMARKS"

#-----------------------------------------------------------------------------
# Run benchmarks
#-----------------------------------------------------------------------------

if test "$MEASURE" = "1"
then
  echo $BUILD_BENCH
  if test "$USE_STACK" = "1"
  then
    $BUILD_BENCH || die "build failed"
  else
    $BUILD_BENCH $EXECUTABLES || die "build failed"
  fi
  run_measurements "$BENCHMARKS"
fi

#-----------------------------------------------------------------------------
# Run reports
#-----------------------------------------------------------------------------

VIRTUAL_REPORTS=""
if test "$(has_benchmark 'array-cmp')" = "array-cmp"
then
  VIRTUAL_REPORTS="$VIRTUAL_REPORTS array-cmp"
  mkdir -p "charts/array-cmp"
  cat "charts/array/results.csv" \
      "charts/prim-array/results.csv" \
      "charts/unpinned-array/results.csv" > "charts/array-cmp/results.csv"
fi

if test "$RAW" = "0"
then
  run_reports "$BENCHMARKS"
  run_reports "$VIRTUAL_REPORTS"
fi
