#!/bin/bash

base_grp="\
    Data.Stream.StreamD \
    Data.Stream.StreamK \
    Data.Stream.StreamDK"
SERIAL_O_1="linear"
SERIAL_O_n="serial-o-n-heap serial-o-n-stack serial-o-n-space"
UNFOLD_BENCHMARKS="unfold-o-1-space unfold-o-n-space"
serial_grp="\
    $SERIAL_O_1 \
    $SERIAL_O_n \
    Data.Fold"

# parallel benchmark-suite is separated because we run it with a higher
# heap size limit.
concurrent_grp="linear-async linear-rate nested-concurrent parallel concurrent adaptive"
array_grp="Memory.Array Data.Array Data.Prim.Array Data.SmallArray"

# XXX We can include SERIAL_O_1 here once "base" also supports --stream-size
infinite_grp="linear linear-async linear-rate nested-concurrent"
finite_grp="$SERIAL_O_n $array_grp fileio parallel concurrent adaptive"

# Benchmarks that take long time per iteration must run fewer iterations to
# finish in reasonable time.
QUICK_BENCHMARKS="linear-rate concurrent adaptive fileio"

# *_cmp denotes a comparison benchmarks, the benchmarks provided in *_cmp
# variables are compared with each other
array_cmp="Memory.Array Data.Prim.Array Data.Array"
base_cmp="Data.Stream.StreamD Data.Stream.StreamK"
COMPARISONS="array_cmp base_cmp"

all_grp="\
    $base_grp \
    $serial_grp \
    $concurrent_grp \
    $array_grp"

ALL_BENCH_GROUPS="\
    all_grp \
    serial_grp \
    concurrent_grp \
    array_grp \
    infinite_grp \
    finite_grp"

# RTS options that go inside +RTS and -RTS while running the benchmark.
bench_rts_opts () {
  case "$1" in
    "unfold-o-1-space") echo -n "-T -K36K -M16M" ;;
    "unfold-o-n-space") echo -n "-T -K32M -M64M" ;;
    "linear") echo -n "-T -K36K -M16M" ;;
    "serial-o-n-stack") echo -n "-T -K1M -M16M" ;;
    "serial-o-n-heap") echo -n "-T -K36K -M128M" ;;
    "serial-o-n-space") echo -n "-T -K16M -M64M" ;;
    Data.SmallArray/o-1-sp*) echo -n "-T -K128K -M16M" ;;
    */o-1-sp*) echo -n "-T -K36K -M16M" ;;
    */o-n-h*) echo -n "-T -K36K -M32M" ;;
    */o-n-st*) echo -n "-T -K1M -M16M" ;;
    */o-n-sp*) echo -n "-T -K1M -M32M" ;;

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
    "linear") echo -n "serial" ;;
    "serial-o-n-stack") echo -n "serial" ;;
    "serial-o-n-heap") echo -n "serial" ;;
    "serial-o-n-space") echo -n "serial" ;;
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
    "linear") echo -n "-m prefix o-1-space" ;;
    "serial-o-n-stack") echo -n "-m prefix o-n-stack" ;;
    "serial-o-n-heap") echo -n "-m prefix o-n-heap" ;;
    "serial-o-n-space") echo -n "-m prefix o-n-space" ;;
    *) echo -n "" ;;
  esac
}

list_benches ()  {
  echo "Individual benchmarks:"
  for i in $all_grp
  do
    echo "$i"
  done
  echo
}

list_bench_groups ()  {
  echo "Benchmark groups:"
  for i in $ALL_BENCH_GROUPS
  do
    echo -n "$i ["
    eval "echo -n \$$i"
    echo "]"
  done
  echo
}

list_comparisons ()  {
  echo "Comparison groups:"
  for i in $COMPARISONS
  do
    echo -n "$i ["
    eval "echo -n \$$i"
    echo "]"
  done
  echo
}

print_help () {
  echo "Usage: $0 "
  echo "       [--benchmarks <"bench1 bench2 ..." | ?>]"
  echo "       [--graphs]"
  echo "       [--no-measure]"
  echo "       [--append]"
  echo "       [--long]"
  echo "       [--slow]"
  echo "       [--quick]"
  echo "       [--cabal-build-flags <flag>]"
  echo "       [--compare] [--base <commit>] [--candidate <commit>]"
  echo "       -- <gauge options or benchmarks>"
  echo
  echo "--benchmarks: benchmarks to run, use '?' for list of benchmarks"
  echo "--graphs: Generate graphical reports"
  echo "--no-measure: Don't run benchmarks, run reports from previous results"
  echo "--append: Don't overwrite previous results, append for comparison"
  echo "--long: Use much longer stream size for infinite stream benchmarks"
  echo "--slow: Slightly more accurate results at the expense of speed"
  echo "--quick: Faster results, useful for longer benchmarks"
  echo "--cabal-build-flags: Pass any cabal builds flags to be used for build"
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
          *_grp) eval "echo -n \$${i}" ;;
          *_cmp) eval "echo -n \$${i} $i" ;;
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

# $1: command
function run_verbose() {
  echo "$*"
  bash -c "$*"
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

  run_verbose $bench_prog $SPEED_OPTIONS \
    +RTS $(bench_rts_opts $GAUGE_ARGS) -RTS \
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
              --benchmark $i
    done
}

#-----------------------------------------------------------------------------
# Execution starts here
#-----------------------------------------------------------------------------

DEFAULT_BENCHMARKS="linear"

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
    --graphs) GRAPH=1; shift ;;
    --no-measure) MEASURE=0; shift ;;
    --) shift; break ;;
    -*|--*) print_help ;;
    *) break ;;
  esac
done
GAUGE_ARGS=$*

only_real_benchmarks () {
  for i in $BENCHMARKS
  do
    local SKIP=0
    for j in $COMPARISONS
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

BENCHMARKS_ORIG=$BENCHMARKS
if test "$(has_benchmark help)" = "help"
then
  list_bench_groups
  list_comparisons
  list_benches
  exit
fi

if test "$LONG" -ne 0
then
  if test -n "$BENCHMARKS"
  then
    echo "Cannot specify benchmarks [$BENCHMARKS] with --long"
    exit
  fi
  BENCHMARKS=$infinite_grp
fi

BENCHMARKS=$(set_benchmarks)
BENCHMARKS_ORIG=$BENCHMARKS
BENCHMARKS=$(only_real_benchmarks)
EXECUTABLES=$(proper_executables)

echo "Using benchmark suites [$BENCHMARKS]"

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

COMPARISON_REPORTS=""
for i in $COMPARISONS
do
  if test "$(has_benchmark $i)" = $i
  then
    COMPARISON_REPORTS="$COMPARISON_REPORTS $i"
    mkdir -p "charts/$i"
    constituents=$(eval "echo -n \$${i}")
    dest_file="charts/$i/results.csv"
    : > $dest_file
    for j in $constituents
    do
      cat "charts/$j/results.csv" >> $dest_file
    done
  fi
done

if test "$RAW" = "0"
then
  run_reports "$BENCHMARKS"
  run_reports "$COMPARISON_REPORTS"
fi
