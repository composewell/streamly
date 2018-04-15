#!/bin/bash

print_help () {
  echo "Usage: $0 [--quick] [--append] [--no-graphs] [--no-measure] \
[--no-compare] [--base commit] [--candidate commit] -- <gauge options>"
  echo
  echo "Default generates comparative chart of HEAD^ vs HEAD commit, "
  echo "in the 'charts' directory. Use --base and --candidate to select "
  echo "the commits to compare. Use --no-compare to just benchmark the "
  echo "current workspace only."
  echo
  echo "Any arguments after a '--' are passed directly to guage"
  echo "You can omit '--' if the gauge args used do not start with a '-'."
  exit
}

# $1: message
die () {
  >&2 echo -e "Error: $1"
  exit 1
}

COMPARE=1

while test -n "$1"
do
  case $1 in
    -h|--help|help) print_help ;;
    --quick) QUICK=1; shift ;;
    --append) APPEND=1; shift ;;
    --base) shift; BASE=$1; shift ;;
    --candidate) shift; CANDIDATE=$1; shift ;;
    --no-compare) COMPARE=0; shift ;;
    --no-graphs) GRAPH=0; shift ;;
    --no-measure) MEASURE=0; shift ;;
    --) shift; break ;;
    -*|--*) print_help ;;
    *) break ;;
  esac
done

GAUGE_ARGS=$*

STACK=stack
echo "Using stack command [$STACK]"

# We build it first at the current commit before checking out any other commit
# for benchmarking.
if test "$GRAPH" != "0"
then
  CHARTS=$($STACK exec which charts)
  if test ! -x $CHARTS
  then
    echo "Building charts executable"
    $STACK build --flag "streamly:dev" || die "build failed"
  fi

  CHARTS=$($STACK exec which charts)
  if test ! -x $CHARTS
  then
    die "Could not find 'charts' executable"
  fi
fi

# We run the benchmarks in isolation in a separate process so that different
# benchmarks do not interfere with other. To enable that we need to pass the
# benchmark exe path to guage as an argument. Unfortunately it cannot find its
# own path currently.

# The path is dependent on the architecture and cabal version.
# Use this command to find the exe if this script fails with an error:
# find .stack-work/ -type f -name "benchmarks"

find_bench_prog () {
  BENCH_PROG=`$STACK path --dist-dir`/build/benchOps/benchOps
  if test ! -x "$BENCH_PROG"
  then
    echo
    echo "WARNING! benchmark binary [$BENCH_PROG] not found or not executable"
    echo "WARNING! not using isolated measurement."
    echo
  fi
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

if test "$QUICK" = "1"
then
  ENABLE_QUICK="--quick"
fi

OUTPUT_FILE="charts/results.csv"

run_bench () {
  $STACK build --bench --no-run-benchmarks || die "build failed"
  find_bench_prog
  mkdir -p charts

  # We set min-samples to 1 so that we run with default benchmark duration of 5
  # seconds, whatever number of samples are possible in that.
  # We run just one iteration for each sample. Anyway the default is to run
  # for 30 ms and most our benchmarks are close to that or more.
  # If we use less than three samples, statistical analysis crashes
  $BENCH_PROG $ENABLE_QUICK \
    --include-first-iter \
    --min-samples 3 \
    --min-duration 0 \
    --csvraw=$OUTPUT_FILE \
    -v 2 \
    --measure-with $BENCH_PROG $GAUGE_ARGS || die "Benchmarking failed"
}

if test "$MEASURE" != "0"
  then
  if test -e $OUTPUT_FILE -a "$APPEND" != 1
  then
    mv -f -v $OUTPUT_FILE ${OUTPUT_FILE}.prev
  fi

  if test "$COMPARE" = "0"
  then
    run_bench
  else
    if test -z "$CANDIDATE"
    then
      CANDIDATE=$(git rev-parse HEAD)
    fi
    if test -z "$BASE"
    then
      # XXX Should be where the current branch is forked from master
      BASE="$CANDIDATE^"
    fi
    echo "Checking out base commit for benchmarking"
    git checkout "$BASE" || die "Checkout of base commit failed"
    run_bench
    echo "Checking out candidate commit for benchmarking"
    git checkout "$CANDIDATE" || die "Checkout of candidate commit failed"
    run_bench
  fi
fi

if test "$GRAPH" != "0"
then
  echo
  echo "Generating charts from ${OUTPUT_FILE}..."
  $CHARTS
fi

# XXX reset back to the original commit
