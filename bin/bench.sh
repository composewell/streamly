#!/usr/bin/env bash

SCRIPT_DIR=$(cd `dirname $0`; pwd)

RUNNING_BENCHMARKS=y
source $SCRIPT_DIR/build-lib.sh

print_help () {
  echo "Usage: $0 "
  echo "       [--benchmarks <"bench1 bench2 ..." | help>]"
  echo "       [--fields <"field1 field2 ..." | help>]"
  echo "       [--graphs]"
  echo "       [--no-measure]"
  echo "       [--append]"
  echo "       [--long]"
  echo "       [--slow]"
  echo "       [--quick]"
  echo "       [--raw]"
  echo "       [--dev-build]"
  echo "       [--with-compiler <compiler exe name>]"
  echo "       [--cabal-build-options <options>]"
  echo "       [--rtsopts <opts>]"
  echo "       [--compare] [--base <commit>] [--candidate <commit>]"
  echo "       -- <gauge options or benchmarks>"
  echo
  echo "--benchmarks: benchmarks to run, use 'help' for list of benchmarks"
  echo "--fields: measurement fields to report, use 'help' for a list"
  echo "--graphs: Generate graphical reports"
  echo "--no-measure: Don't run benchmarks, run reports from previous results"
  echo "--append: Don't overwrite previous results, append for comparison"
  echo "--long: Use much longer stream size for infinite stream benchmarks"
  echo "--slow: Slightly more accurate results at the expense of speed"
  echo "--quick: Faster results, useful for longer benchmarks"
  echo "--raw: Run the benchmarks but don't report them. This is useful when"
  echo "       you only want to work with the csv files generated."
  echo "--cabal-build-options: Pass any cabal build options to be used for build"
  echo "       e.g. --cabal-build-options \"--flag dev\""
  echo
  echo "When specific space complexity group is chosen then (and only then) "
  echo "RTS memory restrictions are used accordingly. For example, "
  echo "bench.sh --benchmarks Data.Parser -- Data.Parser/o-1-space "
  echo "restricts Heap/Stack space for O(1) characterstics"
  echo
  echo "When using --compare, by default comparative chart of HEAD^ vs HEAD"
  echo "commit is generated, in the 'charts' directory."
  echo "Use --base and --candidate to select the commits to compare."
  echo
  echo "Any arguments after a '--' are passed directly to gauge"
  exit
}

#-----------------------------------------------------------------------------
# Reporting utility functions
#-----------------------------------------------------------------------------

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

# chart is expensive to build and usually not required to be rebuilt,
# use master branch as fallback
cabal_which_report() {
  local path=$(cabal_which streamly-benchmarks x $1)
  if test -z "$path"
  then
    echo "Cannot find $1 executable, trying in dist-newstyle" 1>&2
    local path1=$(cabal_which_builddir dist-newstyle streamly-benchmarks x $1)
    if test -z "$path1"
    then
      local path2="./bin/$1"
      echo "Cannot find $1 executable, trying $path2" 1>&2
      if test -e "$path2"
      then
        echo $path2
      fi
    else
        echo $path1
    fi
  else
    echo $path
  fi
}

find_report_prog() {
    local prog_name="chart"
    hash -r
    local prog_path=$(cabal_which_report $prog_name)
    if test -x "$prog_path"
    then
      echo $prog_path
    else
      return 1
    fi
}

build_report_prog() {
    local prog_name="chart"
    local prog_path=$(cabal_which_report $prog_name)

    hash -r
    if test ! -x "$prog_path" -a "$BUILD_ONCE" = "0"
    then
      echo "Building bench-show executables"
      BUILD_ONCE=1
      $CABAL_EXECUTABLE v2-build --flags dev chart \
        || die "bench-show build failed"

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
          die "Cannot find bench-show executable"
      echo "Using bench-show executable [$prog]"
  fi
}

# We run the benchmarks in isolation in a separate process so that different
# benchmarks do not interfere with other. To enable that we need to pass the
# benchmark exe path to gauge as an argument. Unfortunately it cannot find its
# own path currently.

# The path is dependent on the architecture and cabal version.

bench_output_file() {
    local bench_name=$1
    echo "charts/$bench_name/results.csv"
}

run_bench_target () {
  local package_name=$1
  local component=$2
  local target_name=$3
  local output_file=$(bench_output_file $target_name)

  local target_prog
  target_prog=$(cabal_target_prog $package_name $component $target_name) || \
    die "Cannot find executable for target $target_name"

  echo "Running executable $target_name ..."

  # Needed by bench-exec-one.sh
  export BENCH_EXEC_PATH=$target_prog
  if test "$LONG" -ne 0
  then
      BENCH_ARGS="-p /$target_name\/o-1-space/"
      STREAM_SIZE=10000000
      export STREAM_SIZE
  fi

  local MATCH=""
  if test "$USE_GAUGE" -eq 0
  then
    if test "$LONG" -ne 0
    then
      MATCH="-p /$target_name\/o-1-space/"
    else
        if test -n "$GAUGE_ARGS"
        then
          local GAUGE_ARGS1=$(echo "$GAUGE_ARGS" | sed -e 's/\//\\\//g')
          MATCH="-p /$GAUGE_ARGS1/"
        fi
    fi
    echo "Name,cpuTime,2*Stdev (ps),Allocated,bytesCopied" >> $output_file
    $target_prog -l $MATCH \
      | grep "^All" \
      | while read -r name; do bin/bench-exec-one.sh "$name"; done
  else
    if test "$LONG" -ne 0
    then
      MATCH="$target_name/o-1-space"
    else
      MATCH="$GAUGE_ARGS"
    fi
    echo "name,iters,time,cycles,cpuTime,utime,stime,maxrss,minflt,majflt,nvcsw,nivcsw,allocated,numGcs,bytesCopied,mutatorWallSeconds,mutatorCpuSeconds,gcWallSeconds,gcCpuSeconds" >> $output_file
    # XXX We may have to use "sort | awk" to keep only benchmark names with
    # shortest prefix e.g. "a/b/c" and "a/b", we should only keep "a/b"
    # otherwise benchmarks will run multiple times.
    $target_prog -l \
      | grep "^$target_name" \
      | grep "^$MATCH" \
      | sort | paste -sd "," - | awk 'BEGIN {FS=","} {t="XU987"; for(i=1;i<=NF;i++) if (substr($i,1,length(t)) != t) {print $i; t=$i}}' \
      | while read -r name; do bin/bench-exec-one.sh "$name"; done
  fi
}

# $1: package name
# $2: component
# $3: targets
run_bench_targets() {
    for i in $3
    do
      run_bench_target $1 $2 $i
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
    run_bench_targets streamly-benchmarks b "$bench_list" target_exe_extra_args

    echo "Checking out candidate commit [$CANDIDATE] for benchmarking"
    git checkout "$CANDIDATE" || \
        die "Checkout of candidate [$CANDIDATE] commit failed"

    $BUILD_BENCH || die "build failed"
    run_bench_targets streamly-benchmarks b "$bench_list" target_exe_extra_args
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
    run_bench_targets streamly-benchmarks b "$bench_list" target_exe_extra_args
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
              --benchmark $i --fields "$FIELDS"
    done
}

#-----------------------------------------------------------------------------
# Execution starts here
#-----------------------------------------------------------------------------

cd $SCRIPT_DIR/..

USE_GAUGE=0
export USE_GAUGE
USE_GIT_CABAL=1
set_common_vars

DEFAULT_FIELDS="allocated bytescopied cputime"
ALL_FIELDS="$FIELDS time cycles utime stime minflt majflt nvcsw nivcsw"
FIELDS=$DEFAULT_FIELDS

COMPARE=0
BASE=
CANDIDATE=

APPEND=0
LONG=0
RAW=0
GRAPH=0
MEASURE=1

GAUGE_ARGS=
BUILD_ONCE=0

CABAL_BUILD_OPTIONS="--flag fusion-plugin "

#-----------------------------------------------------------------------------
# Read command line
#-----------------------------------------------------------------------------

while test -n "$1"
do
  case $1 in
    -h|--help|help) print_help ;;
    # options with arguments
    --benchmarks) shift; TARGETS=$1; shift ;;
    --targets) shift; TARGETS=$1; shift ;;
    --fields) shift; FIELDS=$1; shift ;;
    --base) shift; BASE=$1; shift ;;
    --candidate) shift; CANDIDATE=$1; shift ;;
    --with-compiler) shift; CABAL_WITH_COMPILER=$1; shift ;;
    --cabal-build-flags) shift; CABAL_BUILD_OPTIONS+=$1; shift ;;
    --cabal-build-options) shift; CABAL_BUILD_OPTIONS+=$1; shift ;;
    --rtsopts) shift; RTS_OPTIONS=$1; shift ;;
    # flags
    --slow) SLOW=1; shift ;;
    --quick) QUICK_MODE=1; shift ;;
    --compare) COMPARE=1; shift ;;
    --raw) RAW=1; shift ;;
    --append) APPEND=1; shift ;;
    --long) LONG=1; shift ;;
    --graphs) GRAPH=1; shift ;;
    --no-measure) MEASURE=0; shift ;;
    --dev-build) RUNNING_DEVBUILD=1; shift ;;    
    --) shift; break ;;
    -*|--*) echo "Unknown flags: $*"; echo; print_help ;;
    *) break ;;
  esac
done
GAUGE_ARGS=$*

set_derived_vars

#-----------------------------------------------------------------------------
# Determine targets
#-----------------------------------------------------------------------------

only_real_benchmarks () {
  for i in $TARGETS
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

# Requires RUNNING_DEVBUILD var
source $SCRIPT_DIR/targets.sh

if test "$(has_item "$TARGETS" help)" = "help"
then
  list_target_groups
  list_comparisons
  list_targets
  exit
fi

if test "$(has_item "$FIELDS" help)" = "help"
then
  echo "Supported fields: $ALL_FIELDS"
  echo "Default fields: $DEFAULT_FIELDS"
  exit
fi

if test "$LONG" -ne 0
then
  if test -n "$TARGETS"
  then
    echo "Cannot specify benchmarks [$TARGETS] with --long"
    exit
  fi
  TARGETS=$infinite_grp
fi

DEFAULT_TARGETS="$(all_grp)"
TARGETS=$(set_targets)

TARGETS_ORIG=$TARGETS
TARGETS=$(only_real_benchmarks)

echo "Using benchmark suites [$TARGETS]"

#-----------------------------------------------------------------------------
# Build reporting utility
#-----------------------------------------------------------------------------

# We need to build the report progs first at the current (latest) commit before
# checking out any other commit for benchmarking.
build_report_progs

#-----------------------------------------------------------------------------
# Build and run targets
#-----------------------------------------------------------------------------

BUILD_BENCH="$CABAL_EXECUTABLE v2-build $CABAL_BUILD_OPTIONS --enable-benchmarks"
if test "$MEASURE" = "1"
then
  run_build "$BUILD_BENCH" streamly-benchmarks bench "$TARGETS"
  export QUICK_MODE
  export RTS_OPTIONS
  export LONG
  run_measurements "$TARGETS"
fi

#-----------------------------------------------------------------------------
# Run reports
#-----------------------------------------------------------------------------

COMPARISON_REPORTS=""
for i in $COMPARISONS
do
  if test "$(has_item "$TARGETS_ORIG" $i)" = $i
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
  run_reports "$TARGETS"
  run_reports "$COMPARISON_REPORTS"
fi
