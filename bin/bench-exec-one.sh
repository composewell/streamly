#!/usr/bin/env bash

# Environment passed:
# BENCH_EXEC_PATH: the benchmark executable
# RTS_OPTIONS: additional RTS options
# QUICK_MODE: whether we are in quick mode
# USE_GAUGE: whether to use gauge or tasty-bench
# LONG: whether to use a large stream size

set -e
set -o pipefail

# $1: message
die () {
  >&2 echo -e "Error: $1"
  exit 1
}

warn () {
  >&2 echo -e "Warning: $1"
}

test -n "$BENCH_EXEC_PATH" || die "BENCH_EXEC_PATH env var must be set"
test -n "$QUICK_MODE" || warn "QUICK_MODE env var not set (to 0 or 1)"

#------------------------------------------------------------------------------
# RTS Options
#------------------------------------------------------------------------------

# RTS options based on the benchmark executable
bench_exe_rts_opts () {
  case "$1" in
    Prelude.Concurrent*) echo -n "-K256K -M384M" ;;
    *) echo -n "" ;;
  esac
}

# General RTS options for different classes of benchmarks
bench_rts_opts_default () {
  case "$1" in
    */o-1-sp*) echo -n "-K36K -M16M" ;;
    */o-n-h*) echo -n "-K36K -M32M" ;;
    */o-n-st*) echo -n "-K1M -M16M" ;;
    */o-n-sp*) echo -n "-K1M -M32M" ;;
    *) echo -n "" ;;
  esac
}

# Overrides for specific benchmarks
# XXX Note: for tasty-bench we replace the "." separator in the benchmark names
# with "/" for matching with this. It may not work reliably if the benchmark
# name already contains ".".
bench_rts_opts_specific () {
  case "$1" in
    Data.Stream.StreamD/o-n-space/elimination/toList) echo -n "-K2M" ;;
    Data.Stream.StreamK/o-n-space/elimination/toList) echo -n "-K2M" ;;

    Prelude.Parallel/o-n-heap/mapping/mapM) echo -n "-M256M" ;;
    Prelude.Parallel/o-n-heap/monad-outer-product/*) echo -n "-M256M" ;;
    Prelude.Parallel/o-n-space/monad-outer-product/*) echo -n "-K4M -M256M" ;;

    Prelude.Rate/o-1-space/*) echo -n "-K128K" ;;
    Prelude.Rate/o-1-space/asyncly/*) echo -n "-K128K" ;;

    # XXX For GHC-9.0
    Prelude.Serial/o-1-space/mixed/sum-product-fold) echo -n "-K64M" ;;

    # XXX These should be moved to o-n-space?
    Prelude.Serial/o-n-heap/grouping/classifySessionsOf) echo -n "-K1M -M32M" ;;
    Prelude.Serial/o-n-heap/Functor/*) echo -n "-K4M -M32M" ;;
    Prelude.Serial/o-n-heap/transformer/*) echo -n "-K8M -M64M" ;;

    Prelude.Serial/o-n-space/Functor/*) echo -n "-K4M -M64M" ;;
    Prelude.Serial/o-n-space/Applicative/*) echo -n "-K8M -M128M" ;;
    Prelude.Serial/o-n-space/Monad/*) echo -n "-K8M -M64M" ;;

    # Use -K4M for o-n-space except for grouping
    Prelude.Serial/o-n-space/grouping/*) echo -n "" ;;
    Prelude.Serial/o-n-space/*) echo -n "-K4M" ;;

    Prelude.WSerial/o-n-space/*) echo -n "-K4M" ;;

    Prelude.Async/o-n-space/monad-outer-product/*) echo -n "-K4M" ;;
    Prelude.Ahead/o-n-space/monad-outer-product/*) echo -n "-K4M" ;;
    Prelude.Ahead/o-1-space/*) echo -n "-K128K" ;;

    Prelude.WAsync/o-n-heap/monad-outer-product/toNull3) echo -n "-M64M" ;;
    Prelude.WAsync/o-n-space/monad-outer-product/*) echo -n "-K4M" ;;

    # XXX need to investigate these, taking too much stack
    Data.Parser.ParserD/o-1-space/some) echo -n "-K8M" ;;
    Data.Parser/o-1-space/some) echo -n "-K8M" ;;
    Data.Parser.ParserD/o-1-space/manyTill) echo -n "-K4M" ;;
    Data.Parser/o-1-space/manyTill) echo -n "-K4M" ;;
    Data.Parser/o-n-heap/manyAlt) echo -n "-K4M -M128M" ;;
    Data.Parser/o-n-heap/someAlt) echo -n "-K4M -M128M" ;;
    Data.Parser.ParserK/o-n-heap/manyAlt) echo -n "-K4M -M128M" ;;
    Data.Parser.ParserK/o-n-heap/someAlt) echo -n "-K4M -M128M" ;;
    Data.Parser.ParserK/o-n-heap/sequence) echo -n "-M64M";;
    Data.Parser.ParserK/o-n-heap/sequenceA) echo -n "-M64M";;

    Data.SmallArray/o-1-sp*) echo -n "-K128K" ;;
    # For tasty-bench
    Data.Array*/o-1-space/generation/show) echo -n "-M32M" ;;
    *) echo -n "" ;;
  esac
}

#------------------------------------------------------------------------------
# Speed options
#------------------------------------------------------------------------------

if test "$USE_GAUGE" -eq 0
then
  SUPER_QUICK_OPTIONS="--stdev 1000000"
  QUICKER_OPTIONS="--stdev 100"
else
  # Do not keep time limit as 0 otherwise GC stats may remain 0 in some cases.
  SUPER_QUICK_OPTIONS="--quick --min-duration 0 --time-limit 0.01 --include-first-iter"
  QUICKER_OPTIONS="--min-samples 3 --time-limit 1"
fi

# tasty-bench does not like an option set twice
set_super_quick_mode () {
    echo -n super_quick
}

# For certain long benchmarks if the user has not requested super quick
# mode we anyway use a slightly quicker mode.
use_quicker_mode () {
  if test "$QUICK_MODE" -eq 0
  then
    echo quicker
  fi
}

bench_exe_quick_opts () {
  case "$1" in
    Prelude.Concurrent) set_super_quick_mode ;;
    Prelude.Rate) set_super_quick_mode ;;
    Prelude.Adaptive) set_super_quick_mode;;
    *) echo -n "" ;;
  esac
}

# XXX Note: for tasty-bench we replace the "." separator in the benchmark names
# with "/" for matching with this. It may not work reliably if the benchmark
# name already contains ".".

# Use quick options for benchmarks that take too long
bench_quick_opts () {
  case "$1" in
    Prelude.Parallel/o-n-heap/mapping/mapM) set_super_quick_mode ;;
    Prelude.Parallel/o-n-heap/monad-outer-product/*) set_super_quick_mode ;;
    Prelude.Parallel/o-n-space/monad-outer-product/*) set_super_quick_mode ;;
    Prelude.Parallel/o-n-heap/generation/*) use_quicker_mode ;;
    Prelude.Parallel/o-n-heap/mapping/*) use_quicker_mode ;;
    Prelude.Parallel/o-n-heap/concat-foldable/*) use_quicker_mode ;;

    Prelude.Async/o-1-space/monad-outer-product/*) use_quicker_mode ;;
    Prelude.Async/o-n-space/monad-outer-product/*) use_quicker_mode ;;

    Prelude.Ahead/o-1-space/monad-outer-product/*) use_quicker_mode ;;
    Prelude.Ahead/o-n-space/monad-outer-product/*) use_quicker_mode ;;

    Prelude.WAsync/o-n-heap/monad-outer-product/*) use_quicker_mode ;;
    Prelude.WAsync/o-n-space/monad-outer-product/*) use_quicker_mode ;;

    FileSystem.Handle/*) use_quicker_mode ;;
    *) echo -n "" ;;
  esac
}

bench_output_file() {
    local bench_name=$1
    echo "charts/$bench_name/results.csv"
}

#------------------------------------------------------------------------------
# Determine options from benchmark name
#------------------------------------------------------------------------------

BENCH_NAME_ORIG="$1"
shift

if test "$USE_GAUGE" -eq 0
then
  # XXX this is a hack to make the "/" separated names used in the functions
  # determining options based on benchmark name. For tasty-bench the benchmark
  # names are separated by "." instead of "/".
  BENCH_NAME0=$(echo $BENCH_NAME_ORIG | sed -e s/^All\.//)
  BENCH_NAME1=$(echo $BENCH_NAME0 | cut -f1 -d '/')
  BENCH_NAME2=$(echo $BENCH_NAME0 | cut -f2- -d '/' | sed -e 's/\./\//g')
  BENCH_NAME="$BENCH_NAME1/$BENCH_NAME2"
else
  BENCH_NAME=$BENCH_NAME_ORIG
fi

RTS_OPTIONS=\
"+RTS -T \
$(bench_exe_rts_opts $(basename $BENCH_EXEC_PATH)) \
$(bench_rts_opts_default $BENCH_NAME) \
$(bench_rts_opts_specific $BENCH_NAME) \
$RTS_OPTIONS \
-RTS"

QUICK_MODE_TYPE="\
$(if test "$QUICK_MODE" -ne 0; then set_super_quick_mode; fi) \
$(bench_exe_quick_opts $(basename $BENCH_EXEC_PATH)) \
$(bench_quick_opts $BENCH_NAME)"

for i in $QUICK_MODE_TYPE
do
  case "$i" in
    super_quick) QUICK_BENCH_OPTIONS="$SUPER_QUICK_OPTIONS"; break ;;
    quicker) QUICK_BENCH_OPTIONS="$QUICKER_OPTIONS"; break ;;
  esac
done

if test "$LONG" -ne 0
then
  STREAM_SIZE=10000000
  STREAM_LEN=$(env LC_ALL=en_US.UTF-8 printf "--stream-size %'.f\n" $STREAM_SIZE)
  STREAM_SIZE_OPT="--stream-size $STREAM_SIZE"
fi

echo "$BENCH_NAME_ORIG: \
$RTS_OPTIONS \
$STREAM_LEN \
$QUICK_BENCH_OPTIONS" \
"$@"

#------------------------------------------------------------------------------
# Run benchmark with options and collect results
#------------------------------------------------------------------------------

output_file=$(bench_output_file $(basename $BENCH_EXEC_PATH))
mkdir -p `dirname $output_file`
rm -f ${output_file}.tmp

if test $USE_GAUGE -eq 0
then
  # Escape "\" and double quotes in benchmark names
  BENCH_NAME_ESC=$(echo "$BENCH_NAME_ORIG" | sed -e 's/\\/\\\\/g' | sed -e 's/"/\\"/g')
  $BENCH_EXEC_PATH \
    -j 1 \
    $RTS_OPTIONS \
    $STREAM_SIZE_OPT \
    $QUICK_BENCH_OPTIONS \
    "$@" \
    --csv=${output_file}.tmp \
    -p '$0 == "'"$BENCH_NAME_ESC"'"'

  # Convert cpuTime field from picoseconds to seconds
  tail -n +2 ${output_file}.tmp | \
    awk 'BEGIN {FPAT = "([^,]+)|(\"[^\"]+\")";OFS=","} {$2=$2/1000000000000;print}' \
    >> $output_file
else
  $BENCH_EXEC_PATH \
    $RTS_OPTIONS \
    $STREAM_SIZE_OPT \
    $QUICK_BENCH_OPTIONS \
    "$@" \
    --csvraw=${output_file}.tmp \
    -m exact "$BENCH_NAME"
  tail -n +2 ${output_file}.tmp \
    >> $output_file
fi
