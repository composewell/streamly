#!/usr/bin/env bash

# Environment passed:
# BENCH_EXEC_PATH: the benchmark executable
# RTS_OPTIONS: additional RTS options
# QUICK_MODE: whether we are in quick mode

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
bench_rts_opts_specific () {
  case "$1" in
    Data.Stream.StreamK/o-n-space/elimination/toList) echo -n "-K2M" ;;

    Prelude.Parallel/o-n-heap/mapping/mapM) echo -n "-M256M" ;;
    Prelude.Parallel/o-n-heap/monad-outer-product/*) echo -n "-M256M" ;;
    Prelude.Parallel/o-n-space/monad-outer-product/*) echo -n "-K4M -M256M" ;;

    Prelude.Serial/o-n-space/Functor/*) echo -n "-K4M -M64M" ;;
    Prelude.Serial/o-n-space/Applicative/*) echo -n "-K8M -M128M" ;;
    Prelude.Serial/o-n-space/Monad/*) echo -n "-K8M -M64M" ;;
    Prelude.Serial/o-n-heap/transformer/*) echo -n "-K8M -M64M" ;;
    Prelude.Serial/o-n-space/grouping/*) echo -n "" ;;
    Prelude.Serial/o-n-space/*) echo -n "-K4M" ;;

    Prelude.WSerial/o-n-space/*) echo -n "-K4M" ;;

    Prelude.Async/o-n-space/monad-outer-product/*) echo -n "-K4M" ;;
    Prelude.Ahead/o-n-space/monad-outer-product/*) echo -n "-K4M" ;;

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
    *) echo -n "" ;;
  esac
}

#------------------------------------------------------------------------------
# Speed options
#------------------------------------------------------------------------------

SUPER_QUICK_OPTIONS="--quick --min-duration 0 --time-limit 0 --include-first-iter"
QUICKER_OPTIONS="--min-samples 3 --time-limit 1"

# For certain long benchmarks if the user has not requested super quick
# mode we anyway use a slightly quicker mode.
use_quicker_mode () {
  if test -n "$QUICK_MODE"
  then
    if test "$QUICK_MODE" -eq 0
    then
      echo $QUICKER_OPTIONS
    fi
  fi
}

bench_exe_quick_opts () {
  case "$1" in
    Prelude.Concurrent) echo -n "$SUPER_QUICK_OPTIONS" ;;
    Prelude.Rate) echo -n "$SUPER_QUICK_OPTIONS" ;;
    Prelude.Adaptive) echo -n "$SUPER_QUICK_OPTIONS" ;;
    *) echo -n "" ;;
  esac
}

# Use quick options for benchmarks that take too long
bench_quick_opts () {
  case "$1" in
    Prelude.Parallel/o-n-heap/mapping/mapM)
        echo -n "$SUPER_QUICK_OPTIONS" ;;
    Prelude.Parallel/o-n-heap/monad-outer-product/*)
        echo -n "$SUPER_QUICK_OPTIONS" ;;
    Prelude.Parallel/o-n-space/monad-outer-product/*)
        echo -n "$SUPER_QUICK_OPTIONS" ;;
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

last=""
for i in "$@"
do
    BENCH_NAME="$last"
    last="$i"
done

RTS_OPTIONS=\
"+RTS -T \
$(bench_exe_rts_opts $(basename $BENCH_EXEC_PATH)) \
$(bench_rts_opts_default $BENCH_NAME) \
$(bench_rts_opts_specific $BENCH_NAME) \
$RTS_OPTIONS \
-RTS"

QUICK_BENCH_OPTIONS="\
$(bench_exe_quick_opts $(basename $BENCH_EXEC_PATH)) \
$(bench_quick_opts $BENCH_NAME)"

if test -n "$STREAM_SIZE"
then
  STREAM_LEN=$(env LC_ALL=en_US.UTF-8 printf "\--stream-size %'.f\n" $STREAM_SIZE)
fi

echo "$BENCH_NAME: \
$STREAM_LEN \
$QUICK_BENCH_OPTIONS \
$RTS_OPTIONS"

$BENCH_EXEC_PATH $RTS_OPTIONS "$@" $QUICK_BENCH_OPTIONS
