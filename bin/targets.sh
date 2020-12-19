#------------------------------------------------------------------------------
# test and benchmark groups
#------------------------------------------------------------------------------

# IMPORTANT NOTE: the names "_grp" and "_cmp" suffixes are special, do
# not rename them to something else.

base_stream_grp="\
  `bench_only Data.Stream.StreamD` \
  `bench_only Data.Stream.StreamK` \
  `bench_only Data.Stream.StreamDK`"

prelude_serial_grp="\
  Prelude.Serial \
  Prelude.WSerial \
  Prelude.ZipSerial"

prelude_concurrent_grp="\
  Prelude.Async \
  Prelude.WAsync \
  Prelude.Ahead \
  Prelude.Parallel \
  Prelude.ZipAsync"


prelude_other_grp="\
  `test_only Prelude` \
  $(test_only $(dev_build Prelude.Rate)) \
  `bench_only Prelude.Rate` \
  `test_only Prelude.Fold` \
  Prelude.Concurrent \
  `bench_only Prelude.Adaptive`"

array_grp="\
  Data.Array \
  Data.Array.Storable.Foreign \
  Data.Array.Prim \
  Data.SmallArray \
  Data.Array.Prim.Pinned"

base_parser_grp="\
  Data.Parser.ParserD \
  `bench_only Data.Parser.ParserK`"

parser_grp="\
  Data.Fold \
  Data.Parser"

list_grp="\
  Data.List.Base \
  Data.List"

#------------------------------------------------------------------------------
# Streaming vs non-streaming
#------------------------------------------------------------------------------
# The "o-1-space" groups of these benchmarks are run with long stream
# sizes when --long option is used.

infinite_grp="\
  $prelude_serial_grp \
  $prelude_concurrent_grp \
  `bench_only Prelude.Rate`"

#------------------------------------------------------------------------------
# All test/benchmark modules must be in at least one of these
#------------------------------------------------------------------------------

# All groups
GROUP_TARGETS="\
    base_stream_grp \
    prelude_serial_grp \
    prelude_concurrent_grp \
    prelude_other_grp \
    array_grp \
    base_parser_grp \
    parser_grp \
    list_grp  \
    infinite_grp"

# Not in any groups
INDIVIDUAL_TARGETS="\
    Data.Unfold \
    Unicode.Stream \
    `bench_only FileSystem.Handle` \
    `test_only FileSystem.Event` \
    `test_only version-bounds`"
