# Proposed design fixes

These changes have a wide design impact or need a user decision, so they
were not made as part of the robustness fixes. This file is not part of the
package distribution.

## The final value of a scan is dropped by scan drivers

`Scanl` has both `extract` and `final`. `final` is called at the end of the
input, but several drivers run it only for its effect and discard its
value:

* `Fold.postscanl scan fold`: at the end of the input `final` of the scan
  is called and its value is not fed to `fold`.
* `Stream.scanl` and `Stream.postscanl`: on `Stop` the drivers run
  `final` and stop without yielding its value. The value of `final` is
  yielded only when the scan returns `Done`.

A scan whose `final` differs from `extract` loses its last output. For
example, `Array.scanCompactMin n` returns `Nothing` from `extract` for an
incomplete buffer and `Just buf` from `final`. `Handle.writeChunksWith` and
`Socket.writeChunksWith` used `Fold.postscanl (Array.scanCompactMin n)` and
did not write the last partial buffer (they now use
`Fold.many (Array.createCompactMin n)`).

Proposal: define the contract of `final` as "the last output of the scan"
and make every driver emit it when it differs from the last emitted value,
or change the type of `final` to return `Maybe b`, `Nothing` meaning no
further output. Either change touches all scan drivers and all scans that
define `final`.

## Scanl.partitionBy and Fold.partitionBy have incompatible shapes

`Fold.partitionBy f l r` returns the pair of the two fold results.
`Scanl.partitionBy f l r` requires both scans to have the same output type
and emits one value per input, the current value of the scan the input was
routed to. With `odd` inputs to `Scanl.sum` and `even` inputs to
`Scanl.length`, `[1,2,3,4,5]` gives `[0,1,1,4,2,9]`.

Proposal: either rename the Scanl version (e.g. `routeBy`), or make it emit
the pair `(b, c)` of the current values of both scans on each input to match
the Fold version.

## maxYields is not exact for ordered streams

The ordered (`ordered True`) channel decrements the yield limit before
running a stream, but the streams it moved to the reordering heap are later
drained without checking the limit (see the comment on
`runStreamWithYieldLimit` in `Stream/Channel/Append.hs`).
`parConcatMap (ordered True . maxThreads 1 . maxYields (Just 4))` over
`[[1,2,3],[4,5,6]]` yields 6 elements instead of 4.

Proposal: account the yields of a stream when its entry is put on the heap,
or stop draining the heap when the limit is reached and leave the remaining
entries for the consumer to discard.

## toParserK uses Partial inside alternatives

The element adapter used by `toParserK` returns `Partial` instead of
`Continue` inside `<|>`, so `(ka *> kb) <|> (ka *> kc)` on `"ac"` throws
"backtracking beyond backtrack buffer". Using `Continue` fixes the
backtracking but buffers the input without bound. `Array.adaptCWith` always
uses `Continue`. A decision is needed on whether ParserK alternatives
should guarantee backtracking at the cost of unbounded buffering.

## Rolling and buffered scans

* `Scanl.rollingMap` and `rollingMapM` have no initial value, their
  `extract` before the first input raises an exception, so they work only
  as postscans. A fix needs an initial value argument (a type change).
* `Scanl.topBy`, `top`, `bottomBy` and `bottom` emit their mutable buffer;
  later inputs modify outputs already emitted. Cloning the buffer on
  `extract` makes the outputs independent at the cost of one allocation per
  input.

## Path equality and normalisation with ignoreCase

`eqPath`, `normalise` and `stripPrefix` with `ignoreCase True` decode the
path to compare or fold case. A Posix path that is not valid UTF-8, or a
Windows path with a lone surrogate, raises an exception instead of being
compared byte-wise. A decision is needed on whether to fall back to byte
comparison for undecodable paths.
