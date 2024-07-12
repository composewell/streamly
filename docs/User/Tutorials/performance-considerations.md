# Performance Considerations

<!-- TBD: move cps-vs-direct.md here -->

## Streams

From performance behavior perspective, the stream operations can be divided
into three categories:

* Byte level loops: Fused streams as the lowest level building blocks.
* Chunk level loops: CPS streams as scalable wrappers to wrap fused streams.
* Thread level loops: Byte or chunk level loops evaluated in multiple threads.

| Fused  | CPS     | Concurrent Combinators       |
|--------|---------|------------------------------|
| Stream | StreamK | Streamly.Data.Stream.Prelude |
| Fold   | ParserK | Streamly.Data.Fold.Prelude   |
| Parser | ParserK |                              |
<!-- | Scan   | PipeK   | Streamly.Data.Scan.Prelude   | -->

## Fused Streams

The fused stream types provide the statically optimized loops giving the
highest performance with no function calls, thus no memory allocations for
wrapping constructors. Such loops provide C like performance.

## CPS Streams

In many cases you cannot determine the loops statically, in which case a
function call overhead or constructor allocation cannot be avoided. Note
this is not Haskell specific, even in C such cases would require a
function call overhead.  For such cases we have stream types which use
function composition rather than constructor fusion. In general we use
such streams to generate large chunks of data which is then processed by
fused streams loops embedded in CPS streams. Because of larger chunks
the number of function calls, therefore, the function call overhead due
to CPS streams is reduced.

## Concurrent Combinators

Concurrent streams use concurrency channels to evaluate streams
concurrently.  Concurrent combinators are provided corresponding to most
serial stream combinator for concurrent evaluation.  These combinators
consume fused or CPS streams, process them in a multithreaded manner and
generate a fused or CPS stream where the output is a stream.

## Arrays

Streams are for processing data, therefore, performance of streams
mostly involves CPU optimization.  Arrays are for storing data,
therefore, performance of arrays mostly involves memory aspects e.g.
boxed or unboxed, pinned or unpinned.

For highest performance we recommend the use of unboxed arrays. Unboxed
arrays store data without an additional heap pointer wrapper
(boxing). The arrays in the following modules are unboxed arrays:

* Streamly.Data.Array
* Streamly.Data.MutArray
* Streamly.Internal.Data.Ring

For storing boxed heap objects, boxed arrays are provided in the following
modules:

* Streamly.Data.Array.Generic
* Streamly.Data.MutArray.Generic
* Streamly.Internal.Data.Ring.Generic

Unboxed arrays can be pinned (cannot be moved by GC) or
unpinned. However, pinned or unpinned nature of the memory is not
statically typed in the array type. The same array may be created in
pinned or unpinned memory, an unpinned array can be dynamically moved
to pinned memory or vice-versa. For this purpose pinned or unpinned
versions of functions are provided in the unboxed array modules.
