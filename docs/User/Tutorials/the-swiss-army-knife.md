# Streamly: The Swiss Army Knife for Haskell

![All your base are belong to us!](./all-your-base.jpg)

<!--
# All your package are belong to us!
Bring the docs from streamly-core/streamly cabal package description here.
-->

Streamly provides a comprehensive suite of functionality through
a **consistent, concise, and modular interface**, backed by
**high-performance APIs**. Designed from the ground up, it **minimizes
abstractions** while **maximizing functionality and efficiency**. By
unifying features from many disparate Haskell packages on Hackage—and
often outperforming them—**Streamly acts as a Swiss army knife for
Haskell programmers**.

Importantly, Streamly does **not** aim to replicate functionality that
already exists elsewhere. Instead, it focuses on creating **foundational
building blocks** that naturally deliver **broad, unified, and concise
capabilities** with **peak performance**. This approach embodies the
Haskell principle of **“don’t repeat yourself” (DRY)**, addressing
a gap often overlooked in the Haskell library ecosystem.

Below, we provide an overview of Streamly’s functionality and show
how its modular building blocks unify disparate features into a single,
coherent framework. Streamly offers a unified alternative to several
existing packages. For new projects, it can reduce dependency bloat,
enable concise and expressive solutions to domain-specific problems, and
often deliver better performance out of the box.

## Streaming

Streamly offers **high-performance, feature-rich streaming** and data
flow programming operations. We are briefly mentioning some of the
key capabilities here among many others.

### Three Core Abstractions

Streamly provides three types of streams, each one allows transformation and
stateful processing of sequential streams of data but they differ in how they
can be composed:

- **Producers (`Stream`)** – these are essentially generators or
  source of data streams, multiple sources can be combined to create
  a single source.
- **Transformers (`Scanl`)** – these are essentially pipes connecting sources
  to sources or sinks, multiple scans can be combined together such that
  a source stream can be split over multiple pipes and the resulting
  streams can be the combined back into a single source stream.
- **Consumers (`Fold`)** – these are essentially final consumers or sinks of
  data, multiple consumers can be combined together into a single
  consumer such that a source stream can be distributed to all consumers
  and the results can be combined.

**Parsers** are a specialization of folds that support backtracking
and errors handling.  Folds and parsers are **first-class building
blocks** for stateful data processing. Grouping, chunking and many other
operations over streams, scans and folds are implemented using folds and
parsers — making the library extremely versatile, modular, and yet
high-performing due to stream fusion.

### Nested and Structured Stream Processing

Streamly supports **nested stream processing** (the functional
equivalent of nested loops) in several styles:

- **Depth-first** – inner loops run to completion before the next
    outer loop iteration.
- **Breadth-first** – all outer loop iterations advance one step at a
    time.
- **Fair interleaving** – inner and outer loops advance fairly in
    parallel.

With `concatIterate`, the output of a stream can be fed back into
its input, allowing recursion and control flow at the stream level.
It can be used for traversing **trees and graphs breadth-first or
depth-first**. `mergeMapWith` merges streams pairwise to form balanced
merge trees, for example you can merge sorted data from many files using
this combinator.

### Sliding Windows and Sessions

**Sliding windows** are an integral part of scans and folds,
enabling incremental window based computations for use cases
such as **time-series analysis, statistical metrics, and trading
indicators**—without recomputing the results over the entire dataset.

Elements in streams can be **classified by keys** to route them to
different sessions, fold each substream differently in its own session,
and generate a stream of end results from these sessions.

### Expressiveness and Performance

These modular abstractions allow complex problems to be expressed in
**succinct and idiomatic way**. For example, merge sort in Streamly
is implemented in just a few lines of idiomatic code using only
general-purpose building blocks—yet the performance matches or exceeds
the highly optimized list implementation in base.

Streamly provides a unified, expressive, and high-level alternative to
stream processing packages like `streaming`, `conduit`, `pipes`, and
`foldl`, with unmatched performance.

## Concurrency

Streamly is built for concurrency, all abstractions are designed for
concurrency, it provides native concurrent evaluation of streams, scans
and folds. All the ways in which stream processing pipelines can be
composed with serial processing can also be composed with concurrent
processing.  For example, you can:

- Map a function over a stream concurrently.
- generate multiple streams in parallel and merge the results in a single
  stream.
- Split streams, scan each branch concurrently, and combine them back
  into a single stream.
- Distribute a stream to different concurrent folds and merge back the results.
- Traverse trees or graphs, feed output streams back to input for
  recursive processing, concurrently.
- Perform time-based operations such as sampling, throttling, or debouncing.
- Group items over time intervals or intersperse actions in a stream
  periodically.

Concurrent streaming with time based operations can be used to implement
programs based on functional reactive programming model.  The operations
you can perform with Streamly often overlap with what libraries like
**Apache Flink** provide, but in a **lightweight, unified, and purely
functional way**.

With all these facilities streamly allows you to express your domain
specific problems much more succinctly and effortlessly. For example,
the concurrent directory traversal written with streamly is just a
few lines of code, built from low level basic building blocks, and
outperforms rust based implementations.

Streamly is much more higher level alternative to low level packages like
async, pipes-concurrency, streamling-concurrency etc. Take a look at the
`Streamly.Data.Stream.Prelude`, `Streamly.Data.Scanl.Prelude`,
`Streamly.Data.Fold.Prelude` modules in the streamly package.

## Reactive Programming

Streamly supports reactive (time domain) programming because of its support for
declarative concurrency. Please see the `Streamly.Data.Stream.Prelude`
module for time-specific and time based sampling combinators.

Reactive programming is modeled beautifully using concurrent streaming in
streamly. It involves generation of streams of events, merging concurrent
streams and processing events concurrently.  Streamly provides native
high-level facilities to do all this easily.

The examples [AcidRain.hs][] and [CirclingSquare.hs][] demonstrate
reactive programming using [Streamly][].

[AcidRain.hs]: https://github.com/composewell/streamly-examples/tree/master/examples/AcidRain.hs
[CirclingSquare.hs]: https://github.com/composewell/streamly-examples/tree/master/examples/CirclingSquare.hs

## Parsing

Parsing in Streamly is first-class, high-performance, and integrated
with streams.  Streamly treats parsers as core building blocks for
data processing. Many tasks—such as **sorting, chunking, and
structured transformations**—are expressed naturally with folds and
parsers. Parsers are not restricted to byte streams, any type of streams
can be parsed. It provides full-fledged parsing functionality equivalent
to the standard parser combinator libraries like `parsec`. Performance
is equivalent or better than some of the best performing libraries like
`attoparsec`.

## Prompt Resource Cleanup

One of the many challenges in a concurrent environment is safe,
correct and timely resource cleanup in all cases and races. Streamly
provides built-in prompt resource cleanup operations in a concurrent
environment. It provides basic IO level building blocks as well as
stream based resource management combinators, everything that you can do
with `resourcet` is built into the library.

## Lists and Strings

Streamly modules `Streamly.Data.Stream` and `Streamly.Data.Fold` cover
the entire functionality of `Data.List` from the `base` package. While
lists are sufficient and easy to use for many cases, it can be
problematic when you need to **interleave IO effects** such as tracing
or debug prints. Streamly streams provide the same interface as lists
but allow you to **inject IO effects anywhere** without buffering the
entire dataset in memory.  Additionally, **Streamly streams often
outperform lists** when combining transformations.

By virtue of its modularity it is able to express the string (stream)
splitting operations similar to the `split` package without any custom
implementation. In the same manner it implements search operations
similar to the `stringsearch` package.  For string search, `splitOnSeq`
and `takeEndBySeq` in Streamly use the **Rabin-Karp algorithm** for
fast pattern matching. Benchmarks show performance comparable to the
Rust `ripgrep` tool.  `Streamly.Unicode.String` also provides string
interpolation.

## Non-determinism and List Transformers

Streamly provides full list transformer functionality (e.g. equivalent
to the `ListT` from list-t package) and more.  It does not provide a
Monad instance for streams, as there are many possible ways to define
the bind operation.  Instead, it offers bind-style operations such as
'concatFor', 'concatForM', and their variants (e.g. fair interleaving
and breadth-first nesting). These can be used for convenient ListT-style
stream composition. Additionally, it provides applicative-style cross
product operations like 'cross' and its variants which are many times
faster than the monad style operations.

## Logic Programming

Streamly does not provide a 'LogicT'-style Monad instance, but it
offers comprehensive logic programming functionality.  Operations like
'fairCross' and 'fairConcatFor' nest outer and inner streams fairly,
ensuring that no stream is starved when exploring cross products.

This enables balanced exploration across all dimensions in backtracking
problems, while also supporting infinite streams without starvation. It
effectively replaces the core functionality of 'LogicT' from the
@logict@ package, with significantly better performance. In particular,
it avoids the quadratic slowdown seen with @observeMany@, and the
applicative 'fairCross' runs many times faster, achieving loop nesting
performance comparable to C.

## Arrays

Streamly provides comprehensive array functionality. Arrays are equal
partners to streams in general purpose programming.  Arrays in streamly
can be mutable or immutable, boxed or unboxed, pinned or unpinned. Thus
they unify the functionality provided by the `bytestring`, `text` and
`vector` packages. The API is integrated with streams, it is concise and
modular because all the processing is done via streams.  There is no
need of thinking about lazy bytestring vs strict bytestring, bytestring
vs text, text vs lazy text. Lazy in streamly is equivalent to stream
processing or stream of arrays, and strict is equivalent to arrays.

The `streamly-bytestring` package provides interoperation of
streamly arrays with `bytestring`. `streamly-text` package provides
interoperability with `text`.

Streamly also implements ring arrays which are essentially mutable
circular buffers. Ring Arrays help in implementing sliding windows
efficiently and conveniently.

Streamly can potentially replace most array like package for example
`array`, `primitive`, `vector`, `vector-algorithms`, `ring-buffer`,
`bytestring`.

## Serialization

Binary serialization in streamly is built into arrays. Unboxed arrays
are in fact store data in binary serialized form. The `Unbox` and
`Serialize` type classes provide fast binary serialization and
deserialization. The `MutByteArray`, `MutArray` and `Array` modules
provide functions that can serialize Haskell data types using the
`Unbox` and `Serialize` type classes.

Thus arrays in streamly are high performance alternative to
serialization packages like `binary`, `cereal`, `store` etc.

<!--
## Builders

Streams, folds, parsers and arrays are natural builders in streamly.
Folds are natural stream builders, mutable arrays are natural array
builders.

The fastest way to encode and decode Haskell data types is via
the `Unbox` and `Serialize` type classes. However, if you require
custom ways to encode or decode the Haskell data types, the fastest
way is to directly encode to or decode from `MutArray` using the
`Streamly.Internal.Data.Binary.MutArray` module (yet to be written).
Also, using `Streamly.Internal.Data.Builder` and yet to be written
serialize and deserialize monads.

Less efficiently, for custom encoding, you can build a stream
of encoded streams or possibly a stream of arrays using
the `Streamly.Internal.Data.Binary.Stream` module and then
flatten it.  For custom decoding you can build a parser using
`Streamly.Internal.Data.Binary.Parser` module.

| Package        | Streamly Type          | Combinators                              |
|----------------|------------------------|------------------------------------------|
| dlist          | Stream, Fold           | Fold.toList, Fold.addOne, Fold.addStream |
| bytestring     | Stream, Fold, MutArray |                                          |
-->

<!-- vector-builder -->

## File System

Streamly provides native, high-performance streaming APIs for file
system operations.  There is also a module for file path representation
which leverages streamly arrays for high performance and safe path
operations. Path module is designed to allow gradual typing, you can
choose to use untyped path, absolute vs relative distinction, file vs
dir distinction or all four. The typed versions are not released yet but
available as internal modules.

Paths and directory modules in streamly can be used as an alternative
for `filepath`, `directory`, and `path` packages.

## Network

Streamly provides native streaming APIs for network operations as a higher
level alternative to the `network` package functionality.

## Clock and Time

Streamly provides clock, time and timer APIs for high-performance
streaming use. See the `Streamly.Internal.Data.Time.Clock`,
`Streamly.Internal.Data.Time.Units` modules. These are currently
internal but will be released in a future release.

## Compatibility

Streamly can interwork with other packages in the Haskell ecosystem
providing similar functionality. Following is a list of examples or
packages providing interconversion:

<!-- TODO foldl interworking -->
<!-- TODO vector interworking -->

| Interop with   | Example, package                                                      |
|----------------|-----------------------------------------------------------------------|
| streaming      | [streamly-examples](https://github.com/composewell/streamly-examples) |
| pipes          | [streamly-examples](https://github.com/composewell/streamly-examples) |
| conduit        | [streamly-examples](https://github.com/composewell/streamly-examples) |
| bytestring     | [streamly-bytestring](https://github.com/psibi/streamly-bytestring)   |
| text           | [streamly-text](https://github.com/composewell/streamly-text)         |
| filepath       | [streamly-filepath](https://github.com/composewell/streamly-filepath) |

See the `streamly-ecosystem` chapter for more details.
