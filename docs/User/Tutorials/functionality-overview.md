# Functionality At a Glance

Streamly allows the programmer to write high-performance code
concisely and idiomatically using high level constructs, and with high
expressivity.

## Streams

**Streaming**: Streamly is a general purpose computing framework
based on data flow programming paradigm also known as the streaming
paradigm.  Streaming enables writing modular and composable applications
declaratively.

**High Performance**: Streamly focuses on high performance in all
areas comparable to low programming languages like C. To achieve that
streamly uses a GHC optimization popularly known as stream fusion. All
abstractions in streamly are designed for stream fusion. It makes sure
that stream fusion works reliably. The abstractions in streamly also
allow nested stream fusion, for example, the `Unfold` abstraction is
specifically designed for nested fusion (an alternative to `concatMap`)
which does not fuse.

**Unified Abstractions**: Furthermore, streamly provides a range
of unified streaming abstractions for representing real life
applications. In general, it includes stream producers and consumers. In
particular, streaming abstractions include `Stream` representing the
stream producers, `Scan` the stream transformers, `Fold` representing
the stream consumers, `Parser` representing the stream consumers with
failure and backtracking. All these abstractions are unified, interwork
with each other, they are all designed based on the same underlying
principles. All other functionality in streamly and applications based
on streamly are designed based on these fundamental abstractions.

**Concise**: The functionality of lists, list-transformer,
logic-programming, streaming, streaming folds, parsers which is covered
by numerous library in the Haskell ecosystem are all represented
efficiently, with the highest possible performance with just these few
abstractions.

**Interworking**: Streamly streams can be converted to and from other streaming
types in the Haskell ecosystem. See the [interop
examples](https://github.com/composewell/streamly-examples/tree/master/examples/Interop)
in the [streamly-examples](https://github.com/composewell/streamly-examples)
repository.

## Arrays

**Arrays**: Arrays complement streams. While streams are used for
in-flight data processing, arrays are used for storing data-at-rest
and for random access. If you look carefully, the core functionality
in streamly is _only_ streams and arrays, the remaining part is just
high level functionality built on these two. All you need to build
any application is streams and arrays. Streamly unifies these two
fundamental concepts, they are intertwined, some stream functionality
requires arrays and some array functionality requires streams. That is
also the reason why we cannot separate these two cleanly in different
packages.

**High Performance**: Similar to streams, arrays are designed for high
performance. While arrays also provide native and high-performance
operations utilizing the random and chunked access nature of arrays,
in most cases they can be processed and transformed efficiently using
streams. Thus, providing a concise API utilizing streams. Unboxed arrays
provide the highest performance.

**Unified Abstractions**: Streamly provides a wide range of abstractions
using arrays to express all types of applications. Unboxed arrays
provide the highest performance whereas boxed arrays provide more
flexibility. Immutable arrays (the `Array` type) guarantee that the data
does not change whereas mutable arrays (the `MutArray` type) provide
in-place mutation for performance where needed. Pinned arrays provide
interfacing with the OS whereas unpinned arrays provide freedom from
fragmentation of memory. The `Unbox` and `Serialize` type classes assist
in high-performance serialization of Haskell data to and from arrays.

**Concise**: The functionality of array processing and serialization
which is covered by numerous library in the Haskell ecosystem are all
represented efficiently, well-integrated with streams, with the highest
possible performance, and in a concise and unified way. Especially, the
functionality of `bytestring`, `text` and `vector` packages can all be
represented by the single `Array` type in streamly and `MutArray` if you
want them to be mutable.

**Interworking**: Streamly arrays can be converted to and from
other popular array types (e.g. bytestring, text and vector) in
the Haskell ecosystem at zero cost i.e. without copying. See the
[streamly-bytestring](https://github.com/psibi/streamly-bytestring)
repository.
<!-- TODO add streamly-text and streamly-vector repos -->

## Declarative Concurrency

Streamly introduces concurrency to the streaming paradigm preserving
the modularity and composability of serial composition. It enables the
programmer to write concurrent programs in a high-level declarative
manner, without using low level concurrency primitives like threads
and synchronization. No explicit thread pools are needed. The degree
of concurrency can be automatically adjusted dynamically based on the
demand by the consumer.

Arrays are processed using streams and streams are concurrent,
therefore, arrays can be processed concurrently.

## Reactive Programming

Streaming and concurrency together enable expressing reactive
applications conveniently.  See the `AcidRain` game in [Streamly
Examples](https://github.com/composewell/streamly-examples) for a simple
reactive programming example.  See the `CirclingSquare` example in
[Streamly Examples](https://github.com/composewell/streamly-examples)
for a simple SDL based reactive programming example. To summarize,
streamly provides a unified computing framework for streaming,
non-determinism and functional reactive programming in an elegant and
simple API that is a natural extension of pure lists to monadic streams.

<!--
## Why data flow programming?

If you need some convincing for using streaming or data flow programming
paradigm itself then try to answer this question - why do we use lists in
Haskell? It boils down to why we use functional programming in the first place.
Haskell is successful in enforcing the functional data flow paradigm for pure
computations using lists, but not for monadic computations. In the absence of a
standard and easy to use data flow programming paradigm for monadic
computations, and the IO monad providing an escape hatch to an imperative
model, we just love to fall into the imperative trap, and start asking the same
fundamental question again - why do we have to use the streaming data model?
-->

## Batteries Included

As we discussed above the core abstractions in streamly are streams
and arrays. For basic programming needs we also need console IO, file
IO, network IO and unicode text processing. All this functionality is
provided by streamly using the core streaming and array abstractions.
