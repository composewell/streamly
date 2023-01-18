# Unified Functional Abstractions in Streamly

The goal of Streamly is to provide unified abstractions with high
performance.  The basic abstractions in Streamly are streams and arrays
and yet it provides the functionality of many packages in the Haskell
ecosystem.

This document discusses the related packages in the Haskell ecosystem
and how streamly unifies, overlaps, or compares with those. We provide
simple code snippets for illustrations, for a better overview of the
library please see the [Streamly Quick Overview](/docs/User/Tutorials/quick-overview.md) document.

## Existing Haskell Libraries

In the Haskell ecosystem effectful streaming functionality is provided
by several streaming libraries e.g. `streaming`, nested looping by list
transformers e.g. `list-t`, interleaved scheduling by `logict`,
concurrency by `async`, time-domain programming by FRP libraries like
`Yampa` and `reflex`, and array functionality by `bytestring`, `text`,
`vector`, and `arrays` packages.

For basic programming needs, one needs to discover and learn all of
these or roll something on their own. These libraries have evolved
independently for specialized needs. They are not designed with a big
picture in mind to eliminate duplicate functionality and to come up with
the most optimal abstractions on the whole. For example, there is no
reason why streaming, list-t, and logict cannot be provided by the same
implementation. Concurrency and reactive programming also naturally fit
with the streaming model and can be unified. `Bytestring`, `text`,
`vector`, and `arrays` all provide the same functionality which can
be unified under one umbrella of arrays. Moreover, there are many
of flavors some of these packages like lazy bytestring, Char8 bytestrings,
lazy text which can be eliminated by using streams instead.

On the performance side, existing streaming libraries lack stream fusion
leading to a function call overhead in each iteration of the loop,
therefore, providing orders of magnitude lower performance in tight
loops compared to writing a monolithic loop by hand. Similarly, `logict`
shows quadratic performance characteristics in some basic use cases.
These problems are solved by Streamly.

## How Streamly Unifies them?

Streamly unifies streaming, nested looping, scheduling, concurrency,
time, and array functionality using two basic constructs, namely streams
and arrays. Streams provide efficient, immutable, composable serial
processing capabilities for in-flight data, whereas arrays provide
efficient storage, mutability, and random access for data at rest.

Streamly builds all the functionality on top of these two building
blocks. It takes a big picture view of programming in general and
provides well-integrated APIs with the same look and feel. Moreover,
performance is a primary goal of streamly, all the functionality is
built for performance comparable to C.

Streamly streams are like lists and provide non-determinism just like
lists.  Streamly also adds asynchronicity and concurrency to
streaming composition.  This seemingly simple change unifies several
disparate abstractions into one powerful, concise, and elegant
abstraction.  A wide variety of programming problems can be solved
elegantly with this abstraction. In particular, it unifies three major
programming domains namely non-deterministic (logic) programming,
concurrent programming, and functional Reactive programming.

## Streaming

The basic, bare-bones functionality of Streamly is processing streams of
data.  In simple terms, stream processing is the functional equivalent
of loops in imperative programming.

Like Haskell lists, `vector`, and `streaming` packages, Streamly composes
streams of data rather than stream processor functions as in other
streaming libraries like `pipes` and `machines`. This makes the types
and API very simple and is very similar to Haskell lists which is
familiar to everyone. The fundamental difference is that Streamly adds
concurrency support but the good thing is that it does not change the
API.

This simple console echo program shows the simplicity of Streamly API
and its similarity with the list API:

```haskell
echo =
      Stream.repeatM getLine
    & Stream.mapM putStrLn
    & Stream.drain
```

Streamly uses dual representation for streams. On top it uses Scott
encoded CPS streams which provide a way to incrementally construct and
append streams efficiently. Under the good, for tight loops, Streamly
uses a vector-like stream representation which is amenable to
`case-of-case` and `SPEC constructor` optimizations by GHC resulting in
low-level code having performance comparable to C.

To further understand the similarity with list API, please see [Streamly vs.
lists](/docs/User/HowTo/streamly-vs-lists.md).
For comparison of Streamly performance with other libraries see
[streaming benchmarks](https://github.com/composewell/streaming-benchmarks).

## Non-determinism

Roughly speaking, non-determinism is a fancy term that functional
programmers use for what you call nested loops in imperative
programming. List transformers are the basic implementations for
non-determinism.

The stream monad in Streamly is a list-transformer with behavior similar
to the list monad. It provides the functionality provided by `list-t` or
`logict` packages for free.  Here is an example of nested looping using
the serial stream monad:

```haskell
import qualified Streamly.Prelude as S

loops = do
    x <- Stream.fromFoldable [1,2]
    y <- Stream.fromFoldable [3,4]
    Stream.fromEffect $ print (x, y)
```

Moreover, the list transformer in Streamly can be concurrent. The Scott
encoding of streams also avoids the quadratic performance issue of
`logict`.

## Scheduling Behaviors

When we execute a stream or combine two streams, the actions in the
stream need to be scheduled for execution.  Existing streaming libraries
are limited to just one form of scheduling of actions in the stream
which is serial execution. Streamly provides several ways of scheduling
the actions in the stream. The good thing is the API does not change, we
just need to use a combinator or a different type to change the
scheduling behavior.

In a single stream, the actions in the stream can be executed
concurrently with different concurrent scheduling behaviors.  Two or
more streams can be combined in several ways. For example, the actions
from two streams being combined can be interleaved.  Similarly, streams
can be interleaved with concurrent execution providing a fair,
concurrent round-robin scheduling of streams.

The monad instance provides a convenient way to combine streams in
different ways. It is just non-determinism with different flavors of
scheduling behavior.  The example from the previous section can be run
with interleaved scheduling behavior as follows, without changing the
code at all:

```haskell
main = Stream.drain $ Stream.fromWserial loops
```

Scheduling is fundamental to expressing many common programming problems
in an idiomatic functional manner.  One example application of
interleaving is breadth-first search mechanism for logic programming.
Please see [mini kanren implementation using streamly](https://github.com/composewell/ds-kanren).

## Declarative Concurrency

The same combinators that are used for serial streams e.g. 'unfoldrM',
'replicateM', 'repeatM' work concurrently when used at the appropriate type.
It allows concurrent programs to be written declaratively and composed
idiomatically. They are not much different than serial programs.  See
[Streamly vs async](/docs/User/HowTo/streamly-vs-async.md)
for a comparison of Streamly with the `async` package.

Streamly provides concurrent scheduling and looping similar to to
[OpenMP](https://en.wikipedia.org/wiki/OpenMP) and
[Cilk](https://en.wikipedia.org/wiki/Cilk) but with a more declarative
style.  The list transformer example can be run with concurrent
execution of loop iterations as follows, without changing the code at
all:

```haskell
main = Stream.drain $ Stream.fromAhead loops
```

And interleaving with concurrent execution of the loop iterations can be
written like this:

```haskell
main = Stream.drain $ Stream.fromWAsync loops
```

All this comes with no change in the streaming APIs.

## Reactive Programming

The combination of non-determinism, concurrency, and streaming makes
Streamly a strong reactive programming library as well. Reactive
programming fundamentally deals with streams of events that can be
processed concurrently. The [Acid Rain][AcidRain.hs] and
[Circling Square][CirclingSquare.hs]
examples demonstrate the basic reactive capability of Streamly.

In core concepts, Streamly is strikingly similar to `dunai`.  `dunai` was
designed from a FRP perspective and Streamly was originally designed
from a concurrency perspective. However, both have similarities at the
core.

## Arrays

Streamly provides immutable, mutable, pinned, unpinned, boxed, and
unboxed arrays with streaming interfaces.  The combination of efficient
streaming and polymorphic arrays lets it express the functionality of
`bytestring` and `text` packages as special cases of arrays with no loss
of performance. Since we can use explicit streaming, the lazy versions
of `bytestring` and `text` are not required.

## Conclusion

Streamly, provides effectful streams, with a simple API, almost
identical to standard lists, and in-built support for concurrency.
By using stream-style combinators on stream composition, streams can be
generated, merged, chained, mapped, zipped, and consumed concurrently –
providing a generalized high-level programming framework unifying
streaming and concurrency. Controlled concurrency allows even infinite
streams to be evaluated concurrently.  Concurrency is auto-scaled based
on feedback from the stream consumer.

Streamly is a programmer-first library, designed to be useful and
friendly to programmers for solving practical problems in a simple and
concise manner. Some key points that Streamly stresses are:

* _Simplicity_: Simple list like streaming API, if you know how to use lists
  then you know how to use Streamly. This library is built with simplicity
  and ease of use as a design goal.
* _Concurrency_: Simple, powerful, and scalable concurrency.  Concurrency is
  built-in, and not intrusive, concurrent programs are written exactly
  as non-concurrent ones.
* _Generality_: Unifies functionality provided by several disparate packages
  (streaming, concurrency, list transformer, logic programming, reactive
  programming) in a concise API.
* _Performance_: Streamly is designed for high performance. It employs stream
  fusion optimizations for the best possible performance. Serial performance is
  equivalent to the venerable `vector` library in most cases and even better
  in some cases.  Concurrent performance is unbeatable.  See
  [streaming-benchmarks](https://github.com/composewell/streaming-benchmarks)
  for a comparison of popular streaming libraries on micro-benchmarks.

## Appendix

Streamly unifies the functionality overlapping the following Haskell
libraries:

### Streaming

* [vector](https://hackage.haskell.org/package/vector)
* [streaming](https://hackage.haskell.org/package/streaming)
* [pipes](https://hackage.haskell.org/package/pipes)
* [conduit](https://hackage.haskell.org/package/conduit)

### List Transformers and Logic Programming

* [pipes](https://hackage.haskell.org/package/pipes)
* [list-t](https://hackage.haskell.org/package/list-t)
* [logict](https://hackage.haskell.org/package/logict)

### Concurrency

* [async](https://hackage.haskell.org/package/async)

### Reactive Programming

* [Yampa](https://hackage.haskell.org/package/Yampa)
* [reflex](https://hackage.haskell.org/package/reflex)

### Arrays

* [vector](https://hackage.haskell.org/package/vector)

<!-- Markdown Links -->

[AcidRain.hs]: https://github.com/composewell/streamly-examples/blob/master/examples/AcidRain.hs
[CirclingSquare.hs]: https://github.com/composewell/streamly-examples/blob/master/examples/CirclingSquare.hs
