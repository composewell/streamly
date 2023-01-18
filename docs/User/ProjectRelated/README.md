# [Streamly][]: Idiomatic Haskell with the Performance of C

[![Gitter chat](https://badges.gitter.im/composewell/gitter.svg)](https://gitter.im/composewell/streamly)
[![Hackage](https://img.shields.io/hackage/v/streamly.svg?style=flat)](https://hackage.haskell.org/package/streamly)

Streamly is a Haskell library that provides building blocks to build
safe, scalable, modular and high performance software.  Streamly offers:

* The type safety of Haskell.
* The performance of C programs.
* Powerful building blocks for modular code.
* Idiomatic functional programming.
* Declarative, fearless concurrency.
* Ecossytem libraries for quick development.

Please read the [Streamly Setup and Usage
Guide](/docs/User/Tutorials/setup-and-usage.md) and [Streamly Quick
Overview](/docs/User/Tutorials/quick-overview.md) to get a taste of the
library. Streamly comes with comprehensive documentation, please visit
the [Haskell Streamly website][Streamly] for documentation.

## Performance with Modularity

Usually, you have to pick one of the two, performance or
modularity. Using [Haskell Streamly][Streamly] you can write highly
modular code and still achieve performance close to an equivalent
(imperative) C program.  Streamly exploits GHC's stream fusion
optimizations (`case-of-case` and `spec-constr`) aggressively to achieve
both modularity and performance at the same time.

Streamly offers excellent performance even for byte-at-a-time stream
operations using efficient abstractions like `Unfold`s and
`Fold`s.  Byte-at-a-time stream operations can simplify programming
because the developer does not have to deal explicitly with chunking
and re-combining data.

### GHC Plugin for Stream Fusion

[Streamly][] usually performs very well without any compiler plugins.
However, we have fixed some deficiencies in GHC's optimizer using a
[compiler plugin](https://github.com/composewell/fusion-plugin).  We
hope to fold these optimizations into GHC in the future; until then we
recommend that you use this plugin for applications that are performance
sensitive.

### Performance Benchmarks

We measured several Haskell streaming implementations
using various micro-benchmarks. Please see the [streaming
benchmarks][streaming-benchmarks] page for a detailed comparison of
Streamly against other streaming libraries.

Our results show that [Streamly][] is the fastest effectful streaming
implementation on almost all the measured microbenchmarks. In many cases
it runs up to 100x faster, and in some cases even 1000x faster than
some of the tested alternatives. In some composite operation benchmarks
[Streamly][] turns out to be significantly faster than Haskell's list
implementation.

*Note*: If you can write a program in some other way or with some other
language that runs significantly faster than what [Streamly][] offers,
please let us know and we will improve.

## Applications

Streamly comes equipped with a very powerful set of abstractions to
accomplish many kinds of programming tasks: it provides support for
programming with streams and arrays, for reading and writing from the
file system and from the network, for time domain programming (reactive
programming), and for reacting to file system events using `fsnotify`.

Please view [Streamly's documentation][Streamly] for more information
about Streamly's features.

## Design

### Design Goals

Our goals for [Streamly][] from the very beginning have been:

1. To achieve simplicity by unifying abstractions.
2. To offer high performance.

These goals are hard to achieve simultaneously because they are usually
inversely related.  We have spent many years trying to get the abstractions
right without compromising performance.

`Unfold` is an example of an abstraction that we have created to achieve
high performance when mapping streams on streams.  `Unfold` allows stream
generation to be optimized well by the compiler through stream fusion.
A `Fold` with termination capability is another example which modularizes
stream elimination operations through stream fusion.  Terminating folds
can perform many simple parsing tasks that do not require backtracking.
In Streamly, `Parser`s are a natural extension to terminating `Fold`s;
`Parser`s add the ability to backtrack to `Fold`s.  Unification leads
to simpler abstractions and lower cognitive overheads while also not
compromising performance.

### Concurrency Design

Streamly uses lock-free synchronization for achieving concurrent
operation with low overheads.  The number of tasks performed concurrently
are determined automatically based on the rate at which a consumer
is consuming the results. In other words, you do not need to manage
thread pools or decide how many threads to use for a particular task.
For CPU-bound tasks Streamly will try to keep the number of threads
close to the number of CPUs available; for IO-bound tasks it will utilize
more threads.

The parallelism available during program execution can be utilized with
very little overhead even where the task size is very small, because
Streamly will automatically switch between serial or batched execution
of tasks on the same CPU depending on whichever is more efficient.
Please see our [concurrency benchmarks][concurrency-benchmarks] for more
detailed performance measurements, and for a comparison with the `async`
package.

## Credits

The following authors/libraries have influenced or inspired this library in a
significant way:

  * Roman Leshchinskiy ([vector](http://hackage.haskell.org/package/vector))
  * Gabriella Gonzalez ([foldl](https://hackage.haskell.org/package/foldl))
  * Alberto G. Corona ([transient](https://hackage.haskell.org/package/transient))

Please see the [`credits`](/docs/User/ProjectRelated/Credits.md) directory for a full
list of contributors, credits and licenses.

## Licensing

Streamly is an [open source](https://github.com/composewell/streamly)
project available under a liberal [BSD-3-Clause license][LICENSE]

## Contributing to Streamly

As an open project we welcome contributions:

* [Streamly Contributor's Guide][CONTRIBUTING.md]
* [Contact the streamly development team](mailto:streamly@composewell.com)

## Getting Support

Professional support is available for [Streamly][]: please contact
[support@composewell.com](mailto:support@composewell.com).

You can also join our [community chat
channel](https://gitter.im/composewell/streamly) on Gitter.

<!--
Link References.
-->

[Streamly]: https://streamly.composewell.com/
[streaming-benchmarks]: https://github.com/composewell/streaming-benchmarks
[concurrency-benchmarks]: https://github.com/composewell/concurrency-benchmarks

<!--
Keep all the unstable links here so that they can be updated to stable
links (for online docs) before we release.
-->

<!-- local files -->
[LICENSE]: /LICENSE
[CONTRIBUTING.md]: /CONTRIBUTING.md
[docs]: docs/
