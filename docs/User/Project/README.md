# [Streamly][]: Idiomatic Haskell with C-Like Performance

[![Gitter chat](https://badges.gitter.im/composewell/gitter.svg)](https://gitter.im/composewell/streamly)
[![Hackage](https://img.shields.io/hackage/v/streamly.svg?style=flat)](https://hackage.haskell.org/package/streamly)

## Upgrading to 0.9.0+

Please read the [Streamly 0.9.0 Upgrade Guide](/docs/User/Project/Upgrading-0.8-to-0.9.md).

## Overview

Streamly is a powerful Haskell library that provides developers with
the essential building blocks to create safe, scalable, modular, and
high-performance software. With Streamly, developers can enjoy the
benefits of Haskell's type safety while leveraging C-like program
performance.  Streamly offers a comprehensive range of features,
comprising:

* Haskell's strong type safety.
* C-program-like performance capabilities.
* Flexible, modular building blocks.
* Idiomatic functional programming.
* Fearless, declarative concurrency for seamless parallel execution.
* A collection of ecosystem libraries for fast and efficient development.

Check out the [Streamly Getting Started
Guide](/docs/User/Tutorials/getting-started.md) and [Quick
Overview](/docs/User/Tutorials/quick-overview.md) for an introduction
to the library. For more detailed documentation, visit the [Haskell
Streamly website][Streamly].

## Blazing Fast

Streamly delivers C-like speed in Haskell by fusing stream pipelines
using the stream-fusion technique, resulting in compiled code that is
equivalent to handwritten C code, eliminating intermediate allocations
and function calls.

For a comprehensive comparison of Streamly to other Haskell streaming
libraries, check out our [streaming benchmarks][streaming-benchmarks]
page. In fact, Streamly's fused loops can be up to 100 times faster than
those of libraries without stream fusion.

## Declarative Concurrency

Streamly introduces declarative concurrency to standard functional
streaming abstractions.  Declarative concurrency abstracts away the
low-level details of concurrency management, such as locks and threads,
and allows for easier and safer parallelization of code.  For example,
with Streamly you can do things like repeat actions concurrently to
generate a stream of results, map functions concurrently on a stream,
and combine multiple streams concurrently to create a single output
stream.

## Unified API

Streamly provides a comprehensive and unified API for basic programming
needs, covering a wide range of areas including streaming, concurrency,
logic programming, reactive programming, pinned and unpinned arrays,
serialization, builders, parsers, unicode processing, file-io, file
system events, and network-io. By unifying functionality from disparate
Haskell libraries, Streamly simplifies development while delivering
equivalent or improved performance. Additionally, the complexity
of handling combinations of lazy, strict, bytestring, and text is
eliminated by using streams for lazy evaluation, and by generalizing
bytestring and text to arrays.

Check out [Streamly's documentation][Streamly] for more information
about Streamly's features.

## Batteries Included

In addition to the fundamental programming constructs, Streamly also
provides higher-level functionality through supporting packages such as
[streamly-process][], [streamly-shell][], and [streamly-coreutils][]
that are essential for general programming tasks. Check out the
[streamly-examples][] repository for some program snippets.

## Highly Modular

Traditionally, you must choose between modularity and performance when
writing code. However, with [Haskell Streamly][Streamly], you can have
the best of both worlds. By taking advantage of GHC's stream fusion
optimizations (such as `case-of-case` and `spec-constr`), Streamly achieves
performance comparable to an equivalent C program while still allowing
for highly modular code.

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
[streamly-examples]: https://github.com/composewell/streamly-examples
[streamly-process]: https://github.com/composewell/streamly-process
[streamly-shell]: https://github.com/composewell/streamly-shell
[streamly-coreutils]: https://github.com/composewell/streamly-coreutils

<!--
Keep all the unstable links here so that they can be updated to stable
links (for online docs) before we release.
-->

<!-- local files -->
[LICENSE]: /LICENSE
[CONTRIBUTING.md]: /docs/Developer/Contributing.md
[docs]: docs/
