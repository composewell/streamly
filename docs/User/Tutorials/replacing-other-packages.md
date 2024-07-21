# All your packages are belong to us!

<!--
Bring the docs from streamly-core/streamly cabal package description here.
-->

Streamly offers a wide range of functionality using consistent, concise,
modular interface and high performance APIs. It has been designed from
scratch in such a way that minimum abstractions are used to cover
maximum functionality with highest possible performance. It unifies
functionality from many disparate packages on hackage and with better
performance in most cases. Streamly is the Swiss army knife for the
Haskell programmer.

Note that streamly does not forcefully strive to provide functionality
that is already available in other packages, that is not at all the
goal. The goal has been to design basic building blocks which naturally happen
to provide as much functionality as possible in a unified and concise
manner with highest possible performance. The basic idea is what Haskell
is actually meant for, do not repeat yourself (DRY). Unfortunately the
idea of DRY does not manifest in the Haskell library ecosystem, streamly
tries to rectify that problem.

Here we list some packages whose functionality streamly subsumes. If you
are looking for streamly functionality similar to an existing package,
this guide can help you find it. If you are starting a new project it
may be good idea to just use streamly instead of depending on a large
assortment of packages, and get a better performance to boot.

## Lists and Strings

The streamly modules `Streamly.Data.Stream`, `Streamly.Data.Fold` cover
the entire `Data.List` functionality as well as split and string search
functionality. String interpolation is provided by `Streamly.Unicode.String`.

| Package        | Streamly Type | Combinators                  |
|----------------|---------------|------------------------------|
| base/Data.List | Stream        |                              |
| interpolate    |               | str                          |
| split          | Stream, Fold  | splitOn, takeEndBy etc       |
| stringsearch   | Stream, Fold  | splitOnSeq, takeEndBySeq etc |

Note that `Data.List` can be easy to use and just enough in many use
cases. However, it can be limiting in other cases e.g. if you have to
interleave IO effects such as tracing or debug prints. With streams you
get the same interface as lists but you can add debug prints or any other IO
effects wherever you want. Also, you do not have to buffer the entire
data in memory when interleaving IO effects. Also, the performance of
streamly streams is better than lists when multiple transformations are
mixed together.

`splitOnSeq` and `takeEndBySeq` in Streamly use the Rabin-Karp algorithm
for fast string search, the performance was measured to be similar to
the Rust ripgrep performance.

## Streaming

The streamly modules `Streamly.Data.Stream`, `Streamly.Data.Scan` and
`Streamly.Data.Fold` cover the streaming functionality with superior
performance compared to any other packages.

| Package        | Streamly Type                     |
|----------------|-----------------------------------|
| streaming      | Stream, Scan, Fold                |
| pipes          | Stream, Scan, Fold                |
| conduit        | Stream, Scan, Fold                |
| foldl          | Scan, Fold                        |

## List Transformer and Logic Programming

A newtype wrapper over the `Stream` type provides list-t and logic-t
functionality with superior performance.

The relevant modules in streamly are:

* Streamly.Data.Stream
* Streamly.Data.Stream.MkType
* Streamly.Data.StreamK

| Package        | Streamly Type                     |
|----------------|-----------------------------------|
| list-t         | CrossStreamK, CrossStream, MkType |
| logic-t        | CrossStreamK, CrossStream, MkType |

`MkType` can be used to create the desired types with the required
monoid, applicative and monad behavior.

## Parsers

Streamly parsers can use any input type not just byte array, provide
a native streaming interface and same or better performance than
attoparsec.

The relevant modules in streamly are:

* `Streamly.Data.Parser`
* `Streamly.Unicode.Parser`

| Package        | Streamly Type           |
|----------------|-------------------------|
| attoparsec     | Fold, Parser, ParserK   |

## Arrays

Arrays in streamly can be mutable or immutable, boxed or unboxed, pinned
or unpinned. Thus they unify the functionality of bytestring, text and
vector into a single type. The API is integrated with streams, it is
very concise because all the processing is done via streams.

The relevant modules in streamly are:

* `Streamly.Data.Array`: immutable, unboxed, pinned, unpinned arrays
* `Streamly.Data.Array.Generic`: immutable, boxed arrays
* `Streamly.Data.MutArray`: mutable, unboxed, pinned, unpinned arrays
* `Streamly.Data.MutArray.Generic`: mutable, boxed arrays
* `Streamly.Internal.Data.Ring`: unboxed ring buffer
* `Streamly.Internal.Data.Ring.Generic`: boxed ring buffer

| Package           | Streamly Type           |
|-------------------|-------------------------|
| array             | Array, MutArray         |
| primitive         | Array, MutArray         |
| vector            | Array, MutArray         |
| vector-algorithms | MutArray                |
| ring-buffer       | Ring                    |
| bytestring        | Array                   |
| text              | Array                   |
<!--| text              | Array, Utf8             | -->

Note that there is no concept of Lazy in streamly like there is in
bytestring (Lazy bytestring) and text (Lazy text), the replacement of
that is stream processing.

The `streamly-bytestring` package provides interoperation of
streamly arrays with bytestring. `streamly-text` package provides
interoperability with `text`.

## Serialization

Fast serialization and deserialization is provided via `Unbox`, `Serialize`
type classes and `MutByteArray`, `MutArray` and `Array` types.

The relevant modules in streamly are:

* `Streamly.Data.MutByteArray`
* `Streamly.Data.MutArray`
* `Streamly.Data.Array`

| Package | Streamly Type                                             |
|---------|-----------------------------------------------------------|
| binary  | `Unbox`, `Serialize`, `MutByteArray`, `MutArray`, `Array` |
| cereal  | `Unbox`, `Serialize`, `MutByteArray`, `MutArray`, `Array` |
| store   | `Unbox`, `Serialize`, `MutByteArray`, `MutArray`, `Array` |

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

## Clock and Time

Streamly provides clock, time and timer APIs for high-performance
streaming use.

| Package | Streamly Module                     |
|---------|-------------------------------------|
| time    | `Streamly.Internal.Data.Time.Clock` |
|         | `Streamly.Internal.Data.Time.Units` |
| clock   | `Streamly.Internal.Data.Time.Clock` |

## Concurrency

Streamly is built for concurrency, it provides native concurrent
evaluation of streams, scans and folds. It also provides time related
streaming combinators for functional reactive programming.

The relevant modules in streamly are:

* Streamly.Data.Stream.Prelude
* Streamly.Data.Fold.Prelude

| Package               | Streamly Type  | Combinators      |
|-----------------------|----------------|------------------|
| async                 | Stream         | parConcatMap etc |
| pipes-concurrency     | Stream         | parConcatMap etc |
| streaming-concurrency | Stream         | parConcatMap etc |

## File System

Streamly provides native, high-performance streaming APIs for file
system operations. The file path representation enables high-performance
directory traversal (faster than Rust fd). The path modules allow
gradual typing, you can choose to use untyped path, absolute vs relative
distinction, file vs dir distinction or all four.

The relevant modules in streamly are:

* `Streamly.Console.Stdio`
* `Streamly.FileSystem.Dir`
* `Streamly.FileSystem.File`
* `Streamly.FileSystem.Handle`
* `Streamly.Internal.FileSystem.Event`
* `Streamly.Internal.FileSystem.Event.Linux`
* `Streamly.Internal.FileSystem.Event.Darwin`
* `Streamly.Internal.FileSystem.Event.Windows`

| Package               | Streamly Module                            |
|-----------------------|--------------------------------------------|
| base/System.IO        | Streamly.FileSystem.File,Handle            |
| filepath              | Streamly.FileSystem.Path                   |
| path                  | Streamly.Internal.FileSystem.Path.LocSeg   |
| path                  | Streamly.Internal.FileSystem.Path.FileDir  |
| path                  | Streamly.Internal.FileSystem.Path.Typed    |
| directory             | Streamly.FileSystem.Dir                    |
| fsnotify              | Streamly.Internal.FileSystem.Event         |
| fsnotify              | Streamly.Internal.FileSystem.Event.Windows |
| hinotify              | Streamly.Internal.FileSystem.Event.Linux   |
| hfsevents             | Streamly.Internal.FileSystem.Event.Darwin  |

## Network

Streamly provides native streaming APIs for network operations.

The relevant modules in streamly are:

* `Streamly.Network.Socket`
* `Streamly.Network.Inet.TCP`

| Package        | Streamly Module           |
|----------------|---------------------------|
| network        | Streamly.Network.Socket   |
|                | Streamly.Network.Inet.TCP |

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
