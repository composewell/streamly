# Functionality Overview

Streamly is a self sufficient, batteries included library for general
purpose programming.  It has been designed for ease of use and high
performance.

Streamly consists of two packages: "streamly-core" and "streamly".
[streamly-core](https://hackage.haskell.org/package/streamly-core)
provides basic features, and depends only on GHC boot libraries, while
[streamly](https://hackage.haskell.org/package/streamly) provides
higher-level features like concurrency, time, lifted exceptions, and
networking.

The general data processing functionality in `streamly` can be divided
into following categories:

* Streams: for processing of in flight data
* Arrays: for in-memory storage of data at rest
* Serialization: for on-disk storage of data at rest
* Concurrency: for scaling your applications across CPUs

The essential domain specific functionality can be divided into the
following categories:

* Console IO: reading from and writing to stdin, stdout and stderr
* File system IO: for interfacing with the file system
* Network IO: for interfacing with the network
* Unicode: stream processing of Unicode text

## Streams

In functional programming, stream processing paradigm is a higher level
alternative to the low level looping paradigm found in imperative
programming. The `Stream` abstraction in streamly represents data as a
sequence of items of the same type.  Functional combinators are used to
process and transform each item in the stream to a new stream of the
same or different type.

### Stream Type

Following is a contrived example which generates a stream of a sequence
of integers, then increments each one by 1, takes the first two
elements, adds them and prints the result:

```haskell
main =
    Stream.enumerateFromTo 1 3 -- Stream IO Int
      & fmap (+1)              -- Stream IO Int
      & Stream.take 2          -- Stream IO Int
      & Stream.fold Fold.sum   -- IO Int
      >>= print                -- IO ()
```

See "Streamly.Data.Stream" module.

### Fold Type

Just like the `Stream` type represents a producer of a sequence of
items, the `Fold` type represents a consumer of a sequence of items of
the same type. A fold consumes a stream and returns a single value as
output. The `Fold.sum` function above is a `Fold` which consumes an
integer stream as input and returns their sum as output. The fold is
driven using the `Stream.fold` combinator.

See "Streamly.Data.Fold" module.

### Scan Type

The `Scan` type represents a stateful transformation from a stream
to another stream. As a contrived example to demonstrate the basic
functionality of scans lets compute the expression `x^4 + 3x^2 + 4` for
each number in a stream.

```haskell
scan1 = Scan.map (\x -> x * x)
scan2 = Scan.map (\x -> 3 * x)
scan3 = Scan.teeWith (+) scan1 scan2 -- Compute x^2 + 3x
scan4 = Scan.compose scan1 scan3     -- compute x^2 then pass it to scan3

main =
    Stream.enumerateFromTo 1 3             -- Stream IO Int
      & Stream.runScan scan4               -- Stream IO Int
      & fmap (+4)                          -- Stream IO Int
      & Stream.fold (Fold.drainMapM print) -- IO ()
```

`scan3` splits the computation into two parts, one part computes `x^2`
using `scan1` and the other part computes `3x` using `scan2` and then it
zips the two parts back into a single stream by summing them. `scan4`
first passes an element through `scan1` thus squaring it and then it
passes the result through `scan3`, the final result is
`x^4 + 3x^2`. Then we add 4 in the resulting stream and print each
element in the stream.

In general, using scans, we can split the stream into multiple streams
and perform different stateful computations in each branch and then
merge them back into a single stream.

See "Streamly.Data.Scan" module.

### Parser Type

The `Parser` type represents a stream consumer just like the `Fold` type
but it adds failure handling and backtracking of input on failure.

For example, to parse a sequence of digits:

```haskell
>>> import qualified Data.Char as Char
>>> decimal = Parser.takeWhile1 Char.isDigit Fold.toList
>>> Stream.parse decimal $ Stream.fromList "1234 is the number"
Right "1234"
>>> Stream.parse decimal $ Stream.fromList "this is the number"
Left (ParseError "takeWhile1: predicate failed on first element")
```

On failure we can return a default value:

```haskell
>>> Stream.parse (decimal <|> pure "0") $ Stream.fromList "this is the number"
Right "0"
```

See "Streamly.Data.Parser" module.

## Arrays

While streams are meant for sequential processing of in-flight data, arrays
are meant for storing data in memory with serial or random access. Processing
of the data stored by arrays is done using streams.

### Immutable Arrays

The `Array` type is used to represent immutable arrays which cannot be
modified in-place.

```haskell
>>> import qualified Streamly.Data.Array as Array
>>> arr = Array.fromList "hello"
>>> Array.length arr
5
>>> Array.getIndex 1 arr
Just 'e'
```

Arrays provide streaming interfaces. Like all other data in streamly, arrays
are transformed using stream transformations.

```haskell
>>> arr <- Stream.fold Array.create $ Stream.enumerateFromTo 1 (5 :: Int)
>>> show arr
"fromList [1,2,3,4,5]"
>>> arr1 <- Stream.fold Array.create $ fmap (+1) $ Array.read arr
>>> show arr1
"fromList [2,3,4,5,6]"
```

See "Streamly.Data.Array" and "Streamly.Data.Array.Generic" modules.

### Mutable Arrays

The `MutArray` type is used to represent mutable arrays which can be
modified in-place.  Mutable arrays also have a reserved capacity to grow
without reallocation.

```haskell
>>> import qualified Streamly.Data.MutArray as MutArray
>>> arr <- Stream.fold (MutArray.createOf 12) $ Stream.fromList "hello"
>>> arr1 <- MutArray.snoc arr ' '
>>> arr2 <- Stream.fold (MutArray.append (pure arr1)) $ Stream.fromList "world "
>>> MutArray.toList arr2
"hello world "
>>> MutArray.putIndex 11 arr2 '!'
>>> MutArray.toList arr2
"hello world!"
```

Since we allocated an array of 12 elements to begin with, `snoc` and
`append` operations do not reallocate the array, they just append to the
existing array.

See "Streamly.Data.MutArray" and "Streamly.Data.MutArray.Generic" modules.

## Serialization

### MutByteArray

The `MutByteArray` type is used to represent low level mutable byte
arrays which can be modified in-place. `Unbox` and `Serialize` type
classes use this data type to serialize Haskell data structures.

### Unbox

Unbox provides fast serialization of fixed size data types.
`deriveUnbox` can be used to automatically derive the instances of
`Unbox`. `Unbox` can also be derived using `Generic`.  `Unbox` type
class provides `pokeAt` and `peekAt` operations to serialize and
deserialize Haskell data types:

```haskell
>>> import qualified Streamly.Data.MutByteArray as MutByteArray
>>> import Data.Proxy (Proxy(..))
>>> arr <- MutByteArray.new 10
>>> MutByteArray.pokeAt 0 arr 'h'
>>> offset = MutByteArray.sizeOf (Proxy :: Proxy Char)
>>> MutByteArray.pokeAt offset arr (1234 :: Int)
>>> r :: Char <- MutByteArray.peekAt 0 arr
>>> r
'h'
>>> r1 :: Int <- MutByteArray.peekAt offset arr
>>> r1
1234
```

The `Array` and MutArray types are unboxed arrays, they require an
`Unbox` instance. Writing to an `Array` or `MutArray` is in fact
serializing the type, and reading from them is deserializing it.

### Serialize

`Serialize` provides fast serialization of any Haskell data types.
`deriveSerialize` can be used to automatically derive the instances
of `Serialize`.  `Serialize` type class provides `serializeAt` and
`deserializeAt` operations to serialize and deserialize Haskell data
types:

```haskell
>>> import qualified Streamly.Data.MutByteArray as MutByteArray
>>> arr <- MutByteArray.new 10
>>> offset <- MutByteArray.serializeAt 0 arr 'h'
>>> offset1 <- MutByteArray.serializeAt offset arr (1234 :: Int)
>>> (next, r :: Char) <- MutByteArray.deserializeAt 0 arr offset1
>>> r
'h'
>>> (next1, r1 :: Int) <- MutByteArray.deserializeAt next arr offset1
>>> r1
1234
```

The Array module also provides operations to serialize and deserialize a
Haskell data type to an `Array Word8`, using the `Serialize` type class:

```haskell
>>> import qualified Streamly.Data.Array as Array
>>> arr = Array.pinnedSerialize (1234 :: Int)
>>> Array.deserialize arr
1234
```

## Console IO

The `Streamly.Console.Stdio` module provides facilities to read a stream
from stdin and to write a stream to stdout and stderr.  Here is an
example to read two numbers from separate lines on stdin and sum them:

```haskell
main =
    Stdio.readChars                           -- Stream IO Char
      & Stream.splitOn (== '\n') Fold.toList  -- Stream IO String
      & Stream.map read                       -- Stream IO Int
      & Stream.take 2                         -- Stream IO Int
      & Stream.fold Fold.sum                  -- IO Int
      >>= print                               -- IO ()
```

## All your packages are belong to us

<!--
Bring the docs from streamly-core/streamly cabal package description here.
-->

Streamly covers a wide range of functionality with a consistent,
concise, modular interface and high performance APIs. It has been
designed from scratch in such a way that minimum abstractions are used
to cover maximum functionality with highest possible performance.

Here we list some existing packages whose functionality streamly
subsumes.

### Lists and Strings

The streamly modules `Streamly.Data.Stream`, `Streamly.Data.Fold` cover
the lists, string and splitting functionality with superior performance.

| Package        | Streamly Type | Module                  | Combinators |
|----------------|---------------|-------------------------|-------------|
| base/Data.List | Stream        | Streamly.Data.Stream    |             |
| interpolate    |               | Streamly.Unicode.String | str         |
| split          | Stream, Fold  | Streamly.Data.Stream    | splitOn etc |
| stringsearch   | Stream, Fold  | Streamly.Data.Stream    | splitOn etc |

### Streaming

The streamly modules `Streamly.Data.Stream`, `Streamly.Data.Scan` and
`Streamly.Data.Fold` cover the streaming functionality with superior
performance.

| Package        | Streamly Type                     |
|----------------|-----------------------------------|
| streaming      | Stream, Scan, Fold                |
| pipes          | Stream, Scan, Fold                |
| conduit        | Stream, Scan, Fold                |
| foldl          | Scan, Fold                        |

### List Transformer and Logic Programming

A newtype wrapper over the `Stream` type provides list-t and logic-t
functionality with superior performance.

| Package        | Streamly Type                     |
|----------------|-----------------------------------|
| list-t         | CrossStreamK, CrossStream, MkType |
| logic-t        | CrossStreamK, CrossStream, MkType |

### Parsers

Streamly parsers are more powerful, use a polymorphic type, provide
a native streaming interface and same or better performance than
attoparsec.

| Package        | Streamly Type  | Module                  |
|----------------|----------------|-------------------------|
| attoparsec     | Fold, Parser   | Streamly.Data.Parser    |
|                |                | Streamly.Unicode.Parser |

### Arrays

array, primitive, vector, vector-algorithms: Streamly.Data.Array/MutArray/Generic
bytestring : Streamly.Data.Array
text: Streamly.Data.Array

### Builders

DList - Streamly.Data.Fold toList fold coupled with addOne and addStream.
bytestring builder, vector builder: Data.Fold, Data.MutArray

### Serialization

binary, cereal, store: Streamly.Data.MutByteArray

### Concurrency

async: Streamly.Data.Stream.Prelude, Fold.Prelude
