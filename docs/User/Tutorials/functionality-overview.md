# Functionality At a Glance

## Overview

Streamly allows the programmer to write high-performance code
concisely and idiomatically using high level constructs, and with high
expressivity.

### Streams

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

### Arrays

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
represented by streamly arrays.

### Declarative Concurrency

Streamly introduces concurrency to the streaming paradigm preserving
the modularity and composability of serial composition. It enables
the programmer to write concurrent programs in a declarative manner.
It enables the programmer to write concurrent applications without
being aware of threads or synchronization. No explicit thread pools
are needed. The degree of concurrency can be automatically adjusted
dynamically based on the demand by the consumer.

Arrays are processed using streams and streams are concurrent,
therefore, arrays can be processed concurrently.

### Reactive Programming

Streaming and concurrency together enable expressing reactive applications
conveniently. See the `CirclingSquare` example in [Streamly
Examples](https://github.com/composewell/streamly-examples) for a simple SDL
based FRP example. To summarize, streamly provides a unified computing
framework for streaming, non-determinism and functional reactive programming in
an elegant and simple API that is a natural extension of pure lists to monadic
streams.

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

### Batteries Included

As we discussed above the core abstractions in streamly are streams
and arrays. For basic programming needs we also need console IO, file
IO, network IO and unicode text processing. All this functionality is
provided by streamly using the core streaming and array abstractions.

## Packages and Modules

Streamly is a self sufficient, batteries included library for general
purpose programming.  It has been designed for ease of use and high
performance.

Streamly consists of two packages: "streamly-core" and "streamly".
[streamly-core](https://hackage.haskell.org/package/streamly-core)
provides basic features, and depends only on GHC boot libraries, while
[streamly](https://hackage.haskell.org/package/streamly) provides
higher-level features like concurrency, time based streaming
combinators, lifted exceptions, and streaming network operations.

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

## Streams and Arrays

In streamly there are two fundamental data structures, streams and
arrays. They are computing duals of each other, streams are for dataflow
style processing while arrays are for storing data.  Both taken together
are powerful tools for general purpose programming in a functional or
dataflow style.

## Streams

In functional programming, stream processing paradigm is a higher level
alternative to the low level looping paradigm found in imperative
programming. The `Stream` abstraction in streamly represents data as a
sequence of items of the same type.  Functional operations are used to
process and transform each item in the stream to a new stream of the
same or different type.

### Stream Type

Following is a contrived example which generates a stream consisting of a
sequence of integers, then increments each one by 1, takes the first two
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

Folds can be composed using combinators, for example, the `teeWith` combinator
combines two folds such that the input of the resulting fold is passed through
both of them.

```haskell
f = Fold.teeWith (,) Fold.sum (Fold.lmap (\x -> x * x) Fold.sum)
main =
    Stream.enumerateFromTo 1 3 -- Stream IO Int
      & Stream.fold f          -- IO Int
      >>= print                -- IO ()
```

See "Streamly.Data.Fold" module.

### Scan Type

The `Scan` type represents a stateful transformation from a stream
to another stream. As a contrived example to demonstrate the basic
functionality of scans let us compute the expression `x^4 + 3x^2 + 4` for
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
exactly the same as serializing the type, and reading from them is
deserializing it.

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

## Concurrency and Time

Concurrency and time operations can be found in the `streamly` package.
Stream operations can be performed concurrently by using the concurrent
combinators.

### Concurrent Streams

The following example uses `parMapM` which is a concurrent version of `mapM`,
consequently it prints a value every second even though there is a 2 second
serial delay for each element.

```haskell
>>> let delay n = threadDelay (n * 1000000) >> return n
>>> :{
parMap =
      Stream.repeatM (delay 1)
    & Stream.parMapM (Stream.ordered True) (\x -> delay 1 >> print x)
    & Stream.fold Fold.drain
:}
```

See `Streamly.Data.Stream.Prelude` module.

### Concurrent Folds

The following example evaluates each fold in a separate thread, therefore, even
though each fold introduces a serial delay of 1 second, the total delay is
still only 1 second instead of 2 seconds.

```haskell
>>> import qualified Streamly.Data.Fold.Prelude as Fold
>>> p x = delay 1 >> print x
>>> f1 = Fold.parEval id (Fold.drainMapM p)
>>> f2 = Fold.parEval id (Fold.lmap (\x -> x * x) (Fold.drainMapM p))
>>> f = Fold.teeWith (\_ _ -> pure ()) f1 f2
>>> :{
parFolds =
    Stream.enumerateFromTo 1 3 -- Stream IO Int
      & Stream.fold f          -- IO ()
:}
```

See `Streamly.Data.Fold.Prelude` module.

### Time Domain Operations

Streamly provides time domain stream operations for reactive programming
including event sampling operations like throttle and debounce.

For example, the `intervalsOf` operation collapses stream elements within a
specified time interval.

```haskell
>>> input = Stream.parEval (Stream.constRate 2) $ Stream.enumerateFrom 1
>>> intervals = Stream.intervalsOf 1 Fold.toList input
>>> Stream.fold Fold.toList $ Stream.take 2 intervals
```

See `Streamly.Data.Fold.Prelude` module.

## Console IO

The `Streamly.Console.Stdio` module provides facilities to read a stream
from stdin and to write a stream to stdout and stderr.  

Implementation of a console echo program:

```haskell
main =
  Stdio.readChunks     -- Stream IO (Array Word8)
    & Stdio.fromChunks -- IO ()
```

An example to read two numbers from separate lines on stdin and sum them:

```haskell
main =
    Stdio.readChars                           -- Stream IO Char
      & Stream.splitOn (== '\n') Fold.toList  -- Stream IO String
      & Stream.map read                       -- Stream IO Int
      & Stream.take 2                         -- Stream IO Int
      & Stream.fold Fold.sum                  -- IO Int
      >>= print                               -- IO ()
```

## File IO

Implementation of the Unix `cp` utility to copy `input.txt` to `output.txt`:

```haskell
main =
  File.readChunks (Path.fromString "input.txt")      -- Stream IO (Array Word8)
    & File.fromChunks (Path.fromString "output.txt") -- IO ()
```

Implementation of the Unix `cat` utility to read all files from
the current directory on standard output.

```haskell
main =
  Dir.read (Path.fromString ".")       -- Stream IO Path
    & Stream.concatMap File.readChunks -- Stream IO (Array Word8)
    & Console.putChunks -- IO ()
```

## Network IO

Streaming network operations can be found in `Streamly.Network.*` modules. Here
is a streaming implementation of a concurrent network server which echo's back
whatever it receives.

```haskell
>>> ...
>>> import qualified Streamly.Network.Inet.TCP as TCP
>>> import qualified Streamly.Network.Socket as Socket
>>> :{
main :: IO ()
main =
      TCP.accept 8091                            -- Stream IO Socket
    & Stream.parMapM id (handleExceptions echo)  -- Stream IO ()
    & Stream.fold Fold.drain                     -- IO ()
    where
    echo :: Socket -> IO ()
    echo sk =
          Socket.readChunksWith 32768 sk      -- Stream IO (Array Word8)
        & Stream.fold (Socket.writeChunks sk) -- IO ()
    handleExceptions :: (Socket -> IO ()) -> Socket -> IO ()
    handleExceptions f sk = finally (f sk) (Net.close sk)
:}
```

## Unicode Operations

The `Streamly.Unicode.*` modules provide stream operations like UTF-8, UTF-16
encoding and decoding, splitting Unicode char streams into lines, words and
joining lines and words etc.

String quasiquoter:

```haskell
>>> [str|this is a string|]
"this is a string"
```

String interpolation:

```haskell
>>> x = "interpolated"
>>> [str|this is an #{x} string|]
"this is an interpolated string"
```

Splitting a string into lines, each line is collected in a list fold and
all the lines are collected in a list fold.:

```haskell
>>> Stream.fold Fold.toList $ Unicode.lines Fold.toList (Stream.fromList "lines\nthis\nstring\n\n\n")
["lines","this","string","",""]
```

You could supply any fold to consume the lines in a different way, for example
use `Fold.length` to print the lengths of the lines:

```haskell
>>> Stream.fold Fold.toList $ Unicode.lines Fold.length (Stream.fromList "lines\nthis\nstring\n\n\n")
[5,4,6,0,0]
```

Decoding the contents of a file into a stream of Unicode chars:

```haskell
>>> File.readChunks (Path.fromString "input.txt") & Unicode.decodeUtf8Chunks & Fold.length
```

Streamly.Unicode.Parser provides Unicode char and sequence parsers:

```haskell
>>> Stream.parse Unicode.double . Stream.fromList "3.14"
>>> Right 3.14
```
