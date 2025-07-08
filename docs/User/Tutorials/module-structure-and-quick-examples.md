# Packages and Modules

Streamly is a self sufficient, batteries included library for general
purpose programming.  It has been designed for ease of use and high
performance.

Streamly consists of two packages: "streamly-core" and "streamly".
[streamly-core](https://hackage.haskell.org/package/streamly-core)
provides basic features, and depends only on GHC boot libraries, while
[streamly](https://hackage.haskell.org/package/streamly) provides
higher-level features like concurrency, time based streaming
combinators, lifted exceptions, and streaming network operations.

In streamly there are two core data structures, streams and arrays. They
are computing duals of each other, streams are for dataflow style
processing while arrays are for storing data.  Both taken together
are powerful tools for general purpose programming in a functional or
dataflow style.

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
sequence of items of the same type.  Functional operations are used to
process and transform each item in the stream to a new stream of the
same or different type.

### Stream Type

Following is a contrived example which generates a stream consisting of a
sequence of integers, then increments each one by 1, takes the first two
elements, adds them and prints the result:

```haskell
import Data.Function ((&))
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Scanl as Scanl

main1 =
    Stream.enumerateFromTo 1 3    -- Stream IO Int
      & fmap ((+1) :: Int -> Int) -- Stream IO Int
      & Stream.take 2             -- Stream IO Int
      & Stream.fold Fold.sum      -- IO Int
      >>= print                   -- IO ()
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
f :: Monad m => Fold.Fold m Int (Int, Int)
f = Fold.teeWith (,) Fold.sum (Fold.lmap (\x -> x * x) Fold.sum)
main2 =
    Stream.enumerateFromTo 1 3 -- Stream IO Int
      & Stream.fold f          -- IO Int
      >>= print                -- IO ()
```

See "Streamly.Data.Fold" module.

### Scanl Type

The `Scanl` type represents a stateful transformation from a stream
to another stream. As a contrived example to demonstrate the basic
functionality of scans let us compute the expression `x^4 + 3x^2 + 4` for
each number in a stream.

```haskell
scan1 :: Monad m => Scanl.Scanl m Int Int
scan1 = Scanl.mkScanl (\_ x -> x * x) undefined

scan2 :: Monad m => Scanl.Scanl m Int Int
scan2 = Scanl.mkScanl (\_ x -> 3 * x) undefined

scan3 :: Monad m => Scanl.Scanl m Int Int
scan3 = Scanl.teeWith (+) scan1 scan2 -- Compute x^2 + 3x

scan4 :: Monad m => Scanl.Scanl m Int Int
scan4 = Scanl.postscanl scan1 scan3   -- compute x^2 then pass it to scan3

main3 =
    Stream.enumerateFromTo 1 3             -- Stream IO Int
      & Stream.scanl scan4                 -- Stream IO Int
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

```haskell docspec
>>> import Data.Function ((&))
>>> import qualified Data.Char as Char
>>> import qualified Streamly.Data.Stream.Prelude as Stream
>>> import qualified Streamly.Data.Parser as Parser
>>> import qualified Streamly.Data.Fold as Fold
>>> import qualified Streamly.Internal.Data.Stream as Stream (parsePos)
>>> decimal = Parser.takeWhile1 Char.isDigit Fold.toList
>>> Stream.parsePos decimal $ Stream.fromList "1234 is the number"
Right "1234"
>>> Stream.parsePos decimal $ Stream.fromList "this is the number"
Left (ParseErrorPos 1 "takeWhile1: predicate failed on first element")
```

On failure we can return a default value:

```haskell docspec
>>> import Control.Applicative ((<|>))
>>> Stream.parse (decimal <|> pure "0") $ Stream.fromList "this is the number"
Right "0"
```

See "Streamly.Data.Parser" module.

## Arrays

<!-- TODO Add pinning and unpinning examples
See the
[streamly-bytestring](https://github.com/psibi/streamly-bytestring)
repository.
-->

While streams are meant for sequential processing of in-flight data, arrays
are meant for storing data in memory with serial or random access. Processing
of the data stored by arrays is done using streams.

### Immutable Arrays

The `Array` type is used to represent immutable arrays which cannot be
modified in-place.

```haskell docspec
>>> import qualified Streamly.Data.Array as Array
>>> arr = Array.fromList "hello"
>>> Array.length arr
5
>>> Array.getIndex 1 arr
Just 'e'
```

Arrays provide streaming interfaces. Like all other data in streamly, arrays
are transformed using stream transformations.

```haskell docspec
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

```haskell docspec
>>> import qualified Streamly.Data.MutArray as MutArray
>>> arr <- Stream.fold (MutArray.createOf 12) $ Stream.fromList "hello"
>>> arr1 <- MutArray.snoc arr ' '
>>> arr2 <- Stream.fold (MutArray.append2 arr1) $ Stream.fromList "world "
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

```haskell docspec
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

```haskell docspec
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

```haskell docspec
>>> import qualified Streamly.Data.Array as Array
>>> arr = Array.serialize' (1234 :: Int)
>>> fst (Array.deserialize arr) :: Int
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

```haskell docspec
>>> import Control.Concurrent (threadDelay)
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

```haskell docspec
>>> import qualified Streamly.Data.Fold.Prelude as Fold
>>> p x = delay 1 >> print x
>>> f1 = Fold.parBuffered id (Fold.drainMapM p)
>>> f2 = Fold.parBuffered id (Fold.lmap (\x -> x * x) (Fold.drainMapM p))
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

```haskell docspec
>>> input = Stream.parBuffered (Stream.constRate 2) $ Stream.enumerateFrom 1
>>> intervals = Stream.intervalsOf 1 Fold.toList input
>>> Stream.fold Fold.toList $ Stream.take 2 intervals
[[1,2],[3,4]]
```

See `Streamly.Data.Fold.Prelude` module.

## Console IO

The `Streamly.Console.Stdio` module provides facilities to read a stream
from stdin and to write a stream to stdout and stderr.

Implementation of a console echo program:

```haskell unshared
import Data.Function ((&))
import qualified Streamly.Console.Stdio as Stdio

main =
  Stdio.readChunks    -- Stream IO (Array Word8)
    & Stdio.putChunks -- IO ()
```

An example to read two numbers from separate lines on stdin and sum them:

```haskell unshared
import Data.Function ((&))
import qualified Streamly.Internal.Console.Stdio as Stdio
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold

main =
    Stdio.readChars                              -- Stream IO Char
      & Stream.splitSepBy_ (== '\n') Fold.toList -- Stream IO String
      & fmap read                                -- Stream IO Int
      & Stream.take 2                            -- Stream IO Int
      & Stream.fold Fold.sum                     -- IO Int
      >>= print                                  -- IO ()
```

## File IO

Implementation of the Unix `cp` utility to copy `input.txt` to `output.txt`:

```haskell unshared
{-# LANGUAGE QuasiQuotes #-}

import Data.Function ((&))
import Streamly.FileSystem.Path (path)
import qualified Streamly.FileSystem.FileIO as File
import qualified Streamly.FileSystem.Path as Path
import qualified Streamly.Data.Stream as Stream

main =
  File.readChunks [path|input.txt|]                     -- Stream IO (Array Word8)
    & Stream.fold (File.writeChunks [path|output.txt|]) -- IO ()

```

Implementation of the Unix `cat` utility to read all files from
the current directory on standard output.

```haskell unshared
{-# LANGUAGE QuasiQuotes #-}

import Data.Function ((&))
import Streamly.FileSystem.Path (path)
import qualified Streamly.FileSystem.DirIO as Dir
import qualified Streamly.FileSystem.Path as Path
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Console.Stdio as Console
import qualified Streamly.FileSystem.FileIO as File

main =
  Dir.read id [path|.|]                -- Stream IO Path
    & Stream.concatMap File.readChunks -- Stream IO (Array Word8)
    & Console.putChunks                -- IO ()
```

## Network IO

Streaming network operations can be found in `Streamly.Network.*` modules. Here
is a streaming implementation of a concurrent network server which echo's back
whatever it receives.

```haskell docspec
>>> import Control.Exception (finally)
>>> import Network.Socket (Socket)
>>> import qualified Network.Socket as Net
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

```haskell docspec
>>> :set -XQuasiQuotes
>>> import Streamly.Unicode.String (str)
>>> [str|this is a string|]
"this is a string"
```

String interpolation:

```haskell docspec
>>> x = "interpolated"
>>> [str|this is an #{x} string|]
"this is an interpolated string"
```

Splitting a string into lines, each line is collected in a list fold and
all the lines are collected in a list fold.:

```haskell docspec
>>> import qualified Streamly.Internal.Unicode.Stream as Unicode
>>> Stream.fold Fold.toList $ Unicode.lines Fold.toList (Stream.fromList "lines\nthis\nstring\n\n\n")
["lines","this","string","",""]
```

You could supply any fold to consume the lines in a different way, for example
use `Fold.length` to print the lengths of the lines:

```haskell docspec
>>> Stream.fold Fold.toList $ Unicode.lines Fold.length (Stream.fromList "lines\nthis\nstring\n\n\n")
[5,4,6,0,0]
```

Decoding the contents of a file into a stream of Unicode chars:

```haskell docspec
>>> import Streamly.FileSystem.FileIO as File
>>> import Streamly.FileSystem.Path as Path
>>> :{
countChars inpFilePath = do
   inp <- Path.fromString inpFilePath
   File.readChunks inp & Unicode.decodeUtf8Chunks & Stream.fold Fold.length
:}
```

Streamly.Unicode.Parser provides Unicode char and sequence parsers:

```haskell docspec
>>> import qualified Streamly.Unicode.Parser as Unicode
>>> Stream.parse Unicode.double $ Stream.fromList "3.14"
Right 3.14
```
