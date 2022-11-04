# [Streamly][]: Idiomatic Haskell with the Performance of C

[![Gitter chat](https://badges.gitter.im/composewell/gitter.svg)](https://gitter.im/composewell/streamly)
[![Hackage](https://img.shields.io/hackage/v/streamly.svg?style=flat)](https://hackage.haskell.org/package/streamly)

Streamly is a Haskell library that provides the building blocks to build
safe, scalable, modular and high performance software.  Streamly offers:

* The type safety of Haskell.
* The performance of C programs.
* Powerful abstractions for structuring your code.
* Idiomatic functional programming.
* Declarative concurrency for the seamless use of multiprocessing hardware.

## About This Document

This guide introduces programming with [Streamly][] using a few practical
examples:

*  We will start with a simple program that [counts the number of words
   in a text](#modular-word-counting). We will then transform this program
   into a [concurrent](#concurrent-word-counting) program that can efficiently
   use multiprocessing hardware.
*  Next, we will create a [concurrent network
   server](#a-concurrent-network-server). We then show
   how to write a network server that [merges multiple
   streams](#merging-incoming-streams) concurrently.
*  Our third example shows how to list a directory tree concurrently,
   by reading [multiple directories in
   parallel](#listing-directories-recursivelyconcurrently).
*  Finally, we will look at how to [rate limit](#rate-limiting) stream
   processing.

The guide then looks at how Streamly achieves its
[performance](#performance).  It [concludes](#notes) with a brief
discussion about Streamly's design philosophy, and with suggestions for
further reading.

## Getting Started

### Installing Streamly

If you wish to follow along with this guide, you will need to have
[Streamly][] installed.

Please see the [Getting Started With The Streamly Package](/docs/User/Tutorials/getting-started.md)
guide for instructions on how to install [Streamly][].

If you wish to run benchmarks, please be sure to build your
application using the instructions in the [Build Guide](/docs/User/HowTo/Compiling.md).

### An overview of the types used in these examples

As an expository device, we have indicated the types at the intermediate
stages of stream computations as comments in the examples below.
The meaning of these types are:

* A `SerialT IO a` is a serial stream of values of type `a` in the IO Monad.
* An `AsyncT IO a` is a concurrent (asynchronous) stream of values of type
  `a` in the IO Monad.
* An `Unfold IO a b` is a representation of a function that converts a seed
  value of type `a` into a stream of values of type `b` in the IO Monad.
* A `Fold IO a b` is a representation of a function that converts a stream of
  type `a` to a final accumulator of type `b` in the IO Monad.

### A Note on Module Naming

Some of the examples below use modules from the `Internal` Streamly package
hierarchy.  These are not really internal to the library.  We classify
`Streamly` modules into two categories:

* _Released modules and APIs_: These modules and APIs are
  stable. Significant changes to these modules and APIs will cause
  Streamly's version number to change according to the package versioning
  policy.
* _Pre-release modules and APIs_: These modules and APIs have not been
  formally released yet.  They may change in the near future, and such
  changes will not necessarily be reflected in Streamly's package
  version number.  As yet unreleased modules and APIs reside in the
  `Internal` namespace.

Please use a minor release upper bound to adhere to the Haskell PVP when
using the pre-release (internal) modules.

## The Examples

### Modular Word Counting

A `Fold` in Streamly is a composable stream consumer.  For our first
example, we will use `Fold`s to count the number of bytes, words and lines
present in a file.  We will then compose individual `Fold`s together to
count words, bytes and lines at the same time.

Please see the file [WordCountModular.hs][] for the complete example
program, including the imports that we have omitted here.

#### Count Bytes (wc -c)

We start with a code fragment that counts the number of bytes in a file:

```haskell
import Data.Function ((&))

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.File as File

wcb :: String -> IO Int
wcb file =
    File.read file           -- Stream IO Word8
  & Stream.fold Fold.length  -- IO Int
```

### Count Lines (wc -l)

The next code fragment shows how to count the number of lines in a file:

```haskell
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Fold (Fold)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.File as File

-- ASCII character 10 is a newline.
countl :: Int -> Word8 -> Int
countl n ch = if ch == 10 then n + 1 else n

-- The fold accepts a stream of `Word8` and returns a line count (`Int`).
nlines :: Monad m => Fold m Word8 Int
nlines = Fold.foldl' countl 0

wcl :: String -> IO Int
wcl file =
    File.read file     -- Stream IO Word8
  & Stream.fold nlines -- IO Int
```

### Count Words (wc -w)

Our final code fragment counts the number of whitespace-separated words
in a stream:

```haskell
import Data.Char (isSpace, chr)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Fold (Fold)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.File as File

countw :: (Int, Bool) -> Word8 -> (Int, Bool)
countw (n, wasSpace) ch =
    if isSpace $ chr $ fromIntegral ch
    then (n, True)
    else (if wasSpace then n + 1 else n, False)

-- The fold accepts a stream of `Word8` and returns a word count (`Int`).
nwords :: Monad m => Fold m Word8 Int
nwords = fst <$> Fold.foldl' countw (0, True)

wcw :: String -> IO Int
wcw file =
    File.read file      -- Stream IO Word8
  & Stream.fold nwords  -- IO Int
```

### Counting Bytes, Words and Lines Together

By using the `Tee` combinator we can compose the three folds that count
bytes, lines and words individually into a single fold that counts all
three at once.  The applicative instance of `Tee` distributes its input
to all the supplied folds (`Fold.length`, `nlines`, and `nwords`) and
then combines the outputs from the folds using the supplied combiner
function (`(,,)`).

```haskell

import Data.Char (isSpace, chr)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Fold.Tee (Tee(..))

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Fold.Tee as Tee
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.File as File

countw :: (Int, Bool) -> Word8 -> (Int, Bool)
countw (n, wasSpace) ch =
    if isSpace $ chr $ fromIntegral ch
    then (n, True)
    else (if wasSpace then n + 1 else n, False)

-- The fold accepts a stream of `Word8` and returns a word count (`Int`).
nwords :: Monad m => Fold m Word8 Int
nwords = fst <$> Fold.foldl' countw (0, True)

-- ASCII character 10 is a newline.
countl :: Int -> Word8 -> Int
countl n ch = if ch == 10 then n + 1 else n

-- The fold accepts a stream of `Word8` and returns a line count (`Int`).
nlines :: Monad m => Fold m Word8 Int
nlines = Fold.foldl' countl 0

-- The fold accepts a stream of `Word8` and returns the three counts.
countAll :: Fold IO Word8 (Int, Int, Int)
countAll = Tee.toFold $ (,,) <$> Tee Fold.length <*> Tee nlines <*> Tee nwords

wc :: String -> IO (Int, Int, Int)
wc file =
    File.read file       -- Stream IO Word8
  & Stream.fold countAll -- IO (Int, Int, Int)
```

This example demonstrates the excellent modularity offered by
[Streamly][]'s simple and concise API.  Experienced Haskellers will
notice that we have not used bytestrings&mdash;we instead used a stream of
`Word8` values, simplifying our program.

### The Performance of Word Counting

We compare two equivalent implementations: one using [Streamly][],
and the other using C.

The performance of the [Streamly word counting
implementation][WordCount.hs] is:

-- XXX This needs to be changed
```
$ time WordCount-hs gutenberg-500MB.txt
11242220 97050938 574714449 gutenberg-500MB.txt

real    0m1.825s
user    0m1.697s
sys     0m0.128s
```

The performance of an equivalent [wc implementation in C][WordCount.c] is:

-- XXX This needs to be changed
```
$ time WordCount-c gutenberg-500MB.txt
11242220 97050938 574714449 gutenberg-500MB.txt

real    0m2.100s
user    0m1.935s
sys     0m0.165s
```

### Concurrent Word Counting

In our next example we show how the task of counting words, lines,
and bytes could be done in parallel on multiprocessor hardware.

To count words in parallel we first divide the stream into chunks
(arrays), do the counting within each chunk, and then add all the
counts across chunks.  We use the same code as above except that we use
arrays for our input data.

Please see the file [WordCountParallel.hs][] for the complete working
code for this example, including the imports that we have omitted below.

The `countArray` function counts the line, word, char counts in one chunk:

```haskell
import Data.Char (isSpace, chr)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Array.Unboxed (Array)

import qualified Streamly.Data.Array.Unboxed as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Unicode.Stream as Stream

-- Counts lines words chars lastCharWasSpace
data Counts = Counts !Int !Int !Int !Bool deriving Show

{-# INLINE count #-}
count :: Counts -> Char -> Counts
count (Counts l w c wasSpace) ch =
    let l1 = if ch == '\n' then l + 1 else l
        (w1, wasSpace1) =
            if isSpace ch
            then (w, True)
            else (if wasSpace then w + 1 else w, False)
    in Counts l1 w1 (c + 1) wasSpace1

countArray :: Array Word8 -> IO Counts
countArray arr =
      Stream.unfold Array.reader arr                      -- Stream IO Word8
    & Stream.decodeLatin1                                 -- Stream IO Char
    & Stream.fold (Fold.foldl' count (Counts 0 0 0 True)) -- IO Counts
```

-- XXX We might as well define `count` and `Counts` here instead of refrencing
it. It's not a lot of code.

Here the function `count` and the `Counts` data type are defined in the
`WordCount` helper module defined in [WordCount.hs][].

When combining the counts in two contiguous chunks, we need to check
whether the first element of the next chunk is a whitespace character
in order to determine if the same word continues in the next chunk or
whether the chunk starts with a new word. The `partialCounts` function
adds a `Bool` flag to `Counts` returned by `countArray` to indicate
whether the first character in the chunk is a space.

-- XXX I've added all the code required to compile it. We can remove the
unecessary parts later.
```haskell
import Data.Char (isSpace, chr)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Array.Unboxed (Array)

import qualified Streamly.Data.Array.Unboxed as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Unicode.Stream as Stream

-- Counts lines words chars lastCharWasSpace
data Counts = Counts !Int !Int !Int !Bool deriving Show

{-# INLINE count #-}
count :: Counts -> Char -> Counts
count (Counts l w c wasSpace) ch =
    let l1 = if ch == '\n' then l + 1 else l
        (w1, wasSpace1) =
            if isSpace ch
            then (w, True)
            else (if wasSpace then w + 1 else w, False)
    in Counts l1 w1 (c + 1) wasSpace1

countArray :: Array Word8 -> IO Counts
countArray arr =
      Stream.unfold Array.reader arr                      -- Stream IO Word8
    & Stream.decodeLatin1                                 -- Stream IO Char
    & Stream.fold (Fold.foldl' count (Counts 0 0 0 True)) -- IO Counts

partialCounts :: Array Word8 -> IO (Bool, Counts)
partialCounts arr = do
    let r = Array.getIndex 0 arr
    case r of
        Just x -> do
            counts <- countArray arr
            return (isSpace (chr (fromIntegral x)), counts)
        Nothing -> return (False, Counts 0 0 0 True)
```

`addCounts` then adds the counts from two consecutive chunks:

```haskell

import Data.Char (isSpace, chr)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Array.Unboxed (Array)

import qualified Streamly.Data.Array.Unboxed as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Unicode.Stream as Stream

-- Counts lines words chars lastCharWasSpace
data Counts = Counts !Int !Int !Int !Bool deriving Show

addCounts :: (Bool, Counts) -> (Bool, Counts) -> (Bool, Counts)
addCounts (sp1, Counts l1 w1 c1 ws1) (sp2, Counts l2 w2 c2 ws2) =
    let wcount =
            if not ws1 && not sp2 -- No space between two chunks.
            then w1 + w2 - 1
            else w1 + w2
     in (sp1, Counts (l1 + l2) wcount (c1 + c2) ws2)
```

To count in parallel we now only need to divide the stream into arrays,
apply our counting function to each array, and then combine the counts
from each chunk.

```haskell

import Data.Char (isSpace, chr)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Array.Unboxed (Array)

import qualified Streamly.Data.Array.Unboxed as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Concurrent as Concur
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Unicode.Stream as Stream

-- Counts lines words chars lastCharWasSpace
data Counts = Counts !Int !Int !Int !Bool deriving Show

{-# INLINE count #-}
count :: Counts -> Char -> Counts
count (Counts l w c wasSpace) ch =
    let l1 = if ch == '\n' then l + 1 else l
        (w1, wasSpace1) =
            if isSpace ch
            then (w, True)
            else (if wasSpace then w + 1 else w, False)
    in Counts l1 w1 (c + 1) wasSpace1

countArray :: Array Word8 -> IO Counts
countArray arr =
      Stream.unfold Array.reader arr                      -- Stream IO Word8
    & Stream.decodeLatin1                                 -- Stream IO Char
    & Stream.fold (Fold.foldl' count (Counts 0 0 0 True)) -- IO Counts

partialCounts :: Array Word8 -> IO (Bool, Counts)
partialCounts arr = do
    let r = Array.getIndex 0 arr
    case r of
        Just x -> do
            counts <- countArray arr
            return (isSpace (chr (fromIntegral x)), counts)
        Nothing -> return (False, Counts 0 0 0 True)

addCounts :: (Bool, Counts) -> (Bool, Counts) -> (Bool, Counts)
addCounts (sp1, Counts l1 w1 c1 ws1) (sp2, Counts l2 w2 c2 ws2) =
    let wcount =
            if not ws1 && not sp2 -- No space between two chunks.
            then w1 + w2 - 1
            else w1 + w2
     in (sp1, Counts (l1 + l2) wcount (c1 + c2) ws2)

numCapabilities :: Int
numCapabilities = 4

wc :: String -> IO (Bool, Counts)
wc file = do
      Stream.unfold File.chunkReader file
    & Concur.mapMWith
          ( Concur.ordered True
          . Concur.maxThreads numCapabilities)
          partialCounts
    & Stream.fold (Fold.foldl' addCounts (False, Counts 0 0 0 True))
```

Please note that the only difference between a concurrent and a
non-concurrent program lies in the use of the `Stream.fromAhead`
combinator.  If we remove the call to `Stream.fromAhead`, we would
still have a perfectly valid and performant serial program. Notice
how succinctly and idiomatically we have expressed the concurrent word
counting problem.

A benchmark with 2 CPUs:

-- XXX Needs to be changed
```
$ time WordCount-hs-parallel gutenberg-500MB.txt
11242220 97050938 574714449 gutenberg-500MB.txt

real    0m1.284s
user    0m1.952s
sys     0m0.140s
```

These example programs have assumed ASCII encoded input data.  For UTF-8
streams, we have a [concurrent wc implementation][WordCountParallelUTF8.hs]
with UTF-8 decoding.  This concurrent implementation performs as well
as the standard `wc` program in serial benchmarks. In concurrent mode
[Streamly][]'s implementation can utilise multiple processing cores if
these are present, and can thereby run much faster than the standard
binary.

Streamly provides concurrency facilities similar
to [OpenMP](https://en.wikipedia.org/wiki/OpenMP) and
[Cilk](https://en.wikipedia.org/wiki/Cilk) but with a more declarative
style of expression.  With Streamly you can write concurrent programs
with ease, with support for different types of concurrent scheduling.

### A Concurrent Network Server

We now move to a slightly more complicated example: we simulate a
dictionary lookup server which can serve word meanings to multiple
clients concurrently.  This example demonstrates the use of the concurrent
`mapM` combinator.

Please see the file [WordServer.hs][] for the complete code for this
example, including the imports that we have omitted below.

```haskell
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.Char (isSpace, chr)
import Data.Function ((&))
import Data.Word (Word8)
import Network.Socket (Socket, close)
import Streamly.Data.Array.Unboxed (Array)

import qualified Streamly.Data.Array.Unboxed as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Concurrent as Concur
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Unicode.Stream as Unicode

-- Simulate network/db query by adding a delay.
fetch :: String -> IO (String, String)
fetch w = threadDelay 1000000 >> return (w,w)

-- Read lines of whitespace separated list of words from a socket, fetch the
-- meanings of each word concurrently and return the meanings separated by
-- newlines, in same order as the words were received. Repeat until the
-- connection is closed.
lookupWords :: Socket -> IO ()
lookupWords sk =
      Stream.unfold Socket.reader sk
    & Unicode.decodeLatin1
    & Stream.parseMany (Parser.wordBy isSpace Fold.toList)
    & Concur.mapMWith (Concur.ordered True) fetch
    & fmap show
    & Stream.intersperse "\n"
    & Unicode.encodeStrings Unicode.encodeLatin1
    & Stream.fold (Socket.writeChunks sk)

serve :: Socket -> IO ()
serve sk = finally (lookupWords sk) (close sk)

-- | Run a server on port 8091. Accept and handle connections concurrently. The
-- connection handler is "serve" (i.e. lookupWords).  You can use "telnet" or
-- "nc" as a client to try it out.
main :: IO ()
main =
      Stream.unfold TCP.acceptorOnPort 8091
    & Concur.mapM serve
    & Stream.fold Fold.drain
```

### Merging Incoming Streams

In the next example, we show how to merge logs coming from multiple
nodes in your network.  These logs are merged at line boundaries and
the merged logs are written to a file or to a network destination.
This example uses the `concatMapWith` combinator to merge multiple
streams concurrently.

Please see the file [MergeServer.hs][] for the complete working code,
including the imports that we have omitted below.

```haskell
import Data.Function ((&))
import Network.Socket (Socket, close)
import Streamly.Data.Array.Unboxed (Array)
import Streamly.Data.Stream (Stream)
import System.IO (Handle, withFile, IOMode(..))

import qualified Streamly.Data.Array.Unboxed as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Concurrent as Concur
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Unicode.Stream as Unicode

-- | Read a line stream from a socket. Note, lines are buffered, we could add
-- a limit to the buffering for safety.
readLines :: Socket -> Stream IO (Array Char)
readLines sk =
    Stream.unfold Socket.reader sk               -- Stream IO Word8
  & Unicode.decodeLatin1                         -- Stream IO Char
  & Stream.foldMany (Fold.takeEndBy (== '\n') Array.write) -- Stream IO String

recv :: Socket -> Stream IO (Array Char)
recv sk = Stream.finallyIO (close sk) (readLines sk)

-- | Starts a server at port 8091 listening for lines with space separated
-- words. Multiple clients can connect to the server and send streams of lines.
-- The server handles all the connections concurrently, merges the incoming
-- streams at line boundaries and writes the merged stream to a file.
server :: Handle -> IO ()
server file =
      Stream.unfold TCP.acceptorOnPort 8090         -- Stream IO Socket
    & Concur.concatMapWith (Concur.eager True) recv -- Stream IO (Array Char)
    & Stream.unfoldMany Array.reader                -- Stream IO Char
    & Unicode.encodeLatin1                          -- Stream IO Word8
    & Stream.fold (Handle.write file)               -- IO ()

main :: IO ()
main = withFile "output.txt" AppendMode server
```

### Listing Directories Recursively/Concurrently

Our next example lists a directory tree recursively, reading
multiple directories concurrently.

This example uses the tree traversing combinator `iterateMapLeftsWith`.
This combinator maps a stream generator on the `Left` values in its
input stream (directory names in this case), feeding the resulting `Left`
values back to the input, while it lets the `Right` values (file names
in this case) pass through to the output. The `Stream.ahead` stream
joining combinator then makes it iterate on the directories concurrently.

Please see the file [ListDir.hs][] for the complete working code,
including the imports that we have omitted below.

-- XXX This needs to be checked. I'm explicitly using the channel here. `ahead2`
would be inefficient.
```haskell
import Data.Bifunctor (bimap)
import Data.Function ((&))
import Streamly.Data.Stream (Stream)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Concurrent as Concur
import qualified Streamly.Internal.Data.Stream.Concurrent.Channel as Concur
import qualified Streamly.Internal.Data.Stream as Stream
       (iterateMapLeftsWith)
import qualified Streamly.Internal.FileSystem.Dir as Dir (readEither)

-- Lists a dir as a stream of (Either Dir File)
listDir :: String -> Stream IO (Either String String)
listDir dir =
      Dir.readEither dir       -- Stream IO (Either String String)
    & fmap (bimap mkAbs mkAbs) -- Stream IO (Either String String)

    where mkAbs x = dir ++ "/" ++ x

-- | List the current directory recursively using concurrent processing
main :: IO ()
main = do
    chan <- Concur.newChannel (Concur.ordered True)
    let combineOrdered s1 s2 = do
            Stream.fromEffect $ Concur.toChannel chan s1
            Stream.fromEffect $ Concur.toChannel chan s2
            Concur.fromChannel chan
    hSetBuffering stdout LineBuffering
    let start = Stream.fromPure (Left ".")
    Stream.iterateMapLeftsWith combineOrdered listDir start
        & Stream.fold (Fold.drainMapM print)
```

### Rate Limiting

For bounded concurrent streams, a stream yield rate can be specified
easily.  For example, to print "tick" once every second you can simply
write:

-- XXX Remove the comment in the code accordingly.
```haskell
import Data.Function ((&))

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Concurrent as Concur
import qualified Streamly.Internal.Data.Stream as Stream (timestamped)

main :: IO ()
main =
      Concur.sequenceWith (Concur.avgRate 1) (Stream.repeat (pure "tick"))
    & Stream.timestamped                             -- Note that we are not
                                                     -- concurrently
                                                     -- timestamping here. The
                                                     -- zip is serial as it
                                                     -- should be. `AsyncT` is
                                                     -- confusing, I'm liking
                                                     -- explicit concurrency a
                                                     -- lot.
    & Stream.fold (Fold.drainMapM print)
```

Please see the file [Rate.hs][] for the complete working code.

The concurrency of the stream is automatically controlled to match the
specified rate. [Streamly][]'s rate control works precisely even at
throughputs as high as millions of yields per second.

For more sophisticated rate control needs please see the Streamly [reference
documentation][Streamly].

### Reactive Programming

Streamly supports reactive (time domain) programming because of its
support for declarative concurrency. Please see the `Streamly.Prelude`
module for time-specific combinators like `intervalsOf`, and
folds like `takeInterval` in `Streamly.Internal.Data.Fold`.
Please also see the pre-release sampling combinators in the
`Streamly.Internal.Data.Stream.IsStream.Top` module for `throttle` and
`debounce` like operations.

The examples [AcidRain.hs][] and [CirclingSquare.hs][] demonstrate
reactive programming using [Streamly][].

### More Examples

If you would like to view more examples, please visit the [Streamly
Examples][streamly-examples] web page.

### Further Reading

* [Streaming Benchmarks][streaming-benchmarks]
* [Concurrency Benchmarks][concurrency-benchmarks]
* Functional Conf 2019 [Video](https://www.youtube.com/watch?v=uzsqgdMMgtk) | [Slides](https://www.slideshare.net/HarendraKumar10/streamly-concurrent-data-flow-programming)
* [Other Guides](/)
* [Streamly Homepage][Streamly]

## Performance

As you have seen in the word count example above, [Streamly][] offers
highly modular abstractions for building programs while also offering
the performance close to an equivalent (imperative) C program.

Streamly offers excellent performance even for byte-at-a-time stream
operations using efficient abstractions like `Unfold`s and terminating
`Fold`s.  Byte-at-a-time stream operations can simplify programming
because the developer does not have to deal explicitly with chunking
and re-combining data.

Streamly exploits GHC's stream fusion optimizations (`case-of-case` and
`spec-constr`) aggressively to achieve C-like speed, while also offering
highly modular abstractions to developers.

[Streamly][] will usually perform very well without any
compiler plugins.  However, we have fixed some deficiencies
that we had noticed in GHC's optimizer using a [compiler
plugin](https://github.com/composewell/fusion-plugin).  We hope to fold
these optimizations into GHC in the future; until then we recommend that
you use this plugin for applications that are performance sensitive.

### Benchmarks

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

## Notes

Streamly comes equipped with a very powerful set of abstractions to
accomplish many kinds of programming tasks: it provides support for
programming with streams and arrays, for reading and writing from the
file system and from the network, for time domain programming (reactive
programming), and for reacting to file system events using `fsnotify`.

Please view [Streamly's documentation][Streamly] for more information
about Streamly's features.

### Concurrency

Streamly uses lock-free synchronization for achieving concurrent
operation with low overheads.  The number of tasks performed concurrently
are determined automatically based on the rate at which a consumer
is consuming the results. In other words, you do not need to manage
thread pools or decide how many threads to use for a particular task.
For CPU-bound tasks Streamly will try to keep the number of threads
close to the number of CPUs available; for IO-bound tasks it will utilize
more threads.

The parallelism available during program execution can be utilized with
very little overhead even where the task size is very
small, because Streamly will automatically switch between
serial or batched execution of tasks on the same CPU depending
on whichever is more efficient.  Please see our [concurrency
benchmarks][concurrency-benchmarks] for more detailed performance
measurements, and for a comparison with the `async` package.

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
[streamly-examples]: https://github.com/composewell/streamly-examples
[streaming-benchmarks]: https://github.com/composewell/streaming-benchmarks
[concurrency-benchmarks]: https://github.com/composewell/concurrency-benchmarks

<!--
Keep all the unstable links here so that they can be updated to stable
links (for online docs) before we release.
-->

<!-- examples -->
[WordCountModular.hs]: https://github.com/composewell/streamly-examples/blob/master/examples/WordCountModular.hs
[WordCount.hs]: https://github.com/composewell/streamly-examples/blob/master/examples/WordCount.hs
[WordCount.c]: https://github.com/composewell/streamly-examples/blob/master/examples/WordCount.c
[WordCountParallel.hs]: https://github.com/composewell/streamly-examples/blob/master/examples/WordCountParallel.hs
[WordCountParallelUTF8.hs]: https://github.com/composewell/streamly-examples/blob/master/examples/WordCountParallelUTF8.hs
[WordServer.hs]: https://github.com/composewell/streamly-examples/blob/master/examples/WordServer.hs
[MergeServer.hs]: https://github.com/composewell/streamly-examples/blob/master/examples/MergeServer.hs
[ListDir.hs]: https://github.com/composewell/streamly-examples/blob/master/examples/ListDir.hs
[Rate.hs]: https://github.com/composewell/streamly-examples/blob/master/examples/Rate.hs
[AcidRain.hs]: https://github.com/composewell/streamly-examples/tree/master/examples/AcidRain.hs
[CirclingSquare.hs]: https://github.com/composewell/streamly-examples/tree/master/examples/CirclingSquare.hs

<!-- local files -->
[LICENSE]: /LICENSE
[CONTRIBUTING.md]: /CONTRIBUTING.md
[docs]: docs/
