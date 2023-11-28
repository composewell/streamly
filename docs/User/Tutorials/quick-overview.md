<!--
(c) 2017, Composewell Technologies and Contributors
SPDX-License-Identifer: BSD-3-Clause
-->

# [Streamly][] Quick Tutorial
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

It [concludes](#further-reading) with suggestions for further reading.

## Getting Started

### Installing Streamly

If you wish to follow along and run examples in this guide, please see
the [Before You Begin](/docs/User/Tutorials/before-you-begin.md) guide
for instructions on how to use the `streamly` package interactively or
in a project.

### An overview of the types used in these examples

As an expository device, we have indicated the types at the intermediate
stages of stream computations as comments in the examples below.
The meaning of these types are:

* A `Stream IO a` is a representation of a sequence of values of type
  `a` in the IO Monad.
* A `Fold IO a b` is a representation of a function that converts a stream of
  type `a` to a final accumulator of type `b` in the IO Monad.

## The Examples

The code snippets below should work in GHCi if all of those are typed
in sequence.  For brevity, imports that are already used in earlier
snippets are omitted from the latter ones.

### Modular Word Counting

A `Fold` in Streamly is a composable stream consumer.  For our first
example, we will use `Fold`s to count the number of bytes, words and lines
present in a file.  We will then compose individual `Fold`s together to
count words, bytes and lines at the same time.

Please see the file [WordCountModular.hs][] for the complete example
program.

#### Count Bytes (wc -c)

We start with a code fragment that counts the number of bytes in a file:

```haskell ghci
import Data.Function ((&))

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.File as File

wcb :: String -> IO Int
wcb file =
    File.read file           -- Stream IO Word8
  & Stream.fold Fold.length  -- IO Int
```

### Count Lines (wc -l)

The next code fragment shows how to count the number of lines in a file:

```haskell ghci
import Data.Word (Word8)
import Streamly.Data.Fold (Fold)

-- ASCII character 10 is a newline.
countl :: Int -> Word8 -> Int
countl n ch = if ch == 10 then n + 1 else n

-- The `nlines` fold accepts a stream of `Word8` and returns a line count (`Int`).
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

```haskell ghci
import Data.Char (chr, isSpace)

countw :: (Int, Bool) -> Word8 -> (Int, Bool)
countw (n, wasSpace) ch =
    if isSpace $ chr $ fromIntegral ch
    then (n, True)
    else (if wasSpace then n + 1 else n, False)

-- The `nwords` fold accepts a stream of `Word8` and returns a word count (`Int`).
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

```haskell ghci
import Streamly.Data.Fold (Tee(..))

-- The fold accepts a stream of `Word8` and returns the three counts.
countAll :: Fold IO Word8 (Int, Int, Int)
countAll = unTee $ (,,) <$> Tee Fold.length <*> Tee nlines <*> Tee nwords

wc :: String -> IO (Int, Int, Int)
wc file =
    File.read file       -- Stream IO Word8
  & Stream.fold countAll -- IO (Int, Int, Int)
```

This example demonstrates the excellent modularity offered by
[Streamly][]'s simple and concise API.

### The Performance of Word Counting

We compare two equivalent implementations: one using [Streamly][],
and the other using C.

The performance of the [Streamly word counting
implementation][WordCount.hs] (using ghc-9.4.4 and fusion-plugin) is:

```
$ time WordCount-hs gutenberg-500MB.txt
11242220 97050938 574714449 gutenberg-500MB.txt

real    0m2.033s
user    0m1.821s
sys     0m0.209s
```

The performance of an equivalent [wc implementation in C][WordCount.c] is:

```
$ time WordCount-c gutenberg-500MB.txt
11242220 97050938 574714449 gutenberg-500MB.txt

real    0m2.113s
user    0m1.928s
sys     0m0.185s
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

First we create a new data type `Counts` that holds all the context.

```haskell ghci
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
```

The `countArray` function counts the line, word, char counts in one chunk:

```haskell ghci
import Streamly.Data.Array (Array)

import qualified Streamly.Data.Array as Array
import qualified Streamly.Unicode.Stream as Unicode

countArray :: Array Word8 -> IO Counts
countArray arr =
      Array.read arr                                      -- Stream IO Word8
    & Unicode.decodeLatin1                                -- Stream IO Char
    & Stream.fold (Fold.foldl' count (Counts 0 0 0 True)) -- IO Counts
```

Here the function `count` and the `Counts` data type are defined in the
`WordCount` helper module defined in [WordCount.hs][].

When combining the counts in two contiguous chunks, we need to check
whether the first element of the next chunk is a whitespace character
in order to determine if the same word continues in the next chunk or
whether the chunk starts with a new word. The `partialCounts` function
adds a `Bool` flag to `Counts` returned by `countArray` to indicate
whether the first character in the chunk is a space.

```haskell ghci
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

```haskell ghci
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

```haskell ghci
{-# LANGUAGE FlexibleContexts #-}

import GHC.Conc (numCapabilities)
import qualified Streamly.Data.Stream.Prelude as Stream

wc :: String -> IO (Bool, Counts)
wc file = do
      File.readChunks file             -- Stream IO (Array Word8)
    & Stream.parMapM cfg partialCounts -- Stream IO (Bool, Counts)
    & Stream.fold add                  -- IO (Bool, Counts)

    where

    cfg = Stream.maxThreads numCapabilities . Stream.ordered True
    add = Fold.foldl' addCounts (False, Counts 0 0 0 True)
```

We can replace `parMapM` with `mapM` to get a serial version of the program.

A benchmark with 2 CPUs:

```
$ time WordCount-hs-parallel gutenberg-500MB.txt
11242220 97050938 574714449 gutenberg-500MB.txt

real    0m1.443s
user    0m2.095s
sys     0m0.202s
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
clients concurrently.

Please see the file [WordServer.hs][] for the complete code for this
example.

```haskell ghci
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Network.Socket (Socket, close)

import qualified Streamly.Data.Parser as Parser
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
      Socket.read sk                             -- Stream IO Word8
    & Unicode.decodeLatin1                       -- Stream IO Char
    & Stream.wordsBy isSpace Fold.toList         -- Stream IO String
    & Stream.parMapM cfg fetch                   -- Stream IO (String, String)
    & fmap show                                  -- Stream IO String
    & Stream.intersperse "\n"                    -- Stream IO String
    & Unicode.encodeStrings Unicode.encodeLatin1 -- Stream IO (Array Word8)
    & Stream.fold (Socket.writeChunks sk)

    where

    cfg = Stream.ordered True

serve :: Socket -> IO ()
serve sk = finally (lookupWords sk) (close sk)

-- | Run a server on port 8091. Accept and handle connections concurrently. The
-- connection handler is "serve" (i.e. lookupWords).  You can use "telnet" or
-- "nc" as a client to try it out.
main :: IO ()
main =
      TCP.accept 8091         -- Stream IO Socket
    & Stream.parMapM id serve -- Stream IO ()
    & Stream.fold Fold.drain  -- IO ()
```

### Merging Incoming Streams

In the next example, we show how to merge logs coming from multiple
nodes in your network.  These logs are merged at line boundaries and
the merged logs are written to a file or to a network destination.
This example uses the `concatMapWith` combinator to merge multiple
streams concurrently.

Please see the file [MergeServer.hs][] for the complete working code,
including the imports that we have omitted below.

```haskell ghci
{-# LANGUAGE FlexibleContexts #-}

import Streamly.Data.Stream (Stream)
import System.IO (IOMode(AppendMode), Handle, withFile)

import qualified Streamly.Network.Socket as Socket
import qualified Streamly.FileSystem.Handle as Handle

-- | Read a line stream from a socket.
-- Note: lines are buffered, and we could add a limit to the
-- buffering for safety.
readLines :: Socket -> Stream IO (Array Char)
readLines sk =
    Socket.read sk       -- Stream IO Word8
  & Unicode.decodeLatin1 -- Stream IO Char
  & Stream.foldMany line -- Stream IO (Array Char)

  where

  line = Fold.takeEndBy (== '\n') Array.write

recv :: Socket -> Stream IO (Array Char)
recv sk = Stream.finallyIO (close sk) (readLines sk)

-- | Starts a server at port 8091 listening for lines with space separated
-- words. Multiple clients can connect to the server and send streams of lines.
-- The server handles all the connections concurrently, merges the incoming
-- streams at line boundaries and writes the merged stream to a file.
server :: Handle -> IO ()
server file =
      TCP.accept 8090                              -- Stream IO Socket
    & Stream.parConcatMap (Stream.eager True) recv -- Stream IO (Array Char)
    & Stream.unfoldMany Array.reader               -- Stream IO Char
    & Unicode.encodeLatin1                         -- Stream IO Word8
    & Stream.fold (Handle.write file)              -- IO ()

main :: IO ()
main = withFile "output.txt" AppendMode server
```

### Listing Directories Recursively/Concurrently

Our next example lists a directory tree recursively, and concurrently.

This example uses the tree traversing combinator `parConcatIterate`.  This
combinator maps a stream generator function on the input stream and then
recursively on the generated stream as well and flattens the results. We map a
directory to a stream generating its children and a file to a nil stream. This
results in a concurrent recursive depth first traversal of the directory tree.

Please see [ListDir.hs][] for the complete working code.

```haskell ghci
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import qualified Streamly.Internal.FileSystem.Dir as Dir (readEitherPaths)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    let start = Stream.fromPure (Left ".")
        f = either Dir.readEitherPaths (const Stream.nil)
        ls = Stream.parConcatIterate id f start
     in Stream.fold (Fold.drainMapM print) ls
```

### Rate Limiting

For concurrent streams, a stream evaluation rate can be specified.  For
example, to print "tick" once every second you can simply write:

```haskell ghci
import qualified Streamly.Internal.Data.Stream as Stream (timestamped)

main :: IO ()
main =
      Stream.parRepeatM (Stream.avgRate 1) (pure "tick") -- Stream IO String
    & Stream.timestamped                                 -- Stream IO (AbsTime, String)
    & Stream.fold (Fold.drainMapM print)                 -- IO ()
```

Please see the file [Rate.hs][] for the complete working code.

The concurrency of the stream is automatically controlled to match the
specified rate. [Streamly][]'s rate control works precisely even at
throughputs as high as millions of yields per second.

For more sophisticated rate control needs please see the Streamly [reference
documentation][Streamly].

### Reactive Programming

Streamly supports reactive (time domain) programming because of its support for
declarative concurrency. Please see the `Streamly.Data.Stream.Prelude`
module for time-specific and sampling combinators.

The examples [AcidRain.hs][] and [CirclingSquare.hs][] demonstrate
reactive programming using [Streamly][].

### More Examples

If you would like to view more examples, please visit the [Streamly
Examples][streamly-examples] web page.

<!--
### Further Reading

* [Streaming Benchmarks][streaming-benchmarks]
* [Concurrency Benchmarks][concurrency-benchmarks]
* Functional Conf 2019 [Video](https://www.youtube.com/watch?v=uzsqgdMMgtk) | [Slides](https://www.slideshare.net/HarendraKumar10/streamly-concurrent-data-flow-programming)
* [Other Guides](/)
* [Streamly Homepage][Streamly]
-->

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
