# Streamly

[![Hackage](https://img.shields.io/hackage/v/streamly.svg?style=flat)](https://hackage.haskell.org/package/streamly)
[![Gitter chat](https://badges.gitter.im/composewell/gitter.svg)](https://gitter.im/composewell/streamly)
[![Travis](https://travis-ci.com/composewell/streamly.svg?branch=master)](https://travis-ci.com/composewell/streamly)
[![Appveyor](https://ci.appveyor.com/api/projects/status/ajxg0c79raou9ned?svg=true)](https://ci.appveyor.com/project/harendra-kumar/streamly)
[![CircleCI](https://circleci.com/gh/composewell/streamly/tree/master.svg?style=svg)](https://circleci.com/gh/composewell/streamly/tree/master)
[![Coverage Status](https://coveralls.io/repos/composewell/streamly/badge.svg?branch=master&service=github)](https://coveralls.io/github/composewell/streamly?branch=master)

## Learning Materials

* Documentation: [Quick](#streaming-concurrently) | [Tutorial](https://hackage.haskell.org/package/streamly/docs/Streamly-Tutorial.html) | [Reference (Hackage)](https://hackage.haskell.org/package/streamly) | [Reference (Latest)](https://composewell.github.io/streamly) | [Guides](docs)
* Installing: [Installing](./INSTALL.md) | [Building for optimal performance](docs/Build.md)
* [streamly-examples](https://github.com/composewell/streamly-examples)
* Benchmarks: [Streaming](https://github.com/composewell/streaming-benchmarks) | [Concurrency](https://github.com/composewell/concurrency-benchmarks)
* Talks: [Functional Conf 2019 Video](https://www.youtube.com/watch?v=uzsqgdMMgtk) | [Functional Conf 2019 Slides](https://www.slideshare.net/HarendraKumar10/streamly-concurrent-data-flow-programming)

## Idiomatic Haskell at the speed of C

Streamly is a Haskell library/framework providing basic building blocks
or combinators to build safe, scalable, modular and high performance
software systems.  The key features it provides are:

* Speed of C
* Safety of Haskell
* Idiomatic functional programming
* Powerful abstractions
* Declarative concurrency

Let's go through some practical examples to see it working. You
can find the working code of these examples in the [streamly-examples
repository](https://github.com/composewell/streamly-examples).

## Types Overview

* `SerialT IO a` is a serial stream of values of type `a` in IO Monad.
* `AsyncT IO a` is a concurrent (async) stream of values of type `a` in IO
  Monad.
* `Unfold IO a b` is a representation of a function that converts a seed
  value of type `a` to a stream of values of type `b` in IO Monad.
* `Fold IO a b` is a representation of a function that converts a stream of
  type `a` to a final accumulator of type `b` in IO Monad.

## Modular Word Counting

In this example, we will use folds to count bytes, words and lines in
a file.  A `Fold` is a composable stream consumer.  We will see how
individual folds can be composed together to perform all the three
counts at once with the same performance.

See [WordCountModular.hs](https://github.com/composewell/streamly-examples/blob/master/examples/WordCountModular.hs)
for full working code including imports that we may have omitted
here. Note, the `Internal` modules imported here are `pre-release`
modules that have been tested and are ready for use except for some
minor signature changes planned before we release them.

### Count bytes (wc -c)

Count bytes in a file.

``` haskell
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Prelude as Stream

wcb :: String -> IO Int
wcb file =
    File.toBytes file        -- SerialT IO Word8
  & Stream.fold Fold.length  -- IO Int
```

### Count lines (wc -l)

Count lines in a file.

``` haskell
-- ASCII character 10 is newline
countl :: Int -> Word8 -> Int
countl n ch = if ch == 10 then n + 1 else n

-- The fold accepts a stream of `Word8` and returns a line count (`Int`).
nlines :: Monad m => Fold m Word8 Int
nlines = Fold.foldl' countl 0

wcl :: String -> IO Int
wcl file =
    File.toBytes file  -- SerialT IO Word8
  & Stream.fold nlines -- IO Int
```

### Count words (wc -w)

Count words in a file.

``` haskell
countw :: (Int, Bool) -> Word8 -> (Int, Bool)
countw (n, wasSpace) ch =
    if isSpace $ chr $ fromIntegral ch
    then (n, True)
    else (if wasSpace then n + 1 else n, False)

-- The fold accepts a stream of `Word8` and returns a word count (`Int`)
nwords :: Monad m => Fold m Word8 Int
nwords = fst <$> Fold.foldl' countw (0, True)

wcw :: String -> IO Int
wcw file =
    File.toBytes file   -- SerialT IO Word8
  & Stream.fold nwords  -- IO Int
```

### Count bytes, words and lines

We can compose the three folds together into a single fold using `Tee`
to do all the three things at once. The applicative instance of `Tee`
distributes the input to all the folds and combines the outputs using the
supplied function.

``` haskell
import qualified Streamly.Internal.Data.Fold.Tee as Tee

-- The fold accepts a stream of `Word8` and returns the three counts
countAll :: Fold IO Word8 (Int, Int, Int)
countAll = Tee.toFold $ (,,) <$> Tee Fold.length <*> Tee nlines <*> Tee nwords

wc :: String -> IO (Int, Int, Int)
wc file =
    File.toBytes file    -- SerialT IO Word8
  & Stream.fold countAll -- IO (Int, Int, Int)
```

This example demonstrates the simple and concise API of streamly with
excellent modularity.  Experienced Haskellers would notice that we have
not used bytestrings, we simply use a stream of `Word8`, simplifying the
program.

## Performance

We compare two equivalent implementations, one using Haskell Streamly and the
other using C. The
[Haskell Streamly word counting implementation](https://github.com/composewell/streamly-examples/blob/master/examples/WordCount.hs):

```
$ time WordCount-hs gutenberg-500MB.txt
11242220 97050938 574714449 gutenberg-500MB.txt

real    0m1.825s
user    0m1.697s
sys     0m0.128s
```

[Equivalent BSD wc implementation in C](https://github.com/composewell/streamly-examples/blob/master/examples/WordCount.c):

```
$ time WordCount-c gutenberg-500MB.txt
11242220 97050938 574714449 gutenberg-500MB.txt

real    0m2.100s
user    0m1.935s
sys     0m0.165s
```

## Concurrent Word Counting

To count words in parallel we divide the stream into chunks (arrays),
count properties in each chunk and then add all the counts.  We use the
same code as above except that we use an array input instead of using a
file input.

See
[WordCountParallel.hs](https://github.com/composewell/streamly-examples/blob/master/examples/WordCountParallel.hs).
for full working code including the imports that we may have omitted below.

Get the line, word, char counts in one chunk.

``` haskell
import qualified Streamly.Data.Array.Foreign as Array

countArray :: Array Word8 -> IO Counts
countArray arr =
      Stream.unfold Array.read arr            -- SerialT IO Word8
    & Stream.decodeLatin1                     -- SerialT IO Char
    & Stream.foldl' count (Counts 0 0 0 True) -- IO Counts
```
When combining the counts in two contiguous chunks, we would also need
to know whether the first element of the next chunk was a space char or
non-space to know whether the same word is continuing to the next chunk
or if it is a new word. `partialCounts` adds a `Bool` flag to `Counts`
returned by `countArray` to indicate whether the first character in the
chunk is a space.

``` haskell
partialCounts :: Array Word8 -> IO (Bool, Counts)
partialCounts arr = do
    let r = Array.getIndex arr 0
    case r of
        Just x -> do
            counts <- countArray arr
            return (isSpace (chr (fromIntegral x)), counts)
        Nothing -> return (False, Counts 0 0 0 True)
```

Combine the counts from two consecutive chunks.
``` haskell
addCounts :: (Bool, Counts) -> (Bool, Counts) -> (Bool, Counts)
addCounts (sp1, Counts l1 w1 c1 ws1) (sp2, Counts l2 w2 c2 ws2) =
    let wcount =
            if not ws1 && not sp2 -- no space between two chunks
            then w1 + w2 - 1
            else w1 + w2
     in (sp1, Counts (l1 + l2) wcount (c1 + c2) ws2)
```

Now put it all together, we only need to divide the stream into arrays,
apply our counting function to each array and then combine all the counts.
``` haskell
wc :: String -> IO (Bool, Counts)
wc file = do
      Stream.unfold File.readChunks file -- AheadT IO (Array Word8)
    & Stream.mapM partialCounts          -- AheadT IO (Bool, Counts)
    & Stream.maxThreads numCapabilities  -- AheadT IO (Bool, Counts)
    & Stream.fromAhead                   -- SerialT IO (Bool, Counts)
    & Stream.foldl' addCounts (False, Counts 0 0 0 True) -- IO (Bool, Counts)
```

Note that `Stream.fromAhead` is the only difference in a concurrent and
non-concurrent program. If we remove that we still have a perfectly valid,
well performing serial program. Notice, how succinctly and idiomatically
we expressed the concurrent word counting problem.

Benchmarked with 2 CPUs:

```
$ time WordCount-hs-parallel gutenberg-500MB.txt
11242220 97050938 574714449 gutenberg-500MB.txt

real    0m1.284s
user    0m1.952s
sys     0m0.140s
```

If you want to get serious about word counting, here is a
[concurrent wc implementation with UTF-8 decoding](https://github.com/composewell/streamly-examples/blob/master/examples/WordCountUTF8.hs).
It performs as well as the stock wc in serial benchmarks, and of course
in concurrent mode it can use multiple cores so can be much faster.

Streamly provides concurrency facilities similar to
[OpenMP](https://en.wikipedia.org/wiki/OpenMP) and
[Cilk](https://en.wikipedia.org/wiki/Cilk) but with a more declarative
expression. You can write concurrent loops with ease, with different types of
concurrent scheduling.

## Concurrent Network Server

Slightly more complicated example. A dictionary lookup server, the server
serves word meanings to multiple clients concurrently. It uses the concurrent
`mapM` combinator.

See
[WordServer.hs](https://github.com/composewell/streamly-examples/blob/master/examples/WordServer.hs)
for full working code including the imports that we may have omitted below.

``` haskell
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Unicode.Stream as Unicode

-- Simulate network/db query by adding a delay
fetch :: String -> IO (String, String)
fetch w = threadDelay 1000000 >> return (w,w)

-- Read lines of whitespace separated list of words from a socket, fetch the
-- meanings of each word concurrently and return the meanings separated by
-- newlines, in same order as the words were received. Repeat until the
-- connection is closed.
lookupWords :: Socket -> IO ()
lookupWords sk =
      Stream.unfold Socket.read sk               -- SerialT IO Word8
    & Unicode.decodeLatin1                       -- SerialT IO Char
    & Stream.wordsBy isSpace Fold.toList         -- SerialT IO String
    & Stream.fromSerial                          -- AheadT  IO String
    & Stream.mapM fetch                          -- AheadT  IO (String, String)
    & Stream.fromAhead                           -- SerialT IO (String, String)
    & Stream.map show                            -- SerialT IO String
    & Stream.intersperse "\n"                    -- SerialT IO String
    & Unicode.encodeStrings Unicode.encodeLatin1 -- SerialT IO (Array Word8)
    & Stream.fold (Socket.writeChunks sk)        -- IO ()

serve :: Socket -> IO ()
serve sk = finally (lookupWords sk) (close sk)

-- | Run a server on port 8091. Accept and handle connections concurrently. The
-- connection handler is "serve" (i.e. lookupWords).  You can use "telnet" or
-- "nc" as a client to try it out.
main :: IO ()
main =
      Stream.unfold TCP.acceptOnPort 8091 -- SerialT IO Socket
    & Stream.fromSerial                   -- AsyncT IO ()
    & Stream.mapM serve                   -- AsyncT IO ()
    & Stream.fromAsync                    -- SerialT IO ()
    & Stream.drain                        -- IO ()
```

## Merging Incoming Streams

Assume you have logs coming from multiple nodes in your network and
you want to merge all the logs at line boundaries and send the merged
stream to a file or to a network destination. It uses the amazing
`concatMapWith` combinator to merge multiple streams concurrently.

See
[MergeServer.hs](https://github.com/composewell/streamly-examples/blob/master/examples/MergeServer.hs)
for full working code including the imports that we may have omitted below.

``` haskell
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Network.Socket as Socket

-- | Read a line stream from a socket. Note, lines are buffered, we could add
-- a limit to the buffering for safety.
readLines :: Socket -> SerialT IO (Array Char)
readLines sk =
    Stream.unfold Socket.read sk                 -- SerialT IO Word8
  & Unicode.decodeLatin1                         -- SerialT IO Char
  & Stream.splitWithSuffix (== '\n') Array.write -- SerialT IO String

recv :: Socket -> SerialT IO (Array Char)
recv sk = Stream.finally (liftIO $ close sk) (readLines sk)

-- | Starts a server at port 8091 listening for lines with space separated
-- words. Multiple clients can connect to the server and send streams of lines.
-- The server handles all the connections concurrently, merges the incoming
-- streams at line boundaries and writes the merged stream to a file.
server :: Handle -> IO ()
server file =
      Stream.unfold TCP.acceptOnPort 8090        -- SerialT IO Socket
    & Stream.concatMapWith Stream.parallel recv  -- SerialT IO (Array Char)
    & Stream.unfoldMany Array.read               -- SerialT IO Char
    & Unicode.encodeLatin1                       -- SerialT IO Word8
    & Stream.fold (Handle.write file)            -- IO ()

main :: IO ()
main = withFile "output.txt" AppendMode server
```

## Listing Directories Recursively/Concurrently

The following example lists a directory tree recursively, reading
multiple directories concurrently.

It uses the wonderful tree traversing combinator
`iterateMapLeftsWith`. It maps a stream generator on the `Left` values
(directories in this case) of the input stream, feeds the resulting
'Left' values back to the input, and lets the `Right` values (files in
this case) pass through to the output. The `Stream.ahead` stream joining
combinator makes it iterate on the directories concurrently.

See
[ListDir.hs](https://github.com/composewell/streamly-examples/blob/master/examples/ListDir.hs)
for full working code including the imports that we may have omitted below.

```haskell
...
import Streamly.Internal.Data.Stream.IsStream (iterateMapLeftsWith)

import qualified Streamly.Prelude as Stream
import qualified Streamly.Internal.FileSystem.Dir as Dir (toEither)

-- Lists a dir as a stream of (Either Dir File)
listDir :: String -> SerialT IO (Either String String)
listDir dir =
      Dir.toEither dir               -- SerialT IO (Either String String)
    & Stream.map (bimap mkAbs mkAbs) -- SerialT IO (Either String String)

    where mkAbs x = dir ++ "/" ++ x

-- | List the current directory recursively using concurrent processing
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    let start = Stream.yield (Left ".")
    Stream.iterateMapLeftsWith Stream.ahead listDir start
        & Stream.mapM_ print
```

## Rate Limiting

For bounded concurrent streams, stream yield rate can be specified. For
example, to print "tick" once every second you can simply write this:

``` haskell
main :: IO ()
main =
      Stream.repeatM (pure "tick")  -- AsyncT IO String
    & Stream.timestamped            -- AsyncT IO (AbsTime, String)
    & Stream.avgRate 1              -- AsyncT IO (AbsTime, String)
    & Stream.fromAsync              -- SerialT IO (AbsTime, String)
    & Stream.mapM_ print            -- IO ()
```

See
[Rate.hs](https://github.com/composewell/streamly-examples/blob/master/examples/Rate.hs)
for full working code.

Concurrency of the stream is automatically controlled to match the specified
rate. Rate control works precisely even at throughputs as high as millions of
yields per second. For more sophisticated rate control see the haddock
documentation.

## Reactive Programming

Streamly supports reactive and time domain programming inherently because of
declarative concurrency. See the `Streamly.Prelude` module for some time
specific combinators like `intervalsOf` and folds like `takeInterval` in
`Streamly.Internal.Data.Fold`.  Also see pre-release sampling combinators in
the `Streamly.Internal.Data.Stream.IsStream.Top` module including `throttle`
and `debounce` like operations.

See
[AcidRain.hs](https://github.com/composewell/streamly-examples/tree/master/examples/AcidRain.hs)
and
[CirclingSquare.hs](https://github.com/composewell/streamly-examples/tree/master/examples/CirclingSquare.hs).

## More examples

Many more examples can be found in the [streamly-examples
repository](https://github.com/composewell/streamly-examples).

Streamly comes equipped with a very powerful set of abstractions to accomplish
any kind of programming tasks that you may want to throw at it. It provides,
streams, arrays, file-io, fsnotify, network-io, time domain programming
(reactive programming). See the [streamly
documentation](https://streamly.composewell.com) to know more.

## Concurrency

Streamly uses lock-free synchronization for low overhead
concurrency. The number of tasks performed concurrently are
determined automatically based on the rate at which a consumer is
consuming the results. In other words, you do not need to manage
thread pools and decide how many threads to use for a particular
task.  For CPU bound tasks it tries to keep the number of threads
close to the number of CPUs available whereas for IO bound tasks
more threads can be utilized.

Parallelism can be utilized with little overhead even if
the task size is very small, because it can automatically
switch to serial mode or batch multiple tasks on the
same CPU if that is more efficient.  See [concurrency
benchmarks](https://github.com/composewell/concurrency-benchmarks) for
detailed performance results and a comparison with the `async` package.

## Performance

As you have seen above in the word count example, streamly enables
highly modular abstractions with the best possible performance (close to
an equivalent C program).

Streamly provides excellent performance even for byte level stream
operations, it is made possible by employing efficient abstractions like
`Unfold`s and terminating `Fold`s. Byte level stream operations make
programming simpler because you do not have to deal with chunking and
re-combining.

If you can write a program significantly faster in some other way or
with some other language, please let us know and we will improve.

## Benchmarks

We measured several Haskell streaming implementations
on various micro-benchmarks. Please see [streaming
benchmarks](https://github.com/composewell/streaming-benchmarks) page
for detailed comparison of streamly with other streaming libraries.

These results show that streamly is the fastest effectful streaming
implementation on almost all the measured micro benchmarks. In many cases
it is up to 100x faster and in some cases even 1000x faster. In many
composite operation benchmarks streamly turns out to be significantly
faster than the Haskell lists implementation.

## Design Goals

The goals of streamly from the very beginning have been, (1) simplicity
by unifying abstractions, (2) high performance. These are hard to
achieve at the same time because they are usually inversely related. We
have spent many years trying to get the abstractions right without
compromising performance.

`Unfold` is an example of an abstraction that we have created to
achieve high performance when mapping streams on streams. It allows
stream generation to be optimized well by the compiler, employing
stream fusion. `Fold` with termination capability is another example
which modularizes stream elimination operations with stream fusion.
Terminating folds can perform many simple parsing tasks that do not
require backtracking.  `Parser`s in streamly are a natural extension
of terminating `Fold`s just adding backtracking capability to folds.
Unification leads to simpler abstractions, lesser cognitive overhead
without compromising performance.

Streamly exploits GHC stream fusion optimizations (`case-of-case`
and `spec-constr`) aggressively to bring C like speed with highly
modular abstractions.  It performs very well without any compiler
plugins.  However, we have fixed some deficiencies in GHC optimizer
via a [compiler plugin](https://github.com/composewell/fusion-plugin).
We hope to bring these optimizations to GHC in future but until
then we recommend that you use the plugin for performance sensitive
applications.

## Installing and using

Please see [INSTALL.md](./INSTALL.md) for instructions on how to use streamly
with your Haskell build tool or package manager.

Streamly comes with batteries included, see [the
documentation](https://streamly.composewell.com) for available modules. Modules
are divided in two categories:

* Released Modules: these are modules that have a stable API, any API changes
  conform to a versioning policy.
* Pre-release APIs:  Some of the APIs that are recently introduced and
  require some soak time for stability are kept in the
  internal modules corresponding to the released module (e.g.
  Streamly.Internal.Data.Fold).
* Pre-release Modules: These modules are not yet released due to some planned
  changes in near future, they will be released soon.

We usually try to change even the unstable APIs in a major release version.

## Support

Please feel free to ask questions on the
[streamly gitter channel](https://gitter.im/composewell/streamly).
If you require professional support, consulting, training or timely
enhancements to the library please contact
[support@composewell.com](mailto:support@composewell.com).

## Credits

The following authors/libraries have influenced or inspired this library in a
significant way:

  * Roman Leshchinskiy ([vector](http://hackage.haskell.org/package/vector))
  * Gabriel Gonzalez ([foldl](https://hackage.haskell.org/package/foldl))
  * Alberto G. Corona ([transient](https://hackage.haskell.org/package/transient))

See the `credits` directory for full list of contributors, credits and licenses.

## Contributing

The code is available under BSD-3-Clause license
[on github](https://github.com/composewell/streamly). Join the [gitter
chat](https://gitter.im/composewell/streamly) channel for discussions.  Please
ask any questions on the gitter channel or [contact the maintainer
directly](mailto:streamly@composewell.com). All contributions are welcome!
