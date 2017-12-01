# Streamly

[![Gitter chat](https://badges.gitter.im/composewell/gitter.svg)](https://gitter.im/composewell/streamly)
[![Build Status](https://travis-ci.org/composewell/streamly.svg?branch=master)](https://travis-ci.org/composewell/streamly)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/ajxg0c79raou9ned?svg=true)](https://ci.appveyor.com/project/harendra-kumar/streamly)
[![Coverage Status](https://coveralls.io/repos/composewell/streamly/badge.svg?branch=master&service=github)](https://coveralls.io/github/composewell/streamly?branch=master)

## Stream`ing` `Concurrent`ly
Streamly unifies concurrency and streaming in a single monad transformer with a
concise and simple API. It provides two ways to combine streams, a monadic
product composition as well as the standard pipelined composition provided by
streaming libraries. A natural extension of regular monadic composition to
streaming and concurrency makes it intuitive and concise with almost universal
application.  You can write concurrent or non-concurrent applications using
simple IO, logic programming, streaming IO or reactive programming (FRP) using
the same API. You can also think about it as representing concurrent and
composable state machines in imperative terms. It unifies the core
functionality provided by [async](https://hackage.haskell.org/package/async),
[logict](https://hackage.haskell.org/package/logict),
[list-t](https://hackage.haskell.org/package/list-t),
[conduit](https://hackage.haskell.org/package/conduit)\/[pipes](https://hackage.haskell.org/package/pipes),
[Yampa](https://hackage.haskell.org/package/Yampa)\/[reflex](https://hackage.haskell.org/package/reflex)
under one type and API. It interworks with the existing streaming libraries.

## Magical Concurrency
Streamly provides high level concurrency primitives (higher level than async)
and hides the low level concurrency details completely from the programmer.
Concurrency can be used with ease in applicative or monadic contexts.  The
programmer just expresses whether a task can run in parallel with another.
Threads, synchronization and concurrency rate control are handled
automatically under the hood. The concurrency facilities provided by streamly
can be compared with [OpenMP](https://en.wikipedia.org/wiki/OpenMP) and
[Cilk](https://en.wikipedia.org/wiki/Cilk) but with a more declarative
expression.  Concurrency support does not compromise performance in
non-concurrent cases, the performance of the library is at par or better than
most of the existing streaming libraries.

## Example
Here is a simple example to concurrently and recursively list the contents of a
directory tree:

``` haskell
import Path.IO (listDir, getCurrentDir)
import Streamly

main = runStreaming $ serially $ getCurrentDir >>= readdir
   where readdir d = do
            (dirs, files) <- lift $ listDir d
            liftIO $ mapM_ putStrLn $ map show files
            foldMapWith (<|>) readdir dirs
```

See "Streamly.Tutorial" and "Streamly.Examples" for more details.

This library was originally inspired by the `transient`
package authored by Alberto G. Corona.
