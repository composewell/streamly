# Streamly

[![Gitter chat](https://badges.gitter.im/composewell/gitter.svg)](https://gitter.im/composewell/streamly)
[![Build Status](https://travis-ci.org/composewell/streamly.svg?branch=master)](https://travis-ci.org/composewell/streamly)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/ajxg0c79raou9ned?svg=true)](https://ci.appveyor.com/project/harendra-kumar/streamly)
[![Coverage Status](https://coveralls.io/repos/composewell/streamly/badge.svg?branch=master&service=github)](https://coveralls.io/github/composewell/streamly?branch=master)

## Stream`ing` `Concurrent`ly

Streamly is a monad transformer unifying non-determinism
([list-t](https://hackage.haskell.org/package/list-t)/[logict](https://hackage.haskell.org/package/logict)),
concurrency ([async](https://hackage.haskell.org/package/async)),
streaming ([conduit](https://hackage.haskell.org/package/conduit)\/[pipes](https://hackage.haskell.org/package/pipes)),
and FRP ([Yampa](https://hackage.haskell.org/package/Yampa)\/[reflex](https://hackage.haskell.org/package/reflex))
functionality in a concise and intuitive API.
High level concurrency makes concurrent applications almost indistinguishable
from non-concurrent ones.  By changing a single combinator you can control
whether the code runs serially or concurrently.  It naturally integrates
concurrency with streaming rather than adding it as an afterthought.
Moreover, it interworks with the popular streaming libraries.

See the haddock documentation for full reference.  It is recommended to read
the comprehensive tutorial module `Streamly.Tutorial` first. Also see
`Streamly.Examples` for some working examples.

## Non-determinism

The monad instance composes like a list monad.

``` haskell
loops = $ do
    x <- each [1,2]
    y <- each [3,4]
    liftIO $ putStrLn $ show (x, y)

main = runStreaming $ serially $ loops
```
```
(1,3)
(1,4)
(2,3)
(2,4)
```

## Magical Concurrency

To run the above code with demand-driven concurrency i.e. each iteration in the
loops can run concurrently depending on the consumer rate:

``` haskell
main = runStreaming $ asyncly $ loops
```

To run it with full parallelism irrespective of demand:

``` haskell
main = runStreaming $ parallely $ loops
```

To run it serially but interleaving the outer and inner loop iterations:

``` haskell
main = runStreaming $ interleaving $ loops
```

You can fold multiple streams or IO actions using parallel combinators like
`<|`, `<|>`. For example, to concurrently generate the squares and then
concurrently sum the square roots of all combinations:

``` haskell
main = do
  print $ sum $ asyncly $ do
      -- Squaring is concurrent (<|)
      x2 <- forEachWith (<|) [1..100] $ \x -> return $ x * x
      y2 <- forEachWith (<|) [1..100] $ \y -> return $ y * y
      -- sqrt is concurrent (asyncly)
      return $ sqrt (x2 + y2)
```

Of course, the actions running in parallel could be arbitrary IO actions.  To
concurrently list the contents of a directory tree recursively:

``` haskell
import Path.IO (listDir, getCurrentDir)
import Streamly

main = runStreaming $ serially $ getCurrentDir >>= readdir
   where readdir d = do
            (dirs, files) <- lift $ listDir d
            liftIO $ mapM_ putStrLn $ map show files
            -- read the subdirs concurrently
            foldMapWith (<|>) readdir dirs
```

In the above examples we do not think in terms of threads, locking or
synchronization, rather we think in terms of what can run in parallel, the rest
is taken care of automatically. With `asyncly` and `<|` the programmer does not
have to worry about how many threads are to be created they are automatically
adjusted based on the demand of the consumer.

The concurrency facilities provided by streamly can be compared with
[OpenMP](https://en.wikipedia.org/wiki/OpenMP) and
[Cilk](https://en.wikipedia.org/wiki/Cilk) but with a more declarative
expression.  Concurrency support does not compromise performance in
non-concurrent cases, the performance of the library is at par or better than
most of the existing streaming libraries.

## Streaming

Streaming is effortless, simple and straightforward. Streamly data type behaves
just like a list and combinators are provided in `Streamly.Prelude` to
transform or fold streamly streams. Unlike other libraries and like `streaming`
library the combinators explicitly consume a stream and produce a stream,
therefore, no special operator is needed to join stream stages, just a forward
(`$`) or reverse (`&`) function application operator is enough.

```haskell
import Streamly
import Streamly.Prelude as S
import Data.Function ((&))

main = S.each [1..10]
     & fmap (+ 1)
     & S.drop 2
     & S.filter even
     & fmap (* 3)
     & S.takeWhile (< 25)
     & S.mapM (\x -> putStrLn ("saw " ++ show x) >> return x)
     & S.toList . serially
     >>= print
```

Fold style combinators can be used to fold purely or monadically. You can also
use the beautiful `foldl` library for folding.

```haskell
main = S.each [1..10]
     & serially
     & S.foldl (+) 0 id
     >>= print
```

Streams can be combined together in multiple ways:

```haskell
return 1 <> return 2               -- serial, combine atoms
S.each [1..10] <> S.each [11..20]  -- serial
S.each [1..10] <| S.each [11..20]  -- demand driven parallel
S.each [1..10] <=> S.each [11..20] -- serial but interleaved
S.each [1..10] <|> S.each [11..20] -- fully parallel
```

As we have already seen streams can be combined using monadic composition in a
non-deterministic manner. This allows arbitrary manipulation and combining of
streams. See `Streamly.Examples.MergeSortedStreams` for a more complicated
example.

## Reactive Programming (FRP)

Streamly is a foundation for first class reactive programming as well by virtue
of integrating concurrency and streaming. See `Streamly.Examples.AcidRainGame`
and `Streamly.Examples.CirclingSquare` for an SDL based animation example.

## Contributing

The code is available under BSD-3 license [on
github](https://github.com/composewell/streamly). Join the [gitter
chat](https://gitter.im/composewell/streamly) channel for discussions. All
contributions are welcome!

This library was originally inspired by the `transient` package authored by
Alberto G. Corona.
