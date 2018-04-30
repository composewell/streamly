# Streamly

[![Hackage](https://img.shields.io/hackage/v/streamly.svg?style=flat)](https://hackage.haskell.org/package/streamly)
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

See the haddock documentation for full reference.  It is recommended that you
read `Streamly.Tutorial` first. Also see `Streamly.Examples` for some working
examples.

## Non-determinism

The monad instance composes like a list monad.

``` haskell
import Streamly
import qualified Streamly.Prelude as S

loops = do
    x <- S.fromFoldable [1,2]
    y <- S.fromFoldable [3,4]
    liftIO $ putStrLn $ show (x, y)

main = runStream loops
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
main = runStream $ coparallely $ loops
```

To run it with round-robin parallelism:

``` haskell
main = runStream $ parallely $ loops
```

To run it serially but interleaving the outer and inner loop iterations:

``` haskell
main = runStream $ costreamly $ loops
```

Streams can perform semigroup (<>) and monadic bind (>>=) operations
concurrently using combinators like `coparallelly`, `parallelly`. For example,
to concurrently generate squares of a stream of numbers and then concurrently
sum the square roots of all combinations of two streams:

``` haskell
import Streamly
import qualified Streamly.Prelude as S

main = do
    s <- S.sum $ coparallely $ do
        -- Each square is performed concurrently, (<>) is concurrent
        x2 <- foldMap (\x -> return $ x * x) [1..100]
        y2 <- foldMap (\y -> return $ x * x) [1..100]
        -- Each addition is performed concurrently, monadic bind is concurrent
        return $ sqrt (x2 + y2)
    print s
```

Of course, the actions running in parallel could be arbitrary IO actions.  For
example, to concurrently list the contents of a directory tree recursively:

``` haskell
import Path.IO (listDir, getCurrentDir)
import Streamly

main = runStream $ coparallely $ getCurrentDir >>= readdir
   where readdir d = do
            (dirs, files) <- liftIO $ listDir d
            liftIO $ mapM_ putStrLn $ map show files
            -- read the subdirs concurrently, (<>) is concurrent
            foldMap readdir dirs
```

In the above examples we do not think in terms of threads, locking or
synchronization, rather we think in terms of what can run in parallel, the rest
is taken care of automatically. When using `coparallely` the programmer does
not have to worry about how many threads are to be created they are
automatically adjusted based on the demand of the consumer.

The concurrency facilities provided by streamly can be compared with
[OpenMP](https://en.wikipedia.org/wiki/OpenMP) and
[Cilk](https://en.wikipedia.org/wiki/Cilk) but with a more declarative
expression.

## Streaming

Streaming is effortless, simple and straightforward. Streamly data type behaves
just like a list and combinators are provided in `Streamly.Prelude` to
transform or fold streamly streams. Unlike other libraries and like `streaming`
library the combinators explicitly consume a stream and produce a stream,
therefore, no special operator is needed to join stream stages, just a forward
(`$`) or reverse (`&`) function application operator is enough.

```haskell
import Streamly
import qualified Streamly.Prelude as S
import Data.Function ((&))

main = S.fromFoldable [1..10]
     & fmap (+ 1)
     & S.drop 2
     & S.filter even
     & fmap (* 3)
     & S.takeWhile (< 25)
     & S.mapM (\x -> putStrLn ("saw " ++ show x) >> return x)
     & S.toList
     >>= print
```

Fold style combinators can be used to fold purely or monadically. You can also
use the beautiful `foldl` library for folding.

```haskell
main = S.fromFoldable [1..10]
     & S.foldl (+) 0 id
     >>= print
```

Streams can be combined together in multiple ways:

```haskell
main = do
    let p s = toList s >>= print
    -- serial, this is the default even if you omit the `streamly` combinator
    p $ streamly    $ S.fromFoldable [1..10] <> S.fromFoldable [11..20]
    -- serial but interleaved
    p $ costreamly  $ S.fromFoldable [1..10] <> S.fromFoldable [11..20]
    -- left-biased, demand driven parallel
    p $ coparallely $ S.fromFoldable [1..10] <> S.fromFoldable [11..20]
    -- round-robin parallel
    p $ parallely   $ S.fromFoldable [1..10] <> S.fromFoldable [11..20]
```

As we have already seen streams can be combined using monadic composition in a
non-deterministic manner. This allows arbitrary manipulation and combining of
streams. See `Streamly.Examples.MergeSortedStreams` for a more complicated
example.

## Reactive Programming (FRP)

Streamly is a foundation for first class reactive programming as well by virtue
of integrating concurrency and streaming. See `Streamly.Examples.AcidRainGame`
and `Streamly.Examples.CirclingSquare` for an SDL based animation example.

## Performance

`Streamly` has best in class performance even though it generalizes streaming
to concurrent composition that does not mean it sacrifices non-concurrent
performance. See
[streaming-benchmarks](https://github.com/composewell/streaming-benchmarks) for
detailed performance comparison with regular streaming libraries and the
explanation of the benchmarks. The following graphs show a summary, the first
one measures how four pipeline stages in a series perform, the second one
measures the performance of individual stream operations; in both cases the
stream processes a million elements:

![Composing Pipeline Stages](charts/comparative/ComposingPipelineStages.svg)
![All Operations at a Glance](charts/comparative/AllOperationsataGlance.svg)

## Contributing

The code is available under BSD-3 license
[on github](https://github.com/composewell/streamly). Join the
[gitter chat](https://gitter.im/composewell/streamly) channel for discussions.
You can find some of the
[todo items on the github wiki](https://github.com/composewell/streamly/wiki/Things-To-Do).
Please ask on the gitter channel or [contact the maintainer directly](mailto:harendra.kumar@gmail.com)
for more details on each item. All contributions are welcome!

This library was originally inspired by the `transient` package authored by
Alberto G. Corona.
