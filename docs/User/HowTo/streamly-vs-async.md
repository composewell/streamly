# Streamly

THIS TUTORIAL IS OBSOLETE.

Streamly is a library to make concurrent programming a joy. The venerable
`async` package is the go to package for concurrent programming for most
Haskellers. Streamly is a higher level library than `async` and provides a lot
more power and functionality, using a simpler and concise expression of
concurrency. At a high level, you should be able to express everything with
streamly that you can with `async`, if you can't please raise an issue. If you
are familiar with `async`, in this document we highlight how streamly can be
used where you would use `async`.

## `async/wait` vs Concurrent Streams

Unlike `async`, streamly does not use a spawn and `wait` model.  Streamly uses
a more high level approach to concurrency and has no explicit notion of
threads. In streamly, we compose multiple actions as a stream and then express
whether you want to run the actions in the stream `serially` or `parallely`.
There are many different ways in which you can run streams concurrently, see
the reference documentation for details.

Since there is no explicit notion of threads in streamly, there are no
equivalents of `async`, `wait`, `cancel`, `poll` or `link` combinators from the
`async` package.

Since streamly is a monad transformer it can work with all monads and not just
IO, you won't need adaptations like `lifted-async` to use it for a generic
monad.

## Using Streamly for Concurrency

You can write all of your program in a streamly monad and use the full power of
the library.  Streamly can be used as a direct replacement of the IO monad with
no loss of performance, and no change in code except using `liftIO` or `fromEffect`
to run any IO actions.  Streamly IO monads (e.g. `SerialT IO`) are just a
generalization of the IO monad with non-deterministic composition of streams
added on top.

However, if you would like to just run only some concurrent portions of your
program using streamly, you can do that too. Just use `drain` if you want
to run the stream without collecting the outputs of the concurrent actions or
use `toList` if you want to convert the output stream into a list.  Other
stream folding operations can also be used, see the docs for more details.

## Features as Compared with `async`

Use the following imports to run the snippets shown below:

```haskell
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as F
import qualified Data.Text as Text
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, SomeException)
import Control.Monad.Catch (throwM, try)
```

Let us simulate a URL fetch with a delay of `n` seconds using the following
functions:

```haskell
getURL :: Int -> IO String
getURL n = threadDelay (n * 1000000) >> return (show n)
getURLString = getURL
getURLText n = getURL n >>= return . Text.pack
```

### concurrently

You can run any number of actions concurrently. For example, to fetch two URLs
concurrently:

```haskell
getUrlsConcurrently = S.parSequence id $ S.fromList [getURL 2, getURL 1]
```

This would return the results in their arrival order i.e. first 1 and then 2.
If you want to preserve the order of the results, use the lookahead style
using `fromAhead` instead. In the following example both URLs are fetched
concurrently, and even though URL 1 arrives before URL 2 the results will
return 2 first and then 1.

```haskell
getUrlsOrdered = S.parSequence (S.ordered True) $ S.fromList [getURL 2, getURL 1]
```

### concurrently_

Use `drain` instead of `toList` to run the actions but ignore the results:

```haskell
drainUrlsConcurrently = S.fold F.drain $ S.parMapM id getURL $ S.fromList [1, 2]
```

### Concurrent Zipping

If the actions that you are executing result in different output types you can
use zip to collect the results or to directly apply them to a function:

```haskell
concurrentZipping =
    S.parZipWith id (,) (S.fromEffect (getURLString 1)) (S.fromEffect (getURLText 2))
```

### race

There are two ways to achieve the race functionality, using `take` or using
exceptions.

#### `race` Using `take`

We can run multiple actions concurrently and take the first result that
arrives:

```haskell
fastest = S.toList $ S.take 1 $ S.parSequence id $ S.fromList [getURL 1, getURL 2]
```

After the first result arrives, the rest of the actions are canceled
automatically.  In general, we can take first `n` results as they arrive:

```haskell
fastestN n = S.toList $ S.take n $ S.parSequence id $ S.fromList [getURL 1, getURL 2]
```

#### `race` Using Exceptions

When an exception occurs in a concurrent stream all the concurrently running
actions are cacnceled on arrival of the exception. This can be used to
implement the race functionality. Each action in the stream can use an
exception to communicate the result. As soon as the first result arrives all
other actions will be canceled, for example:

```haskell
data Result = Result String deriving Show
instance Exception Result

raceUsingExceptions = do
    url <- try $ S.fold F.drain $ S.parSequence id $ S.fromList
               [ (getURL 2 >>= throwM . Result)
               , (getURL 1 >>= throwM . Result)
               ]
    case url of
        Left (e :: SomeException) -> print e
        Right _ -> undefined
```

### mapConcurrently

There are many ways to map concurrently on a container and collect the results.
The recommended way is to first convert itinto a stream and then map an action
on the stream concurrently:

```haskell
mapConcurrently = S.toList $ S.parMapM id getURL $ S.fromList [1..3]
```

### replicateConcurrently

Streamly has not just the equivalent of `replicateConcurrently` which is
`parReplicateM` but many more ways to generate concurrent streams, for example,
`parRepeatM`, `parBuffered`, etc. See the `Streamly.Data.Stream.Prelude` module
documentation for more details.

```haskell
replicateConcurrently = S.toList $ S.parReplicateM id 2 $ getURL 1
```

### Functor

The stream resulting from concurrent actions can be mapped serially or
concurrently.

To map serially just use `fmap`:

```haskell
serialMap = S.toList $ fmap (+1) (S.fromList [1, 2] :: S.Stream IO Int)

```

To map a monadic action concurrently on all elements of the stream use `parMapM`:

```haskell
concurrentMap =
    S.toList $ S.parMapM id (\x -> pure (x + 1) :: IO Int) $ S.fromList [1, 2]
```

### Semigroup

The `Semigroup` instances of streamly merge multiple streams serially or
concurrently.

### Monad

The `Monad` instances of streamly nest loops concurrently (concurrent
non-determinism).

### Performance

Streamly has very little concurrency overhead (ranging from a few 100
nanoseconds to a few microseconds on a 2.2 GHz Intel Core i7), you can even run
very lightweight actions in parallel without worrying about the overhead of
concurrency. See the performance benchmarks [comparing streamly with the `async`
package in this repo](https://github.com/composewell/concurrency-benchmarks).

## Further Reading

There is much more that you can do with streamly. For example, you can use the
`maxThreads` combinator to restrict the total number of concurrent threads or
use the `maxBuffer` combinator to restrict the total number of bufferred
results or you can use the `avgRate` combinator to control the rate at which
the concurrent actions are executed.

See the [haddock documentation on
hackage](https://hackage.haskell.org/package/streamly) and [a comprehensive tutorial
here](https://streamly.composewell.com/streamly-0.8.2/Tutorial.html).

## References

* https://hackage.haskell.org/package/async
* https://hackage.haskell.org/package/lifted-async
