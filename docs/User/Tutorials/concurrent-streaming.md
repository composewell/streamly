## Streaming Concurrently

THIS DOC IS NOT READY.

Haskell lists express pure computations using composable stream operations like
`:`, `unfold`, `map`, `filter`, `zip` and `fold`.  Streamly is exactly like
lists except that it can express sequences of pure as well as monadic
computations aka streams. More importantly, it can express monadic sequences
with concurrent execution semantics without introducing any additional APIs.

Streamly expresses concurrency using standard, well known abstractions.
Concurrency semantics are defined for list operations, semigroup, applicative
and monadic compositions. Programmer does not need to know any low level
notions of concurrency like threads, locking or synchronization.  Concurrent
and non-concurrent programs are fundamentally the same.  A chosen segment of
the program can be made concurrent by annotating it with an appropriate
combinator.  We can choose a combinator for lookahead style or asynchronous
concurrency.  Concurrency is automatically scaled up or down based on the
demand from the consumer application, we can finally say goodbye to managing
thread pools and associated sizing issues.  The result is truly fearless
and declarative monadic concurrency.

## Concurrent Stream Generation

`consM` or its operator form `|:` can be used to construct a stream from
monadic actions. A stream constructed with `consM` can run the monadic actions
in the stream concurrently when used with appropriate stream type combinator
(e.g. `fromAsync`, `fromAhead` or `fromParallel`).

The following code finishes in 3 seconds (6 seconds when serial), note the
order of elements in the resulting output, the outputs are consumed as soon as
each action is finished (asyncly):

``` haskell
> let p n = threadDelay (n * 1000000) >> return n
> S.toList $ S.fromAsync $ p 3 |: p 2 |: p 1 |: S.nil
[1,2,3]
```

Use `fromAhead` if you want speculative concurrency i.e. execute the actions in
the stream concurrently but consume the results in the specified order:

``` haskell
> S.toList $ S.fromAhead $ p 3 |: p 2 |: p 1 |: S.nil
[3,2,1]
```

Monadic stream generation functions e.g. `unfoldrM`, `replicateM`, `repeatM`,
`iterateM` and `fromFoldableM` etc. can work concurrently.

The following finishes in 10 seconds (100 seconds when serial):

``` haskell
S.drain $ S.fromAsync $ S.replicateM 10 $ p 10
```

## Concurrent Streaming Pipelines

Use `|&` or `|$` to apply stream processing functions concurrently. The
following example prints a "hello" every second; if you use `&` instead of
`|&` you will see that the delay doubles to 2 seconds instead because of serial
application.

``` haskell
main = S.drain $
      S.repeatM (threadDelay 1000000 >> return "hello")
   |& S.mapM (\x -> threadDelay 1000000 >> putStrLn x)
```

## Mapping Concurrently

We can use `mapM` or `sequence` functions concurrently on a stream.

``` haskell
> let p n = threadDelay (n * 1000000) >> return n
> S.drain $ S.fromAhead $ S.mapM (\x -> p 1 >> print x) (S.fromSerial $ S.repeatM (p 1))
```

## Serial and Concurrent Merging

Semigroup and Monoid instances can be used to fold streams serially or
concurrently. In the following example we compose ten actions in the
stream, each with a delay of 1 to 10 seconds, respectively. Since all the
actions are concurrent we see one output printed every second:

``` haskell
import qualified Streamly.Prelude as S
import Control.Concurrent (threadDelay)

main = S.toList $ S.fromParallel $ foldMap delay [1..10]
 where delay n = S.fromEffect $ threadDelay (n * 1000000) >> print n
```

Streams can be combined together in many ways. We provide some examples
below, see the tutorial for more ways. We use the following `delay`
function in the examples to demonstrate the concurrency aspects:

``` haskell
import qualified Streamly.Prelude as S
import Control.Concurrent

delay n = S.fromEffect $ do
    threadDelay (n * 1000000)
    tid <- myThreadId
    putStrLn (show tid ++ ": Delay " ++ show n)
```
### Serial

``` haskell
main = S.drain $ delay 3 <> delay 2 <> delay 1
```
```
ThreadId 36: Delay 3
ThreadId 36: Delay 2
ThreadId 36: Delay 1
```

### Parallel

``` haskell
main = S.drain . S.fromParallel $ delay 3 <> delay 2 <> delay 1
```
```
ThreadId 42: Delay 1
ThreadId 41: Delay 2
ThreadId 40: Delay 3
```

## Concurrent Nested Loops

To run the above code with speculative concurrency i.e. each iteration in the
loop can run concurrently but the results are presented to the consumer of the
output in the same order as serial execution:

``` haskell
main = S.drain $ S.fromAhead $ loops
```

Different stream types execute the loop iterations in different ways. For
example, `fromWSerial` interleaves the loop iterations. There are several
concurrent stream styles to execute the loop iterations concurrently in
different ways, see the `Streamly.Tutorial` module for a detailed treatment.

## Rate Limiting

For bounded concurrent streams, stream yield rate can be specified. For
example, to print hello once every second you can simply write this:

``` haskell
import Streamly.Prelude as S

main = S.drain $ S.fromAsync $ S.avgRate 1 $ S.repeatM $ putStrLn "hello"
```

For some practical uses of rate control, see
[AcidRain.hs](https://github.com/composewell/streamly-examples/tree/master/examples/AcidRain.hs)
and
[CirclingSquare.hs](https://github.com/composewell/streamly-examples/tree/master/examples/CirclingSquare.hs)
.
Concurrency of the stream is automatically controlled to match the specified
rate. Rate control works precisely even at throughputs as high as millions of
yields per second. For more sophisticated rate control see the haddock
documentation.

