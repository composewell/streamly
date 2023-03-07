# Upgrading to streamly 0.9.0

`Streamly.Prelude` module has been deprecated, equivalent
functionality is covered by the `Streamly.Data.Stream`,
`Streamly.Data.Stream.Prelude`, and `Streamly.Data.Fold` modules. The
new modules use a monomorphic `Stream` type instead of the polymorphic
`IsStream t` type.

`Streamly.Data.Stream` module and the `Stream` type are designed
for writing high-performance fused pipelines not involving explicit
recursion. For writing code that may require recursive function calls,
`Streamly.Data.Stream.StreamK` module and the `StreamK` type have been
introduced which provide a CPS based stream implementation. `Stream` and
`StreamK` types can be easily interconverted. These changes have been made to
make performance robust and not rely on GHC rewrite rules which could be
fragile. For example, GHC 9.0.x had broken the rewrite rule which was
not fixed until GHC-9.2.2. This split also gives more power and
transparency of the performance behavior to the programmer.

Instead of using separate stream types for concurrent code, now
you have to use explicit concurrent combinators with appropriate
concurrency parameters. These combinators are available in the
`Streamly.Data.Stream.Prelude` module. This change has been made to allow
programmers to control concurrent behavior in a more robust way and
reduce pitfalls.

The old code can be adapted to use the new modules with some
changes. More details about this is supplied in the following
sections. Assume the following imports in the code snippets below:

```haskell docspec
>>> import qualified Streamly.Data.Stream as Stream
>>> import qualified Streamly.Data.StreamK as StreamK
>>> import qualified Streamly.Data.Stream.Prelude as Stream
>>> import qualified Streamly.Data.Fold as Fold
>>> import qualified Streamly.Data.Parser as Parser
```

## The `Stream` and `StreamK` types

The following types are removed:

* `IsStream`
* `ZipSerialM`
* `ZipAsyncM`
* `WSerialT`
* `WAsyncT`
* `SerialT`
* `ParallelT`
* `AsyncT`
* `AheadT`

In the new release, the `Stream` type is the primary stream type that
you will use most of the time. You can think of it as a replacement for
the `SerialT` type. However, it does not provide an Applicative or Monad
instance.

The `CrossStream` type in `Streamly.Internal.Data.Stream.StreamD` is a
wrapper over `Stream` type supplying the Monad instance. However, see
the "Performance Notes" section in the `Streamly.Data.Stream` module for
limitations of the `Stream` type. The `StreamK` type and `CrossStreamK`
(in Streamly.Internal.Data.Stream.StreamK) could be used to overcome the
limitations of `Stream` type.

If required, you can use the template-haskell functions in
`Streamly.Data.Stream.MkType` to create stream type wrappers (like
ZipSerialM or WSerialT) with custom Applicative or Monadic properties.
But in general, try to avoid specific types and use explicit functions
from the stream module.

`adapt` is not needed anymore.

```haskell docspec
>>> (.:) = StreamK.cons
>>> cons = StreamK.cons
>>> wSerial = StreamK.interleave
>>> serial = StreamK.append
>>> fromIndicesM f = Stream.mapM f $ Stream.enumerateFrom 0
>>> fromIndices f = fmap f $ Stream.enumerateFrom 0
>>> fromListM = Stream.sequence . Stream.fromList
>>> fromFoldable = StreamK.toStream . StreamK.fromFoldable
>>> fromFoldableM = Stream.sequence . fromFoldable
```

## Stream folding functions

Explicit stream fold functions have been omitted from the new stream
module. You can use the following equivalent definitions:

```haskell docspec
>>> toList = Stream.fold Fold.toList
>>> the = Stream.fold Fold.the
>>> sum = Stream.fold Fold.sum
>>> product = Stream.fold Fold.product
>>> or = Stream.fold Fold.or
>>> null = Stream.fold Fold.null
>>> elemIndex a = Stream.fold (Fold.elemIndex a)
>>> elem a = Stream.fold (Fold.elem a)
>>> notElem a = Stream.fold (Fold.notElem a)
>>> minimumBy ordering = Stream.fold (Fold.minimumBy ordering)
>>> minimum = Stream.fold Fold.minimum
>>> maximumBy ordering = Stream.fold (Fold.maximumBy ordering)
>>> maximum = Stream.fold Fold.maximum
>>> mapM_ f = Stream.fold (Fold.drainMapM f)
>>> lookup a = Stream.fold (Fold.lookup a)
>>> length = Stream.fold Fold.length
>>> last = Stream.fold Fold.latest
>>> head = Stream.fold Fold.one
>>> foldr f a = Stream.fold (Fold.foldr' f a)
>>> foldlM' f a = Stream.fold (Fold.foldlM' f a)
>>> foldl1 f = Stream.fold (Fold.foldl1' f)
>>> foldl' f a = Stream.fold (Fold.foldl' f a)
>>> findIndex eq = Stream.fold (Fold.findIndex eq)
>>> find eq = Stream.fold (Fold.find eq)
>>> findM eq = Stream.fold (Fold.findM eq)
>>> drainWhile predicate = Stream.fold Fold.drain . Stream.takeWhile predicate
>>> drainN i = Stream.fold Fold.drain . Stream.take i
>>> drain = Stream.fold Fold.drain
>>> any predicate = Stream.fold (Fold.any predicate)
>>> and = Stream.fold Fold.and
>>> all predicate = Stream.fold (Fold.all predicate)
>>> (!!) i = Stream.fold (Fold.index i)
>>> tail = Streamly.Internal.Data.Stream.StreamK.tail
>>> init = Streamly.Internal.Data.Stream.StreamK.init
>>> foldrM = Streamly.Internal.Data.Stream.StreamD.foldrM
```

Mapping functions:

```haskell docspec
>>> map = fmap
```

Similarly for scanning use `Stream.scan` or `Stream.postscan`
with an appropriate fold.

```haskell docspec
>>> scanl' f z = Stream.scan (Fold.foldl' f z)
>>> scanlM' f z = Stream.scan (Fold.foldlM' f z)
>>> postscanl' f z = Stream.postscan (Fold.foldl' f z)
>>> postscanlM' f z = Stream.postscan (Fold.foldlM' f z)
>>> scanl1' f = Stream.catMaybes . Stream.scan (Fold.foldl1' f)
>>> scanl1M' f = Stream.catMaybes . Stream.scan (Fold.foldlM1' f)
>>> concatMapWith = StreamK.concatMapWith
>>> concatFoldableWith f = Prelude.foldr f StreamK.nil
>>> concatMapFoldableWith f g = Prelude.foldr (f . g) StreamK.nil
>>> concatForFoldableWith f xs g = Prelude.foldr (f . g) StreamK.nil xs
```

Filters:

```haskell docspec
>>> deleteBy cmp x = Stream.scanMaybe (Fold.deleteBy cmp x)
>>> findIndices p = Stream.scanMaybe (Fold.findIndices p)
>>> elemIndices a = findIndices (== a)
```

Direct implementations of most of these folds, scans and filters are also
available in the `Streamly.Internal.Data.Stream.StreamD` module. Those may in
fact be better fusible in some situations.

## Stream splitting and grouping functions

Stream splitting and grouping functions like `splitOn`, `wordsBy`, and
`groupsBy` have been omitted from the new stream module as these can
now be implemented using `foldMany` and an appropriate fold from the
`Streamly.Data.Fold` module or using `parseMany` and an appropriate
parser from the `Streamly.Data.Parser` module.

```haskell docspec
>>> uniq = Stream.scanMaybe (Fold.uniqBy (==))
>>> splitWithSuffix predicate f = Stream.foldMany (Fold.takeEndBy predicate f)
>>> splitOn = Streamly.Internal.Data.Stream.StreamD.splitOn
>>> splitOnSuffix predicate f = Stream.foldMany (Fold.takeEndBy_ predicate f)
>>> indexedR = Streamly.Internal.Data.Stream.StreamD.indexedR
>>> groupsBy eq fld = Stream.parseMany (Parser.groupBy eq fld)
>>> groups = groupsBy (==)
>>> groupsByRolling = Streamly.Internal.Data.Stream.StreamD.groupsRollingBy
>>> wordsBy p f = Stream.parseMany (Parser.wordBy p f)
>>> chunksOf n f = Stream.foldMany (Fold.take n f)
```

Direct implementation of these are also available in
`Streamly.Internal.Data.Stream.StreamD`.

## Concurrency

Earlier, concurrent and non-concurrent code used the same
combinators. The code was made concurrent by using different concurrent
stream types such as `AsyncT`, `ParallelT` etc. Now you use the same
stream type everywhere, you have to choose a concurrent combinator
for concurrent behavior.  For example, use `mapM` for serial behavior
and `parMapM` for concurrent behavior. Concurrent combinators can
be imported from `Streamly.Data.Stream.Prelude` module. Concurrent
combinators are prefixed with `par`.

Parallel combinators take a concurrency config argument to specify the
concurrency control parameters.  The following combinators have the same
meaning as before except that they are used to set the config parameters
instead of being applied on the stream.
* `rate`
* `maxRate`
* `constRate`
* `avgRate`
* `minRate`
* `maxThreads`
* `maxBuffer`

A stream is evaluated asynchronously using `parEval`:

```haskell docspec
>>> :set -XFlexibleContexts
>>> mkAsync = Stream.parEval id
```

Earlier `consM` was used to create an implicitly concurrent stream of
actions. In the new release, an equivalent effect is achieved by using
a serial `consM` to create a stream of actions and then explicitly using
`Stream.parEval` on it to evaluate it concurrently.

```haskell docspec
>>> consM = StreamK.consM
>>> (|:) = consM
```

Note that you will have to use `StreamK.toStream` to covert it to `Stream`
before using `parEval` on it.

Existing generation combinators that can be implemented using new primitives:

```haskell docspec
>>> repeatM = Stream.parRepeatM
>>> replicateM = Stream.parReplicateM
>>> unfoldrM step = Stream.parEval id . Stream.unfoldrM step
>>> iterateM step = Stream.parEval id . Stream.iterateM step
>>> fromIndicesM f = Stream.parEval id . fromIndicesM f
>>> fromListM = Stream.parSequence id . Stream.fromList
>>> fromFoldableM = Stream.parSequence id . StreamK.toStream . StreamK.fromFoldable
```

Existing transformation combinators that can be implemented using `parEval`:

```haskell docspec
>>> (|$.) f = f . Stream.parEval id
>>> (|&.) = flip (|$.)
>>> (|$) f = f . Stream.parEval id
>>> (|&) = flip (|$)
>>> sequence = Stream.parSequence
>>> mapM = Stream.parMapM
```

`parList` is used to evaluate multiple streams concurrently and combine the
outputs. Existing combinators that can be implemented using `parList`:

```haskell docspec
>>> async x y = Stream.parList id [x, y]
>>> wAsync x y = Stream.parList (Stream.interleaved True) [x, y]
>>> parallel x y = Stream.parList (Stream.eager True) [x, y]
>>> ahead x y = Stream.parList (Stream.ordered True) [x, y]
```

Concurrent zipping and merging combinators:

```haskell docspec
>>> zipAsyncWithM = Stream.parZipWith id
>>> zipAsyncWith = Stream.parZipWith id
>>> mergeAsyncByM = Stream.parMergeByM id
>>> mergeAsyncBy = Stream.parMergeBy id
```

The equivalent of `concatMapWith` using a concurrent combining operation in the
new release is `parConcatMap`. The config argument in `parConcatMap` can
specify an equivalent of the combining operation. Similarly, concurrent
`concatFoldableWith`, `concatMapFoldableWith`, `concatForFoldableWith` can also
be expressed using `parConcatMap`.