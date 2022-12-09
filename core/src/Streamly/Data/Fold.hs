-- |
-- Module      : Streamly.Data.Fold
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- A 'Fold' is a sink or a consumer of a stream of values.  The 'Fold' type
-- consists of an accumulator and an effectful action that absorbs a value into
-- the accumulator.
--
-- >>> import Data.Function ((&))
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Stream as Stream
--
-- For example, a 'sum' Fold represents adding the input to the accumulated
-- sum.  A fold driver pushes values from a stream to the 'Fold' one at a time,
-- reducing the stream to a single value.
--
-- >>> Stream.fold Fold.sum $ Stream.fromList [1..100]
-- 5050
--
-- Conceptually, a 'Fold' is a data type that can mimic a strict left fold
-- ('Data.List.foldl') as well as lazy right fold ('Prelude.foldr').  The above
-- example is similar to a left fold using @(+)@ as the step and @0@ as the
-- initial value of the accumulator:
--
-- >>> Data.List.foldl' (+) 0 [1..100]
-- 5050
--
-- 'Fold's have an early termination capability e.g. the 'one' fold would
-- terminate on an infinite stream:
--
-- >>> Stream.fold Fold.one $ Stream.fromList [1..]
-- Just 1
--
-- The above example is similar to the following right fold:
--
-- >>> Prelude.foldr (\x _ -> Just x) Nothing [1..]
-- Just 1
--
-- 'Fold's can be combined together using combinators. For example, to create a
-- fold that sums first two elements in a stream:
--
-- >>> sumTwo = Fold.take 2 Fold.sum
-- >>> Stream.fold sumTwo $ Stream.fromList [1..100]
-- 3
--
-- Folds can be combined to run in parallel on the same input. For example, to
-- compute the average of numbers in a stream without going through the stream
-- twice:
--
-- >>> avg = Fold.teeWith (/) Fold.sum (fmap fromIntegral Fold.length)
-- >>> Stream.fold avg $ Stream.fromList [1.0..100.0]
-- 50.5
--
-- Folds can be combined so as to partition the input stream over multiple
-- folds. For example, to count even and odd numbers in a stream:
--
-- >>> split n = if even n then Left n else Right n
-- >>> stream = fmap split $ Stream.fromList [1..100]
-- >>> countEven = fmap (("Even " ++) . show) Fold.length
-- >>> countOdd = fmap (("Odd "  ++) . show) Fold.length
-- >>> f = Fold.partition countEven countOdd
-- >>> Stream.fold f stream
-- ("Even 50","Odd 50")
--
-- Terminating folds can be combined to parse the stream serially such that the
-- first fold consumes the input until it terminates and the second fold
-- consumes the rest of the input until it terminates:
--
-- >>> f = Fold.splitWith (,) (Fold.take 8 Fold.toList) (Fold.takeEndBy (== '\n') Fold.toList)
-- >>> Stream.fold f $ Stream.fromList "header: hello\n"
-- ("header: ","hello\n")
--
-- A 'Fold' can be applied repeatedly on a stream to transform it to a stream
-- of fold results. To split a stream on newlines:
--
-- >>> f = Fold.takeEndBy (== '\n') Fold.toList
-- >>> Stream.fold Fold.toList $ Stream.foldMany f $ Stream.fromList "Hello there!\nHow are you\n"
-- ["Hello there!\n","How are you\n"]
--
-- Similarly, we can split the input of a fold too:
--
-- >>> Stream.fold (Fold.many f Fold.toList) $ Stream.fromList "Hello there!\nHow are you\n"
-- ["Hello there!\n","How are you\n"]
--
-- Please see "Streamly.Internal.Data.Fold" for additional @Pre-release@
-- functions.
--
-- = Folds vs. Streams
--
-- We can often use streams or folds to achieve the same goal. However, streams
-- are more efficient in composition of producers (e.g.
-- 'Data.Stream.append' or 'Data.Stream.mergeBy') whereas folds are
-- more efficient in composition of consumers (e.g.  'splitWith', 'partition'
-- or 'teeWith').
--
-- Streams are producers, transformations on streams happen on the output side:
--
-- >>> :{
--  f stream =
--        Stream.filter odd stream
--      & fmap (+1)
--      & Stream.fold Fold.sum
-- :}
--
-- >>> f $ Stream.fromList [1..100 :: Int]
-- 2550
--
-- Folds are stream consumers with an input stream and an output value, stream
-- transformations on folds happen on the input side:
--
-- >>> :{
-- f =
--        Fold.filter odd
--      $ Fold.lmap (+1)
--      $ Fold.sum
-- :}
--
-- >>> Stream.fold f $ Stream.fromList [1..100 :: Int]
-- 2550
--
-- Notice the similiarity in the definition of @f@ in both cases, the only
-- difference is the composition by @&@ vs @$@ and the use @lmap@ vs @map@, the
-- difference is due to output vs input side transformations.

module Streamly.Data.Fold
    (
    -- * Fold Type

      Fold -- (..)
    , Tee (..)

    -- * Constructors
    , foldl'
    , foldlM'
    , foldl1'
    , foldr'

    -- * Folds
    -- ** Accumulators
    -- | Folds that never terminate, these folds are much like strict left
    -- folds. 'mconcat' is the fundamental accumulator.  All other accumulators
    -- can be expressed in terms of 'mconcat' using a suitable Monoid.  Instead
    -- of writing folds we could write Monoids and turn them into folds.

    -- Monoids
    , sconcat
    , mconcat
    , foldMap
    , foldMapM

    -- Reducers
    , drain
    , drainMapM
    , length
    , sum
    , product
    , mean
    , rollingHash
    , rollingHashWithSalt

    -- Collectors
    , toList
    , toListRev

    -- ** Non-Empty Accumulators
    -- | Accumulators that do not have a default value, therefore, return
    -- 'Nothing' on an empty stream.
    , latest
    , maximumBy
    , maximum
    , minimumBy
    , minimum

    -- ** Filtering Scanners
    -- | Accumulators that are usually run as a scan using the 'scanMaybe'
    -- combinator.
    , uniq
    , uniqBy
    , deleteBy
    , findIndices
    , elemIndices

    -- ** Terminating Folds
    -- | These are much like lazy right folds.

    , one
    , null
    -- , satisfy
    -- , maybe

    , index
    , the
    , find
    , lookup
    , findIndex
    , elemIndex
    , elem
    , notElem
    , all
    , any
    , and
    , or

    -- * Running A Fold
    , drive
    , driveBreak

    -- * Building Incrmentally
    , extractM
    , reduce
    , snoc
    -- , snocl
    , snocM
    -- , snoclM
    , augment
    , duplicate
    -- , isClosed

    -- * Combinators
    -- | Combinators are modifiers of folds.  In the type @Fold m a b@, @a@ is
    -- the input type and @b@ is the output type.  Transformations can be
    -- applied either on the input side (contravariant) or on the output side
    -- (covariant).  Therefore, combinators are of one of the following general
    -- shapes:
    --
    -- * @... -> Fold m a b -> Fold m c b@ (input transformation)
    -- * @... -> Fold m a b -> Fold m a c@ (output transformation)
    --
    -- The input side transformations are more interesting for folds.  Most of
    -- the following sections describe the input transformation operations on a
    -- fold. When an operation makes sense on both input and output side we use
    -- the prefix @l@ (for left) for input side operations and the prefix @r@
    -- (for right) for output side operations.

    -- ** Mapping on output
    -- | The 'Functor' instance of a fold maps on the output of the fold:
    --
    -- >>> Stream.fold (fmap show Fold.sum) (Stream.enumerateFromTo 1 100)
    -- "5050"
    --
    , rmapM

    -- ** Mapping on Input
    , lmap
    , lmapM

    -- ** Scanning and Filtering
    , scan
    , postscan
    , scanMaybe
    , filter
    , filterM

    -- -- ** Mapping Filters
    , catMaybes
    , mapMaybe

    -- ** Trimming
    , take
    -- , takeInterval
    , takeEndBy_
    , takeEndBy

    -- ** Serial Append
    , splitWith

    -- ** Parallel Distribution
    -- | For applicative composition using distribution see
    -- "Streamly.Internal.Data.Fold.Tee".

    , teeWith
    --, teeWithFst
    --, teeWithMin
    , tee
    , distribute

    -- ** Partitioning
    -- | Direct items in the input stream to different folds using a binary
    -- fold selector.

    , partition
    --, partitionByM
    --, partitionByFstM
    --, partitionByMinM
    --, partitionBy

    -- ** Unzipping
    , unzip

    -- ** Splitting
    , many
    , chunksOf
    -- , intervalsOf

    -- ** Nesting
    , concatMap

    -- * Deprecated
    , foldr
    , drainBy
    , last
    , head
    , sequence
    , mapM
    , variance
    , stdDev
    , serialWith
    )
where

import Prelude
       hiding (filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, map, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, replicate, concatMap, mconcat, foldMap, unzip,
               span, splitAt, break, mapM, maybe)

import Streamly.Internal.Data.Fold

-- $setup
-- >>> import Data.Function ((&))
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Stream as Stream
