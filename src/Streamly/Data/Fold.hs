-- |
-- Module      : Streamly.Data.Fold
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- The 'Fold' type represents an effectful action that absorbs a value into an
-- accumulator. For example, a 'sum' 'Fold' represents adding the input to the
-- accumulated sum.  A fold driver e.g. 'Streamly.Prelude.fold' pushes values
-- from a stream to the 'Fold' one at a time, reducing the stream to a single
-- value. Therefore, a fold can also be thought of as a sink or a stream
-- consumer.
--
-- A 'Fold' is in fact a data representation of a standard left fold ('foldl').
-- Unlike the standard left folds, 'Fold's can terminate at any point e.g. the
-- 'head' fold would terminate immediately after returning the head element.
--
-- 'Fold's can be combined efficiently using combinators provided in this
-- module; a stream supplied to the combined fold is provided to the
-- constituent folds according to the behavior of the combinator.  For example,
-- the 'tee' combinator distributes the input stream to two folds and then
-- combines the resulting fold outputs.  Similarly, a 'partition' combinator
-- divides the input among constituent folds.
--
-- = Accumulators and Terminating Folds
--
-- Folds in this module can be classified in two categories viz. accumulators
-- and terminating folds. Accumulators do not have a terminating condition,
-- they run forever and consume the entire stream, for example the 'length'
-- fold. Terminating folds have a terminating condition and can terminate
-- without consuming the entire stream, for example, the 'head' fold.
--
-- = Monoids
--
-- Monoids allow generalized, modular folding.  The accumulators in this module
-- can be expressed using 'mconcat' and a suitable 'Monoid'.  Instead of
-- writing folds we can write Monoids and turn them into folds.
--
-- = Performance Notes
--
-- 'Streamly.Prelude' module provides fold functions to directly fold streams
-- e.g.  Streamly.Prelude/'Streamly.Prelude.sum' serves the same purpose as
-- Fold/'sum'.  However, the functions in Streamly.Prelude cannot be
-- efficiently combined together e.g. we cannot drive the input stream through
-- @sum@ and @length@ fold functions simultaneously.  Using the 'Fold' type we
-- can efficiently split the stream across mutliple folds because it allows the
-- compiler to perform stream fusion optimizations.
--
-- = Programmer Notes
--
-- > import qualified Streamly.Data.Fold as Fold
--
-- More, not yet exposed, fold combinators can be found in
-- "Streamly.Internal.Data.Fold".

module Streamly.Data.Fold
    (
    -- * Fold Type
    -- |
    -- A 'Fold' can be run over a stream using the 'Streamly.Prelude.fold'
    -- combinator:
    --
    -- >>> Stream.fold Fold.sum (Stream.enumerateFromTo 1 100)
    -- 5050

      Fold -- (..)

    -- * Accumulators
    -- ** Monoids
    , mconcat
    , foldMap
    , foldMapM

    -- ** Reducers
    , drain
    , drainBy
    , last
    , length
    , sum
    , product
    , maximumBy
    , maximum
    , minimumBy
    , minimum
    , mean
    , variance
    , stdDev

    -- ** Collectors
    -- | Avoid using these folds in scalable or performance critical
    -- applications, they buffer all the input in GC memory which can be
    -- detrimental to performance if the input is large.

    , toList

    -- * Terminating Folds
    -- , drainN
    -- , drainWhile
    -- , lastN
    -- , (!!)
    -- , genericIndex
    , index
    , head
    -- , findM
    , find
    , lookup
    , findIndex
    , elemIndex
    , null
    , elem
    , notElem
    , all
    , any
    , and
    , or

    -- * Output Transformations
    -- | Unlike stream producer types (e.g. @SerialT m a@) which have only
    -- output side, folds have an input side as well as an output side.  In the
    -- type @Fold m a b@, the input type is @a@ and the output type is @b@.
    -- Transformations can be applied either on the input side or on the output
    -- side. The 'Functor' instance of a fold maps on the output of the fold:
    --
    -- >>> Stream.fold (fmap show Fold.sum) (Stream.enumerateFromTo 1 100)
    -- "5050"
    --
    -- Note: Output transformations are also known as covariant
    -- transformations.
    , sequence
    , rmapM
    , mapM

    {-
    -- * Input Transformations
    -- The input side transformations are more interesting for folds.  The
    -- following sections describe the input transformation operations on a
    -- fold.  The names and signatures of the operations are consistent with
    -- corresponding operations in "Streamly.Prelude".
    --
    -- Note: Input transformations are also known as contravariant
    -- transformations.

    -- ** Mapping
    --, transform
    -- , lmap
    --, lsequence
    -- , lmapM

    -- -- ** Filtering
    -- , filter
    -- , filterM
    -- , ldeleteBy
    -- , luniq

    -- ** Mapping Filters
    , lmapMaybe
    , lmapMaybeM

    -- ** Scanning Filters
    , lfindIndices
    , lelemIndices

    -- ** Insertion
    -- | Insertion adds more elements to the stream.

    , linsertBy
    , lintersperseM

    -- ** Reordering
    , lreverse
    -}

    {-
    -- ** Trimming
    , take
    -- takeByTime
    , ldrop
    , ldropWhile
    , ldropWhileM
    -}

    -- * Distributing
    -- |
    -- The 'Applicative' instance of a distributing 'Fold' distributes one copy
    -- of the stream to each fold and combines the results using a function.
    --
    -- @
    --
    --                 |-------Fold m a b--------|
    -- ---stream m a---|                         |---m (b,c,...)
    --                 |-------Fold m a c--------|
    --                 |                         |
    --                            ...
    -- @
    --
    -- To compute the average of numbers in a stream without going through the
    -- stream twice:
    --
    -- >>> let avg = Fold.teeWith (/) Fold.sum (fmap fromIntegral Fold.length)
    -- >>> Stream.fold avg (Stream.enumerateFromTo 1.0 100.0)
    -- 50.5
    --
    -- The 'Semigroup' and 'Monoid' instances of a distributing fold distribute
    -- the input to both the folds and combines the outputs using Monoid or
    -- Semigroup instances of the output types:
    --
    -- >>> import Data.Monoid (Sum(..))
    -- >>> Stream.fold (Fold.teeWith (<>) Fold.head Fold.last) (fmap Sum $ Stream.enumerateFromTo 1.0 100.0)
    -- Just (Sum {getSum = 101.0})
    --
    -- The 'Num', 'Floating', and 'Fractional' instances work in the same way.

    , tee
    , teeWith
    , distribute

    -- * Partitioning
    -- |
    -- Direct items in the input stream to different folds using a binary
    -- fold selector.

    -- , partitionByM
    -- , partitionBy
    , partition

    {-
    -- * Demultiplexing
    -- | Direct values in the input stream to different folds using an n-ary
    -- fold selector.

    , demux
    -- , demuxWith
    , demux_
    -- , demuxWith_

    -- * Classifying
    -- | In an input stream of key value pairs fold values for different keys
    -- in individual output buckets using the given fold.

    , classify
    -- , classifyWith
    -}

    -- * Unzipping
    , unzip
    -- These can be expressed using lmap/lmapM and unzip
    -- , unzipWith
    -- , unzipWithM

    -- -- * Nesting
    -- , concatMap
    -- , chunksOf
    -- , duplicate  -- experimental
    )
where

import Prelude
       hiding (filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, map, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, replicate, concatMap, mconcat, foldMap, unzip,
               span, splitAt, break, mapM)

import Streamly.Internal.Data.Fold

--
-- $setup
-- >>> :m
-- >>> import Prelude hiding (head, sum, last, length)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
