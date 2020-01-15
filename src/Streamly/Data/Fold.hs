-- |
-- Module      : Streamly.Data.Fold
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- 'Fold' type represents an effectful action that consumes a value from an
-- input stream and combines it with a single final value often called an
-- accumulator, returning the resulting output accumulator.  Values from a
-- stream can be /pushed/ to the fold and consumed one at a time. It can also
-- be called a consumer of stream or a sink.  It is a data representation of
-- the standard 'Streamly.Prelude.foldl'' function.  A 'Fold' can be turned
-- into an effect (@m b@) using 'Streamly.Prelude.fold' by supplying it the
-- input stream.
--
-- Using this representation multiple folds can be combined efficiently using
-- combinators; a stream can then be supplied to the combined fold and it would
-- distribute the input to constituent folds according to the composition.  For
-- example, an applicative composition distributes the same input to the
-- constituent folds and then combines the resulting fold outputs.  Similarly,
-- a partitioning combinator divides the input among constituent folds.
--
-- = Performance Notes
--
-- 'Fold' representation is more efficient than using streams when splitting
-- streams.  @Fold m a b@ can be considered roughly equivalent to a fold action
-- @m b -> t m a -> m b@ (where @t@ is a stream type and @m@ is a 'Monad').
-- Instead of using a 'Fold' type one could just use a fold action of the shape
-- @m b -> t m a -> m b@ for folding streams. However, multiple such actions
-- cannot be composed into a single fold function in an efficient manner.
-- Using the 'Fold' type we can efficiently split the stream across mutliple
-- folds because it allows the compiler to perform stream fusion optimizations.
--
-- On the other hand, transformation operations (e.g. 'Streamly.Prelude.map')
-- on stream types can be as efficient as transformations on 'Fold' (e.g.
-- 'Streamly.Internal.Data.Fold.lmap').
--
-- = Left folds vs Right Folds
--
-- The folds in this module are left folds, therefore, even partial folds, e.g.
-- @head@ in this module, would drain the whole stream. On the other hand, the
-- partial folds in "Streamly.Prelude" module are lazy right folds and would
-- terminate as soon as the result is determined. However, the folds in this
-- module can be composed but the folds in "Streamly.Prelude" cannot be
-- composed.
--
-- = Programmer Notes
--
-- > import qualified Streamly.Data.Fold as FL
--
-- More, not yet exposed, fold combinators can be found in
-- "Streamly.Internal.Data.Fold".

-- IMPORTANT: keep the signatures consistent with the folds in Streamly.Prelude

module Streamly.Data.Fold
    (
    -- * Fold Type
    -- |
    -- A 'Fold' can be run over a stream using the 'Streamly.Prelude.fold'
    -- combinator:
    --
    -- >>> S.fold FL.sum (S.enumerateFromTo 1 100)
    -- 5050

      Fold -- (..)

    -- , tail
    -- , init

    -- ** Full Folds
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
    -- , the
    , mean
    , variance
    , stdDev

    -- ** Full Folds (Monoidal)
    , mconcat
    , foldMap
    , foldMapM

    -- ** Full Folds (To Containers)
    -- | Avoid using these folds in scalable or performance critical
    -- applications, they buffer all the input in GC memory which can be
    -- detrimental to performance if the input is large.

    , toList

    -- ** Partial Folds
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
    -- XXX these are slower than right folds even when full input is used
    , all
    , any
    , and
    , or

    -- * Transformations
    -- | Unlike stream producer types (e.g. @SerialT m a@) which have only
    -- output side, folds have an input side as well as an output side.  In the
    -- type @Fold m a b@, the input type is @a@ and the output type is @b@.
    -- Transformations can be applied either on the input side or on the output
    -- side. The 'Functor' instance of a fold maps on the output of the fold:
    --
    -- >>> S.fold (fmap show FL.sum) (S.enumerateFromTo 1 100)
    -- "5050"
    --
    -- However, the input side or contravariant transformations are more
    -- interesting for folds.  The following sections describe the input
    -- transformation operations on a fold.  The names of the operations are
    -- consistent with their covariant counterparts in "Streamly.Prelude", the
    -- only difference is that they are prefixed with 'l' which stands for
    -- 'left' assuming left side is the input side, notice that in @Fold m a b@
    -- the type variable @a@ is on the left side.

    -- ** Covariant Operations
    , sequence
    , mapM

    {-
    -- ** Mapping
    --, transform
    -- , lmap
    --, lsequence
    -- , lmapM

    -- -- ** Filtering
    -- , lfilter
    -- , lfilterM
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
    -- * Parsing
    -- ** Trimming
    , ltake
    -- , lrunFor -- time
    , ltakeWhile
    , ltakeWhileM
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
    -- >>> let avg = (/) <$> FL.sum <*> fmap fromIntegral FL.length
    -- >>> S.fold avg (S.enumerateFromTo 1.0 100.0)
    -- 50.5
    --
    -- The 'Semigroup' and 'Monoid' instances of a distributing fold distribute
    -- the input to both the folds and combines the outputs using Monoid or
    -- Semigroup instances of the output types:
    --
    -- >>> import Data.Monoid (Sum)
    -- >>> S.fold (FL.head <> FL.last) (fmap Sum $ S.enumerateFromTo 1.0 100.0)
    -- Just (Sum {getSum = 101.0})
    --
    -- The 'Num', 'Floating', and 'Fractional' instances work in the same way.

    , tee
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

    -- -- * Nested Folds
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
