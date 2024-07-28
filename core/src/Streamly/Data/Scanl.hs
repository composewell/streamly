{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Data.Scanl
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--

module Streamly.Data.Scanl
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Fold Type

      Scanl -- (..)

    -- * Constructors
    , scanl'
    , scanlM'
    , scanl1'
    , scanl1M'
    , scanr'

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
    -- , drainMapM
    , length
    , countDistinct
    , countDistinctInt
    -- , frequency
    -- , sum
    , product
    , mean
    , rollingHash
    , rollingHashWithSalt

    -- Collectors
    , toList
    , toListRev
    , toSet
    , toIntSet
    , topBy

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
    , findIndices
    , elemIndices
    , deleteBy
    -- , uniq
    , uniqBy
    , nub
    , nubInt

    -- ** Terminating Folds
    -- , satisfy
    -- , maybe

    , the

    -- * Transformations
    -- | Transformations are modifiers of folds.  In the type @Fold m a b@, @a@
    -- is the input type and @b@ is the output type.  Transformations can be
    -- applied either on the input side (contravariant) or on the output side
    -- (covariant).  Therefore, transformations have one of the following
    -- general shapes:
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

    -- ** Filtering
    , filter
    , filterM

    -- -- ** Mapping Filters
    , mapMaybe
    , catMaybes
    , catLefts
    , catRights
    , catEithers

    -- ** Trimming
    , take
    , takeEndBy
    , takeEndBy_

    {-
    -- ** Key-value Collectors
    , toMap
    , toMapIO
    -}

    -- ** Key-value Scanners
    , classify
    , classifyIO

    -- ** Transforming the Monad
    , morphInner

    -- * Combinators
    -- | Transformations that combine two or more folds.

    -- ** Scanning
    , scan
    , postscan
    , scanMaybe

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

    -- * Dynamic Combinators
    -- | The fold to be used is generated dynamically based on the input or
    -- based on the output of the previous fold.

    {-
    -- ** Key-value Collectors
    , demuxToMap
    , demuxToMapIO
    -}

    -- ** Key-value Scanners
    , demux
    , demuxIO
    )
where

import Prelude
       hiding (Foldable(..), filter, drop, dropWhile, take, takeWhile, zipWith,
               map, mapM_, sequence, all, any,
               notElem, head, last, tail,
               reverse, iterate, init, and, or, lookup, (!!),
               scanl, scanl1, replicate, concatMap, mconcat, unzip,
               span, splitAt, break, mapM, maybe)

import Streamly.Internal.Data.Scanl

#include "DocTestDataFold.hs"
