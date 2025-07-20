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

    -- * Scanl Type

      Scanl -- (..)

    -- * Constructors
    , mkScanl
    , mkScanlM
    , mkScanl1
    , mkScanl1M
    , mkScanr

    -- * Scans
    -- ** Accumulators
    -- | Scans that never terminate, these scans are much like strict left
    -- folds. 'mconcat' is the fundamental accumulator.  All other accumulators
    -- can be expressed in terms of 'mconcat' using a suitable Monoid.  Instead
    -- of writing scans we could write Monoids and turn them into scans.

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
    , sum
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
    -- | Accumulators that are usually run as a scan using the 'potscanlMaybe'
    -- combinator.
    , findIndices
    , elemIndices
    , deleteBy
    -- , uniq
    , uniqBy
    , nub
    , nubInt

    -- ** Terminating Scans
    -- , satisfy
    -- , maybe

    , the

    -- * Transformations
    -- | Transformations are modifiers of scans.  In the type @Scan m a b@, @a@
    -- is the input type and @b@ is the output type.  Transformations can be
    -- applied either on the input side (contravariant) or on the output side
    -- (covariant).  Therefore, transformations have one of the following
    -- general shapes:
    --
    -- * @... -> Scanl m a b -> Scanl m c b@ (input transformation)
    -- * @... -> Scanl m a b -> Scanl m a c@ (output transformation)
    --
    -- The input side transformations are more interesting for scans.  Most of
    -- the following sections describe the input transformation operations on a
    -- scan. When an operation makes sense on both input and output side we use
    -- the prefix @l@ (for left) for input side operations and the prefix @r@
    -- (for right) for output side operations.

    -- ** Mapping on output
    -- | The 'Functor' instance of a scan maps on the output of the scan:
    --
    -- >>> Stream.toList $ Stream.scanl (fmap show Scanl.sum) (Stream.enumerateFromTo 1 10)
    -- ["0","1","3","6","10","15","21","28","36","45","55"]
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

    -- ** Key-value Scanners
    , classify
    , classifyIO

    -- ** Transforming the Monad
    , morphInner

    -- * Combinators
    -- | Transformations that combine two or more scans.

    -- ** Scanning
    , scanl
    , postscanl
    , postscanlMaybe

    -- ** Parallel Distribution
    -- | The 'Applicative' instance distributes the input to both scans.

    , teeWith
    --, teeWithFst
    --, teeWithMin
    , tee
    , distribute

    -- ** Partitioning
    -- | Direct items in the input stream to different scans using a binary
    -- scan selector.

    , partition
    --, partitionByM
    --, partitionByFstM
    --, partitionByMinM
    --, partitionBy

    -- ** Unzipping
    , unzip

    -- * Dynamic Combinators
    -- | The scan to be used is generated dynamically based on the input.

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

#include "DocTestDataScanl.hs"
