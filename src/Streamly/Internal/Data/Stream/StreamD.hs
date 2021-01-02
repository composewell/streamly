#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD
-- Copyright   : (c) 2018 Composewell Technologies
--               (c) Roman Leshchinskiy 2008-2010
--               (c) The University of Glasgow, 2009
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Direct style re-implementation of CPS style stream in StreamK module.  The
-- symbol or suffix 'D' in this module denotes the "Direct" style.  GHC is able
-- to INLINE and fuse direct style better, providing better performance than
-- CPS implementation.
--
-- @
-- import qualified Streamly.Internal.Data.Stream.StreamD as D
-- @

-- Some of the functions in this file have been adapted from the vector
-- library,  https://hackage.haskell.org/package/vector.

module Streamly.Internal.Data.Stream.StreamD
    (
    -- * The stream type
      Step (..)
    , Stream (Stream, UnStream)

    -- * Construction
    , nil
    , nilM
    , cons

    -- * Deconstruction
    , uncons

    -- * Generation
    -- ** Unfolds
    , unfoldr
    , unfoldrM
    , unfold

    -- ** Specialized Generation
    -- | Generate a monadic stream from a seed.
    , repeat
    , repeatM
    , replicate
    , replicateM
    , fromIndices
    , fromIndicesM
    , generate
    , generateM
    , iterate
    , iterateM

    -- ** Enumerations
    , enumerateFromStepIntegral
    , enumerateFromIntegral
    , enumerateFromThenIntegral
    , enumerateFromToIntegral
    , enumerateFromThenToIntegral

    , enumerateFromStepNum
    , numFrom
    , numFromThen
    , enumerateFromToFractional
    , enumerateFromThenToFractional

    -- ** Time
    , times

    -- ** Conversions
    -- | Transform an input structure into a stream.
    -- | Direct style stream does not support @fromFoldable@.
    , yield
    , yieldM
    , fromList
    , fromListM
    , fromStreamK
    , fromStreamD
    , fromPrimIORef
    , fromSVar

    -- * Elimination
    -- ** General Folds
    , foldrS
    , foldrT
    , foldrM
    , foldrMx
    , foldr
    , foldr1

    , foldl'
    , foldlM'
    , foldlS
    , foldlT
    , reverse
    , reverse'

    , foldlx'
    , foldlMx'
    , foldOnce
    , foldMany
    , foldMany1

    , parselMx'
    , parseMany
    , parseIterate

    -- ** Specialized Folds
    , tap
    , tapOffsetEvery
    , tapAsync
    , tapRate
    , pollCounts
    , drain
    , null
    , head
    , headElse
    , tail
    , last
    , elem
    , notElem
    , all
    , any
    , maximum
    , maximumBy
    , minimum
    , minimumBy
    , findIndices
    , lookup
    , findM
    , find
    , (!!)
    , toSVarParallel

    -- ** Flattening nested streams
    , concatMapM
    , concatMap
    , ConcatMapUState (..)
    , concatMapU
    , ConcatUnfoldInterleaveState (..)
    , concatUnfoldInterleave
    , concatUnfoldRoundrobin
    , AppendState(..)
    , append
    , InterleaveState(..)
    , interleave
    , interleaveMin
    , interleaveSuffix
    , interleaveInfix
    , roundRobin -- interleaveFair?/ParallelFair
    , gintercalateSuffix
    , interposeSuffix
    , gintercalate
    , interpose

    -- ** Grouping
    , groupsOf2
    , groupsBy
    , groupsRollingBy

    -- ** Splitting
    , wordsBy

    , splitOnSeq
    , splitOnSuffixSeq

    , splitInnerBy
    , splitInnerBySuffix

    -- ** Substreams
    , isPrefixOf
    , isSubsequenceOf
    , stripPrefix

    -- ** Map and Fold
    , mapM_

    -- ** Conversions
    -- | Transform a stream into another type.
    , toList
    , toListRev
    , toStreamK
    , toStreamD

    , hoist
    , generally

    , liftInner
    , runReaderT
    , evalStateT
    , runStateT

    -- * Transformation
    , transform

    -- ** By folding (scans)
    , scanlM'
    , scanlMAfter'
    , scanl'
    , scanlM
    , scanl
    , scanl1M'
    , scanl1'
    , scanl1M
    , scanl1

    , prescanl'
    , prescanlM'

    , postscanl
    , postscanlM
    , postscanl'
    , postscanlM'
    , postscanlMAfter'

    , postscanlx'
    , postscanlMx'
    , scanlMx'
    , scanlx'
    , postscanOnce
    , scanOnce

    -- * Filtering
    , filter
    , filterM
    , uniq
    , take
    , takeByTime
    , takeWhile
    , takeWhileM
    , drop
    , dropByTime
    , dropWhile
    , dropWhileM

    -- * Mapping
    , map
    , mapM
    , sequence
    , rollingMap
    , rollingMapM

    -- * Inserting
    , intersperseM
    , intersperseM_
    , intersperse
    , intersperseSuffix
    , intersperseSuffix_
    , intersperseSuffixBySpan
    , insertBy

    -- * Deleting
    , deleteBy

    -- ** Map and Filter
    , mapMaybe
    , mapMaybeM

    -- * Zipping
    , indexed
    , indexedR
    , zipWith
    , zipWithM

    -- * Comparisons
    , eqBy
    , cmpBy

    -- * Merging
    , mergeBy
    , mergeByM

    -- * Transformation comprehensions
    , the

    -- * Exceptions
    , newFinalizedIORef
    , runIORefFinalizer
    , withIORefFinalizer
    , gbracket_
    , gbracket
    , before
    , after_
    , after
    , bracket_
    , bracket
    , onException
    , finally_
    , finally
    , ghandle
    , handle

    -- * Concurrent Application
    , mkParallel
    , mkParallelD
    , newCallbackStream
    )
where

import Prelude hiding
       ( map, mapM, mapM_, repeat, foldr, last, take, filter
       , takeWhile, drop, dropWhile, all, any, maximum, minimum, elem
       , notElem, null, head, tail, zipWith, lookup, foldr1, sequence
       , (!!), scanl, scanl1, concatMap, replicate, enumFromTo, concat
       , reverse, iterate, splitAt)
import Streamly.Internal.Data.Stream.StreamD.Type
import Streamly.Internal.Data.Stream.StreamD.Generate
import Streamly.Internal.Data.Stream.StreamD.Eliminate
import Streamly.Internal.Data.Stream.StreamD.Exceptions
import Streamly.Internal.Data.Stream.StreamD.Combine
import Streamly.Internal.Data.Stream.StreamD.SplitGroup
import Streamly.Internal.Data.Stream.StreamD.Transform
