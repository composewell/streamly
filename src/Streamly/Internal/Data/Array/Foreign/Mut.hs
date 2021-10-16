#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Array.Foreign.Mut
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Unboxed pinned mutable array type for 'Storable' types with an option to use
-- foreign (non-GHC) memory allocators. Fulfils the following goals:
--
-- * Random access (array)
-- * Efficient storage (unboxed)
-- * Performance (unboxed access)
-- * Performance - in-place operations (mutable)
-- * Performance - GC (pinned, mutable)
-- * interfacing with OS (pinned)
-- * Fragmentation control (foreign allocators)
--
-- Stream and Fold APIs allow easy, efficient and convenient operations on
-- arrays.

module Streamly.Internal.Data.Array.Foreign.Mut
    (
    -- * Type
      Array
    -- , writeIndex
    -- , writeFrom -- start writing at the given position
    -- , writeFromRev
    -- , writeTo   -- write from beginning up to the given position
    -- , writeToRev
    -- , writeFromTo
    -- , writeFromThenTo
    --
    -- , writeChunksOfFrom
    -- , ...

    -- * Mutation
    , writeIndex
    , snoc

    --, writeIndices
    --, writeRanges

    -- -- * In-pace mutation (for Mutable Array type)
    -- , partitionBy
    -- , shuffleBy
    -- , foldtWith
    -- , foldbWith

    -- * Folding
    , foldl'
    , foldr

    -- * Composable Folds
    , writeN
    , write

    -- * Unfolds
    , read

    -- * To containers
    , toList

    -- * Combining
    , splice
    , spliceExp
    , spliceCopy

    -- * Splitting
    , breakOn
    , splitAt
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Storable (Storable(..))
import Streamly.Internal.BaseCompat (unsafeWithForeignPtr)

import Prelude hiding (foldr, length, read, splitAt)

import Streamly.Internal.Data.Array.Foreign.Mut.Type

-- | /O(1)/ Write the given element at the given index in the array.
-- Performs in-place mutation of the array.
--
-- /Pre-release/
{-# INLINE writeIndex #-}
writeIndex :: (MonadIO m, Storable a) => Array a -> Int -> a -> m ()
writeIndex arr i a = do
    let maxIndex = length arr - 1
    if i < 0
    then error "writeIndex: negative array index"
    else if i > maxIndex
         then error $ "writeIndex: specified array index " ++ show i
                    ++ " is beyond the maximum index " ++ show maxIndex
         else
            liftIO $ unsafeWithForeignPtr (aStart arr) $ \p ->
                pokeElemOff p i a

{-

-------------------------------------------------------------------------------
-- In-place mutation APIs
-------------------------------------------------------------------------------

-- | Partition an array into two halves using a partitioning predicate. The
-- first half retains values where the predicate is 'False' and the second half
-- retains values where the predicate is 'True'.
{-# INLINE partitionBy #-}
partitionBy :: (a -> Bool) -> Array a -> (Array a, Array a)
partitionBy f arr = undefined

-- | Shuffle corresponding elements from two arrays using a shuffle function.
-- If the shuffle function returns 'False' then do nothing otherwise swap the
-- elements. This can be used in a bottom up fold to shuffle or reorder the
-- elements.
shuffleBy :: (a -> a -> m Bool) -> Array a -> Array a -> m (Array a)
shuffleBy f arr1 arr2 = undefined

-- XXX we can also make the folds partial by stopping at a certain level.
--
-- | Perform a top down hierarchical recursive partitioning fold of items in
-- the container using the given function as the partition function.
--
-- This will perform a quick sort if the partition function is
-- 'partitionBy (< pivot)'.
--
-- @since 0.7.0
{-# INLINABLE foldtWith #-}
foldtWith :: Int -> (Array a -> Array a -> m (Array a)) -> Array a -> m (Array a)
foldtWith level f = undefined

-- | Perform a pairwise bottom up fold recursively merging the pairs. Level
-- indicates the level in the tree where the fold would stop.
--
-- This will perform a random shuffle if the shuffle function is random.
-- If we stop at level 0 and repeatedly apply the function then we can do a
-- bubble sort.
foldbWith :: Int -> (Array a -> Array a -> m (Array a)) -> Array a -> m (Array a)
foldbWith level f = undefined
-}

-- XXX consider the bulk update/accumulation/permutation APIs from vector.
