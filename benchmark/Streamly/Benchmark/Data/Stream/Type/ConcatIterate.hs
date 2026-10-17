-- |
-- Module      : Stream.Type.ConcatIterate
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

-- | Benchmarks for the concatIterate family of tree traversals, on a tree held
-- in memory. The work per element is an addition, so the cost of the
-- traversal itself dominates.
module Stream.Type.ConcatIterate
    ( benchmarks
    ) where

import Streamly.Internal.Data.Stream (Stream)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Stream.Type.Basic (benchIO)
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Tree
-------------------------------------------------------------------------------

-- A complete k-ary tree with nodes numbered in level order from 0, node n has
-- the children k * n + 1 to k * n + k if n < limit. The tree has
-- k * limit + 1 nodes.

{-# INLINE childrenStream #-}
childrenStream :: Int -> Int -> Int -> Maybe (Stream IO Int)
childrenStream k limit n
    | n < limit = Just (Stream.enumerateFromTo (k * n + 1) (k * n + k))
    | otherwise = Nothing

-------------------------------------------------------------------------------
-- Traversals
-------------------------------------------------------------------------------

{-# INLINE sumTree #-}
sumTree :: Stream IO Int -> IO Int
sumTree = Stream.fold Fold.sum

{-# INLINE concatIterate #-}
concatIterate :: Int -> Int -> IO Int
concatIterate k limit =
    sumTree
        $ Stream.concatIterate (childrenStream k limit) (Stream.fromPure 0)

{-# INLINE wideDfsConcatIterate #-}
wideDfsConcatIterate :: Int -> Int -> IO Int
wideDfsConcatIterate k limit =
    sumTree
        $ Stream.wideDfsConcatIterate
            (childrenStream k limit) (Stream.fromPure 0)

{-# INLINE wideDfsConcatIterateRev #-}
wideDfsConcatIterateRev :: Int -> Int -> IO Int
wideDfsConcatIterateRev k limit =
    sumTree
        $ Stream.wideDfsConcatIterateRev
            (childrenStream k limit) (Stream.fromPure 0)

{-# INLINE bfsConcatIterate #-}
bfsConcatIterate :: Int -> Int -> IO Int
bfsConcatIterate k limit =
    sumTree
        $ Stream.bfsConcatIterate (childrenStream k limit) (Stream.fromPure 0)

{-# INLINE bfsConcatIterateRev #-}
bfsConcatIterateRev :: Int -> Int -> IO Int
bfsConcatIterateRev k limit =
    sumTree
        $ Stream.bfsConcatIterateRev
            (childrenStream k limit) (Stream.fromPure 0)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

-- The BFS traversals hold a level of the tree in memory, the others hold
-- memory proportional to the depth times the number of children per node.
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    concatMap shape [("binary", 2), ("10-ary", 10), ("1000-ary", 1000)]

    where

    shape (name, k) =
        let limit = size `div` k
            prefix x = "iterate/" ++ name ++ "/" ++ x
         in [ (SpaceO_1, benchIO (prefix "concatIterate")
                $ concatIterate k limit)
            , (SpaceO_1, benchIO (prefix "wideDfsConcatIterate")
                $ wideDfsConcatIterate k limit)
            , (SpaceO_1, benchIO (prefix "wideDfsConcatIterateRev")
                $ wideDfsConcatIterateRev k limit)
            , (SpaceO_n, benchIO (prefix "bfsConcatIterate")
                $ bfsConcatIterate k limit)
            , (SpaceO_n, benchIO (prefix "bfsConcatIterateRev")
                $ bfsConcatIterateRev k limit)
            ]
