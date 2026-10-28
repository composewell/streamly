-- |
-- Module      : Stream.Type.UnfoldIterate
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

-- | Benchmarks for the unfoldIterate family of tree traversals, on the same
-- trees as "Stream.Type.ConcatIterate". They are in a separate module because
-- compiling both families in one module takes more than 500 MB of GHC heap.
module Stream.Type.UnfoldIterate
    ( benchmarks
    , childrenSeed
    , childrenUnfold
    , sumTree
    ) where

import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Unfold (Unfold)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold

import Test.Tasty.Bench
import Stream.Type.Basic (benchIO)
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Tree
-------------------------------------------------------------------------------

-- A complete k-ary tree with nodes numbered in level order from 0, node n has
-- the children k * n + 1 to k * n + k if n < limit. The tree has
-- k * limit + 1 nodes.

{-# INLINE childrenSeed #-}
childrenSeed :: Int -> Int -> Maybe Int
childrenSeed limit n
    | n < limit = Just n
    | otherwise = Nothing

{-# INLINE childrenUnfold #-}
childrenUnfold :: Int -> Unfold IO Int Int
childrenUnfold k = Unfold.lmap range Unfold.enumerateFromToNum

    where

    range n = (k * n + 1, k * n + k)

-------------------------------------------------------------------------------
-- Traversals
-------------------------------------------------------------------------------

{-# INLINE sumTree #-}
sumTree :: Stream IO Int -> IO Int
sumTree = Stream.fold Fold.sum

{-# INLINE unfoldIterateMaybe #-}
unfoldIterateMaybe :: Int -> Int -> IO Int
unfoldIterateMaybe k limit =
    sumTree
        $ Stream.unfoldIterateMaybe
            (childrenSeed limit) (childrenUnfold k) (Stream.fromPure 0)

{-# INLINE wideDfsUnfoldIterate #-}
wideDfsUnfoldIterate :: Int -> Int -> IO Int
wideDfsUnfoldIterate k limit =
    sumTree
        $ Stream.wideDfsUnfoldIterate
            (childrenSeed limit) (childrenUnfold k) (Stream.fromPure 0)

{-# INLINE wideDfsUnfoldIterateRev #-}
wideDfsUnfoldIterateRev :: Int -> Int -> IO Int
wideDfsUnfoldIterateRev k limit =
    sumTree
        $ Stream.wideDfsUnfoldIterateRev
            (childrenSeed limit) (childrenUnfold k) (Stream.fromPure 0)

{-# INLINE bfsUnfoldIterateMaybe #-}
bfsUnfoldIterateMaybe :: Int -> Int -> IO Int
bfsUnfoldIterateMaybe k limit =
    sumTree
        $ Stream.bfsUnfoldIterateMaybe
            (childrenSeed limit) (childrenUnfold k) (Stream.fromPure 0)

{-# INLINE bfsUnfoldIterateRev #-}
bfsUnfoldIterateRev :: Int -> Int -> IO Int
bfsUnfoldIterateRev k limit =
    sumTree
        $ Stream.bfsUnfoldIterateRev
            (childrenSeed limit) (childrenUnfold k) (Stream.fromPure 0)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

-- The names use the same prefix as "Stream.Type.ConcatIterate" so that the
-- reports list the two families together. The BFS traversals hold a level of
-- the tree in memory, the others hold memory proportional to the depth times
-- the number of children per node.
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    concatMap shape [("binary", 2), ("10-ary", 10), ("1000-ary", 1000)]

    where

    shape (name, k) =
        let limit = size `div` k
            prefix x = "iterate/" ++ name ++ "/" ++ x
         in [ (SpaceO_1, benchIO (prefix "unfoldIterateMaybe")
                $ unfoldIterateMaybe k limit)
            , (SpaceO_1, benchIO (prefix "wideDfsUnfoldIterate")
                $ wideDfsUnfoldIterate k limit)
            , (SpaceO_1, benchIO (prefix "wideDfsUnfoldIterateRev")
                $ wideDfsUnfoldIterateRev k limit)
            , (SpaceO_n, benchIO (prefix "bfsUnfoldIterateMaybe")
                $ bfsUnfoldIterateMaybe k limit)
            , (SpaceO_n, benchIO (prefix "bfsUnfoldIterateRev")
                $ bfsUnfoldIterateRev k limit)
            ]
