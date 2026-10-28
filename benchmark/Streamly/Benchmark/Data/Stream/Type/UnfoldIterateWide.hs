-- |
-- Module      : Stream.Type.UnfoldIterateWide
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

-- | Benchmarks for the breadth first unfoldIterate traversals named with the
-- wide prefix, on the same trees as "Stream.Type.UnfoldIterate". They are in
-- a separate module to keep the GHC heap needed to compile a module below
-- 500 MB.
module Stream.Type.UnfoldIterateWide
    ( benchmarks
    ) where

import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Stream.Type.Basic (benchIO)
import Stream.Type.UnfoldIterate (childrenSeed, childrenUnfold, sumTree)
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Traversals
-------------------------------------------------------------------------------

{-# INLINE wideUnfoldIterate #-}
wideUnfoldIterate :: Int -> Int -> IO Int
wideUnfoldIterate k limit =
    sumTree
        $ Stream.wideUnfoldIterate
            (childrenSeed limit) (childrenUnfold k) (Stream.fromPure 0)

{-# INLINE wideUnfoldIteratePost #-}
wideUnfoldIteratePost :: Int -> Int -> IO Int
wideUnfoldIteratePost k limit =
    sumTree
        $ Stream.wideUnfoldIteratePost
            (childrenSeed limit) (childrenUnfold k) (Stream.fromPure 0)

{-# INLINE wideUnfoldIterateBatchPost #-}
wideUnfoldIterateBatchPost :: Int -> Int -> IO Int
wideUnfoldIterateBatchPost k limit =
    sumTree
        $ Stream.wideUnfoldIterateBatchPost
            (childrenSeed limit) (childrenUnfold k) (Stream.fromPure 0)

{-# INLINE wideUnfoldIterateBatchPostRev #-}
wideUnfoldIterateBatchPostRev :: Int -> Int -> IO Int
wideUnfoldIterateBatchPostRev k limit =
    sumTree
        $ Stream.wideUnfoldIterateBatchPostRev
            (childrenSeed limit) (childrenUnfold k) (Stream.fromPure 0)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

-- wideUnfoldIterate and wideUnfoldIteratePost hold a stream or a seed for
-- each node of the widest level, the batched post order holds every non-leaf
-- node.
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    concatMap shape [("binary", 2), ("10-ary", 10), ("1000-ary", 1000)]

    where

    shape (name, k) =
        let limit = size `div` k
            prefix x = "iterate/" ++ name ++ "/" ++ x
         in [ (SpaceO_n, benchIO (prefix "wideUnfoldIterate")
                $ wideUnfoldIterate k limit)
            , (SpaceO_n, benchIO (prefix "wideUnfoldIteratePost")
                $ wideUnfoldIteratePost k limit)
            , (SpaceO_n, benchIO (prefix "wideUnfoldIterateBatchPost")
                $ wideUnfoldIterateBatchPost k limit)
            , (SpaceO_n, benchIO (prefix "wideUnfoldIterateBatchPostRev")
                $ wideUnfoldIterateBatchPostRev k limit)
            ]
