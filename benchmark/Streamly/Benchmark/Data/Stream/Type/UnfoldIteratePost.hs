-- |
-- Module      : Stream.Type.UnfoldIteratePost
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

-- | Benchmarks for the depth first post order unfoldIterate traversals, on
-- the same trees as "Stream.Type.UnfoldIterate". They are in a separate
-- module to keep the GHC heap needed to compile a module below 500 MB.
module Stream.Type.UnfoldIteratePost
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

{-# INLINE unfoldIteratePost #-}
unfoldIteratePost :: Int -> Int -> IO Int
unfoldIteratePost k limit =
    sumTree
        $ Stream.unfoldIteratePost
            (childrenSeed limit) (childrenUnfold k) (Stream.fromPure 0)

{-# INLINE unfoldIterateBatchPost #-}
unfoldIterateBatchPost :: Int -> Int -> IO Int
unfoldIterateBatchPost k limit =
    sumTree
        $ Stream.unfoldIterateBatchPost
            (childrenSeed limit) (childrenUnfold k) (Stream.fromPure 0)

{-# INLINE unfoldIterateBatchPostRev #-}
unfoldIterateBatchPostRev :: Int -> Int -> IO Int
unfoldIterateBatchPostRev k limit =
    sumTree
        $ Stream.unfoldIterateBatchPostRev
            (childrenSeed limit) (childrenUnfold k) (Stream.fromPure 0)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

-- The memory held is proportional to the depth times the number of children
-- per node.
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    concatMap shape [("binary", 2), ("10-ary", 10), ("1000-ary", 1000)]

    where

    shape (name, k) =
        let limit = size `div` k
            prefix x = "iterate/" ++ name ++ "/" ++ x
         in [ (SpaceO_1, benchIO (prefix "unfoldIteratePost")
                $ unfoldIteratePost k limit)
            , (SpaceO_1, benchIO (prefix "unfoldIterateBatchPost")
                $ unfoldIterateBatchPost k limit)
            , (SpaceO_1, benchIO (prefix "unfoldIterateBatchPostRev")
                $ unfoldIterateBatchPostRev k limit)
            ]
