-- |
-- Module      : Stream.Type.ConcatIterateWide
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

-- | Benchmarks for the breadth first concatIterate traversals named with the
-- wide prefix, on the same trees as "Stream.Type.ConcatIterate". They are in
-- a separate module to keep the GHC heap needed to compile a module below
-- 500 MB.
module Stream.Type.ConcatIterateWide
    ( benchmarks
    ) where

import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Stream.Type.Basic (benchIO)
import Stream.Type.ConcatIterate (childrenStream, sumTree)
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Traversals
-------------------------------------------------------------------------------

{-# INLINE wideConcatIterate #-}
wideConcatIterate :: Int -> Int -> IO Int
wideConcatIterate k limit =
    sumTree
        $ Stream.wideConcatIterate (childrenStream k limit) (Stream.fromPure 0)

{-# INLINE wideConcatIteratePost #-}
wideConcatIteratePost :: Int -> Int -> IO Int
wideConcatIteratePost k limit =
    sumTree
        $ Stream.wideConcatIteratePost
            (childrenStream k limit) (Stream.fromPure 0)

{-# INLINE wideConcatIterateBatchPost #-}
wideConcatIterateBatchPost :: Int -> Int -> IO Int
wideConcatIterateBatchPost k limit =
    sumTree
        $ Stream.wideConcatIterateBatchPost
            (childrenStream k limit) (Stream.fromPure 0)

{-# INLINE wideConcatIterateBatchPostRev #-}
wideConcatIterateBatchPostRev :: Int -> Int -> IO Int
wideConcatIterateBatchPostRev k limit =
    sumTree
        $ Stream.wideConcatIterateBatchPostRev
            (childrenStream k limit) (Stream.fromPure 0)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

-- wideConcatIterate and wideConcatIteratePost hold a stream for each node of
-- the widest level, the batched post order holds every non-leaf node.
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    concatMap shape [("binary", 2), ("10-ary", 10), ("1000-ary", 1000)]

    where

    shape (name, k) =
        let limit = size `div` k
            prefix x = "iterate/" ++ name ++ "/" ++ x
         in [ (SpaceO_n, benchIO (prefix "wideConcatIterate")
                $ wideConcatIterate k limit)
            , (SpaceO_n, benchIO (prefix "wideConcatIteratePost")
                $ wideConcatIteratePost k limit)
            , (SpaceO_n, benchIO (prefix "wideConcatIterateBatchPost")
                $ wideConcatIterateBatchPost k limit)
            , (SpaceO_n, benchIO (prefix "wideConcatIterateBatchPostRev")
                $ wideConcatIterateBatchPostRev k limit)
            ]
