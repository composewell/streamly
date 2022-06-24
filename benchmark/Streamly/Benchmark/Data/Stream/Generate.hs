-- |
-- Module      : Stream.Generate
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Stream.Generate (benchmarks) where

import Stream.Common (sourceUnfoldr, benchIOSrc)
import System.IO.Unsafe (unsafeInterleaveIO)
import Streamly.Internal.Data.Stream (Stream)
import qualified Streamly.Internal.Data.Stream as Stream

import Prelude hiding (length, sum, or, and, any, all, notElem, elem, (!!),
    lookup, repeat, minimum, maximum, product, last, mapM_, init)

import Gauge
import Streamly.Benchmark.Common
import qualified Streamly.Data.Unfold as Unfold

{-# INLINE mfixUnfold #-}
mfixUnfold :: Int -> Int -> Stream IO (Int, Int)
mfixUnfold count start = Stream.mfix f
    where
    f action = do
        let incr n act = fmap ((+n) . snd)  $ unsafeInterleaveIO act
        x <- Stream.unfold Unfold.fromListM [incr 1 action, incr 2 action]
        y <- sourceUnfoldr count start
        return (x, y)

{-# INLINE fromFoldable #-}
fromFoldable :: Int -> Int -> Stream m Int
fromFoldable count start =
    Stream.fromFoldable (Prelude.enumFromTo count start)

{-# INLINE fromFoldableM #-}
fromFoldableM :: Monad m => Int -> Int -> Stream m Int
fromFoldableM count start =
    Stream.fromFoldableM (fmap return (Prelude.enumFromTo count start))

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
    [ bgroup "generation"
        [ benchIOSrc "unfold" (sourceUnfoldr value)
        , benchIOSrc "fromFoldable" (fromFoldable value)
        , benchIOSrc "fromFoldableM" (fromFoldableM value)
        , benchIOSrc "mfix_10" (mfixUnfold 10)
        , benchIOSrc "mfix_100" (mfixUnfold 100)
        , benchIOSrc "mfix_1000" (mfixUnfold 1000)
        ]
    ]

benchmarks :: String -> Int -> [Benchmark]
benchmarks moduleName size =
    [ bgroup (o_1_space_prefix moduleName) (o_1_space_generation size)
    ]
