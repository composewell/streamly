module Scanl.Window (benchmarks) where

import Streamly.Internal.Data.Scanl (Scanl)
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream

import Streamly.Benchmark.Common
import Test.Tasty.Bench

{-# INLINE source #-}
source :: (Monad m, Num a, Stream.Enumerable a) =>
    Int -> a -> Stream m a
source len from =
    Stream.enumerateFromThenTo from (from + 1) (from + fromIntegral len)

{-# INLINE sourceDescending #-}
sourceDescending :: (Monad m, Num a, Stream.Enumerable a) =>
    Int -> a -> Stream m a
sourceDescending len from =
    Stream.enumerateFromThenTo
        (from + fromIntegral len)
        (from + fromIntegral (len - 1))
        from

{-# INLINE sourceDescendingInt #-}
sourceDescendingInt :: Monad m => Int -> Int -> Stream m Int
sourceDescendingInt = sourceDescending

{-# INLINE benchScanWith #-}
benchScanWith :: Num a =>
    (Int -> a -> Stream IO a) -> Int -> String -> Scanl IO a b -> Benchmark
benchScanWith src len name f =
    bench name
        $ nfIO
        $ randomRIO (1, 1 :: Int)
            >>= Stream.fold Fold.drain
                . Stream.postscanl f
                . src len
                . fromIntegral

{-# INLINE benchWithPostscan #-}
benchWithPostscan :: Int -> String -> Scanl IO Double a -> Benchmark
benchWithPostscan = benchScanWith source

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks numElements =
    [ (SpaceO_1, benchWithPostscan numElements "minimum (window size 10)"
        (Scanl.windowMinimum 10))
    -- Below window size 30 the linear search based impl performs better
    -- than the dequeue based implementation.
    , (SpaceO_1, benchWithPostscan numElements "minimum (window size 30)"
        (Scanl.windowMinimum 30))
    , (SpaceO_1, benchWithPostscan numElements "minimum (window size 1000)"
        (Scanl.windowMinimum 1000))
    , (SpaceO_1, benchScanWith sourceDescendingInt numElements
        "minimum descending (window size 1000)"
        (Scanl.windowMinimum 1000))

    , (SpaceO_1, benchWithPostscan numElements "maximum (window size 10)"
        (Scanl.windowMaximum 10))
    , (SpaceO_1, benchWithPostscan numElements "maximum (window size 30)"
        (Scanl.windowMaximum 30))
    , (SpaceO_1, benchWithPostscan numElements "maximum (window size 1000)"
        (Scanl.windowMaximum 1000))
    , (SpaceO_1, benchScanWith sourceDescendingInt numElements
        "maximum descending (window size 1000)"
        (Scanl.windowMaximum 1000))

    , (SpaceO_1, benchWithPostscan numElements "range (window size 10)"
        (Scanl.windowRange 10))
    , (SpaceO_1, benchWithPostscan numElements "range (window size 30)"
        (Scanl.windowRange 30))
    , (SpaceO_1, benchWithPostscan numElements "range (window size 1000)"
        (Scanl.windowRange 1000))
    , (SpaceO_1, benchScanWith sourceDescendingInt numElements
        "range descending (window size 1000)"
        (Scanl.windowRange 1000))

    , (SpaceO_1, benchWithPostscan numElements "sum (window size 100)"
        (Scanl.incrScan 100 Scanl.incrSum))
    , (SpaceO_1, benchWithPostscan numElements "sum (window size 1000)"
        (Scanl.incrScan 1000 Scanl.incrSum))

    , (SpaceO_1, benchWithPostscan numElements "mean (window size 100)"
        (Scanl.incrScan 100 Scanl.incrMean))
    , (SpaceO_1, benchWithPostscan numElements "mean (window size 1000)"
        (Scanl.incrScan 1000 Scanl.incrMean))

    , (SpaceO_1, benchWithPostscan numElements "powerSum 2 (window size 100)"
        (Scanl.incrScan 100 (Scanl.incrPowerSum 2)))
    , (SpaceO_1, benchWithPostscan numElements "powerSum 2 (window size 1000)"
        (Scanl.incrScan 1000 (Scanl.incrPowerSum 2)))
    ]
