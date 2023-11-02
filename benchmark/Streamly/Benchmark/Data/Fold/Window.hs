{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Control.DeepSeq (NFData)
import Streamly.Data.Fold (Fold)
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Fold as Window
import qualified Streamly.Internal.Data.Ring as Ring
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

{-# INLINE benchWith #-}
benchWith :: (Num a, NFData b) =>
    (Int -> a -> Stream IO a) -> Int -> String -> Fold IO a b -> Benchmark
benchWith src len name f =
    bench name
        $ nfIO
        $ randomRIO (1, 1 :: Int) >>= Stream.fold f . src len . fromIntegral

{-# INLINE benchWithFold #-}
benchWithFold :: NFData a => Int -> String -> Fold IO Double a -> Benchmark
benchWithFold = benchWith source

{-# INLINE benchWithFoldInt #-}
benchWithFoldInt :: Int -> String -> Fold IO Int Int -> Benchmark
benchWithFoldInt = benchWith source

{-# INLINE benchScanWith #-}
benchScanWith :: Num a =>
    (Int -> a -> Stream IO a) -> Int -> String -> Fold IO a b -> Benchmark
benchScanWith src len name f =
    bench name
        $ nfIO
        $ randomRIO (1, 1 :: Int)
            >>= Stream.fold Fold.drain
                . Stream.postscan f
                . src len
                . fromIntegral

{-# INLINE benchWithPostscan #-}
benchWithPostscan :: Int -> String -> Fold IO Double a -> Benchmark
benchWithPostscan = benchScanWith source

o_1_space_folds :: Int -> [Benchmark]
o_1_space_folds numElements =
    [ bgroup "fold"
        [ benchWithFold numElements "minimum (window size 100)"
            (Window.windowMinimum 100)
        , benchWithFold numElements "minimum (window size 1000)"
            (Window.windowMinimum 1000)
        , benchWith sourceDescendingInt numElements
            "minimum descending (window size 1000)"
            (Window.windowMinimum 1000)

        , benchWithFold numElements "maximum (window size 100)"
            (Window.windowMaximum 100)
        , benchWithFold numElements "maximum (window size 1000)"
            (Window.windowMaximum 1000)
        , benchWith sourceDescendingInt numElements
            "maximum descending (window size 1000)"
            (Window.windowMaximum 1000)

        , benchWithFold numElements "range (window size 100)"
            (Window.windowRange 100)
        , benchWithFold numElements "range (window size 1000)"
            (Window.windowRange 1000)
        , benchWith sourceDescendingInt numElements
            "range descending (window size 1000)"
            (Window.windowRange 1000)

        , benchWithFoldInt numElements "sumInt (window size 100)"
            (Ring.slidingWindow 100 Window.windowSumInt)
        , benchWithFoldInt numElements "sum for Int (window size 100)"
            (Ring.slidingWindow 100 Window.windowSum)
        , benchWithFold numElements "sum (window size 100)"
            (Ring.slidingWindow 100 Window.windowSum)
        , benchWithFold numElements "sum (window size 1000)"
            (Ring.slidingWindow 1000 Window.windowSum)
        , benchWithFold numElements "sum (entire stream)"
            (Window.cumulative Window.windowSum)
        , benchWithFold numElements "sum (Data.Fold)"
            Fold.sum

        , benchWithFold numElements "mean (window size 100)"
            (Ring.slidingWindow 100 Window.windowMean)
        , benchWithFold numElements "mean (window size 1000)"
            (Ring.slidingWindow 1000 Window.windowMean)
        , benchWithFold numElements "mean (entire stream)"
            (Window.cumulative Window.windowMean)
        , benchWithFold numElements "mean (Data.Fold)"
            Fold.mean

        , benchWithFold numElements "powerSum 2 (window size 100)"
            (Ring.slidingWindow 100 (Window.windowPowerSum 2))
        , benchWithFold numElements "powerSum 2 (entire stream)"
            (Window.cumulative (Window.windowPowerSum 2))

        ]
    ]

o_1_space_scans :: Int -> [Benchmark]
o_1_space_scans numElements =
    [ bgroup "scan"
        [ benchWithPostscan numElements "minimum (window size 10)"
            (Window.windowMinimum 10)
        -- Below window size 30 the linear search based impl performs better
        -- than the dequeue based implementation.
        , benchWithPostscan numElements "minimum (window size 30)"
            (Window.windowMinimum 30)
        , benchWithPostscan numElements "minimum (window size 1000)"
            (Window.windowMinimum 1000)
        , benchScanWith sourceDescendingInt numElements
            "minimum descending (window size 1000)"
            (Window.windowMinimum 1000)

        , benchWithPostscan numElements "maximum (window size 10)"
            (Window.windowMaximum 10)
        , benchWithPostscan numElements "maximum (window size 30)"
            (Window.windowMaximum 30)
        , benchWithPostscan numElements "maximum (window size 1000)"
            (Window.windowMaximum 1000)
        , benchScanWith sourceDescendingInt numElements
            "maximum descending (window size 1000)"
            (Window.windowMaximum 1000)

        , benchWithPostscan numElements "range (window size 10)"
            (Window.windowRange 10)
        , benchWithPostscan numElements "range (window size 30)"
            (Window.windowRange 30)
        , benchWithPostscan numElements "range (window size 1000)"
            (Window.windowRange 1000)
        , benchScanWith sourceDescendingInt numElements
            "range descending (window size 1000)"
            (Window.windowRange 1000)

        , benchWithPostscan numElements "sum (window size 100)"
            (Ring.slidingWindow 100 Window.windowSum)
        , benchWithPostscan numElements "sum (window size 1000)"
            (Ring.slidingWindow 1000 Window.windowSum)

        , benchWithPostscan numElements "mean (window size 100)"
            (Ring.slidingWindow 100 Window.windowMean)
        , benchWithPostscan numElements "mean (window size 1000)"
            (Ring.slidingWindow 1000 Window.windowMean)

        , benchWithPostscan numElements "powerSum 2 (window size 100)"
            (Ring.slidingWindow 100 (Window.windowPowerSum 2))
        , benchWithPostscan numElements "powerSum 2 (window size 1000)"
            (Ring.slidingWindow 1000 (Window.windowPowerSum 2))
        ]
    ]

moduleName :: String
moduleName = "Data.Fold.Window"

main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks value =
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [ o_1_space_folds value
            , o_1_space_scans value
            ]
        ]
