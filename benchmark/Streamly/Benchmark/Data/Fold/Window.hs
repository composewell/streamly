{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Control.DeepSeq (NFData)
import Streamly.Data.Fold (Fold)
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Fold as Window
import qualified Streamly.Internal.Data.RingArray as RingArray
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

o_1_space_folds :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_folds numElements =
    [ (SpaceO_1, benchWithFold numElements "fold minimum (window size 100)"
        (Window.windowMinimum 100))
    , (SpaceO_1, benchWithFold numElements "fold minimum (window size 1000)"
        (Window.windowMinimum 1000))
    , (SpaceO_1, benchWith sourceDescendingInt numElements
        "fold minimum descending (window size 1000)"
        (Window.windowMinimum 1000))

    , (SpaceO_1, benchWithFold numElements "fold maximum (window size 100)"
        (Window.windowMaximum 100))
    , (SpaceO_1, benchWithFold numElements "fold maximum (window size 1000)"
        (Window.windowMaximum 1000))
    , (SpaceO_1, benchWith sourceDescendingInt numElements
        "fold maximum descending (window size 1000)"
        (Window.windowMaximum 1000))

    , (SpaceO_1, benchWithFold numElements "fold range (window size 100)"
        (Window.windowRange 100))
    , (SpaceO_1, benchWithFold numElements "fold range (window size 1000)"
        (Window.windowRange 1000))
    , (SpaceO_1, benchWith sourceDescendingInt numElements
        "fold range descending (window size 1000)"
        (Window.windowRange 1000))

    , (SpaceO_1, benchWithFoldInt numElements "fold sumInt (window size 100)"
        (RingArray.slidingWindow 100 Window.windowSumInt))
    , (SpaceO_1, benchWithFoldInt numElements "fold sum for Int (window size 100)"
        (RingArray.slidingWindow 100 Window.windowSum))
    , (SpaceO_1, benchWithFold numElements "fold sum (window size 100)"
        (RingArray.slidingWindow 100 Window.windowSum))
    , (SpaceO_1, benchWithFold numElements "fold sum (window size 1000)"
        (RingArray.slidingWindow 1000 Window.windowSum))
    , (SpaceO_1, benchWithFold numElements "fold sum (entire stream)"
        (Window.cumulative Window.windowSum))
    , (SpaceO_1, benchWithFold numElements "fold sum (Data.Fold)"
        Fold.sum)

    , (SpaceO_1, benchWithFold numElements "fold mean (window size 100)"
        (RingArray.slidingWindow 100 Window.windowMean))
    , (SpaceO_1, benchWithFold numElements "fold mean (window size 1000)"
        (RingArray.slidingWindow 1000 Window.windowMean))
    , (SpaceO_1, benchWithFold numElements "fold mean (entire stream)"
        (Window.cumulative Window.windowMean))
    , (SpaceO_1, benchWithFold numElements "fold mean (Data.Fold)"
        Fold.mean)

    , (SpaceO_1, benchWithFold numElements "fold powerSum 2 (window size 100)"
        (RingArray.slidingWindow 100 (Window.windowPowerSum 2)))
    , (SpaceO_1, benchWithFold numElements "fold powerSum 2 (entire stream)"
        (Window.cumulative (Window.windowPowerSum 2)))
    ]

o_1_space_scans :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_scans numElements =
    [ (SpaceO_1, benchWithPostscan numElements "scan minimum (window size 10)"
        (Window.windowMinimum 10))
    -- Below window size 30 the linear search based impl performs better
    -- than the dequeue based implementation.
    , (SpaceO_1, benchWithPostscan numElements "scan minimum (window size 30)"
        (Window.windowMinimum 30))
    , (SpaceO_1, benchWithPostscan numElements "scan minimum (window size 1000)"
        (Window.windowMinimum 1000))
    , (SpaceO_1, benchScanWith sourceDescendingInt numElements
        "scan minimum descending (window size 1000)"
        (Window.windowMinimum 1000))

    , (SpaceO_1, benchWithPostscan numElements "scan maximum (window size 10)"
        (Window.windowMaximum 10))
    , (SpaceO_1, benchWithPostscan numElements "scan maximum (window size 30)"
        (Window.windowMaximum 30))
    , (SpaceO_1, benchWithPostscan numElements "scan maximum (window size 1000)"
        (Window.windowMaximum 1000))
    , (SpaceO_1, benchScanWith sourceDescendingInt numElements
        "scan maximum descending (window size 1000)"
        (Window.windowMaximum 1000))

    , (SpaceO_1, benchWithPostscan numElements "scan range (window size 10)"
        (Window.windowRange 10))
    , (SpaceO_1, benchWithPostscan numElements "scan range (window size 30)"
        (Window.windowRange 30))
    , (SpaceO_1, benchWithPostscan numElements "scan range (window size 1000)"
        (Window.windowRange 1000))
    , (SpaceO_1, benchScanWith sourceDescendingInt numElements
        "scan range descending (window size 1000)"
        (Window.windowRange 1000))

    , (SpaceO_1, benchWithPostscan numElements "scan sum (window size 100)"
        (RingArray.slidingWindow 100 Window.windowSum))
    , (SpaceO_1, benchWithPostscan numElements "scan sum (window size 1000)"
        (RingArray.slidingWindow 1000 Window.windowSum))

    , (SpaceO_1, benchWithPostscan numElements "scan mean (window size 100)"
        (RingArray.slidingWindow 100 Window.windowMean))
    , (SpaceO_1, benchWithPostscan numElements "scan mean (window size 1000)"
        (RingArray.slidingWindow 1000 Window.windowMean))

    , (SpaceO_1, benchWithPostscan numElements "scan powerSum 2 (window size 100)"
        (RingArray.slidingWindow 100 (Window.windowPowerSum 2)))
    , (SpaceO_1, benchWithPostscan numElements "scan powerSum 2 (window size 1000)"
        (RingArray.slidingWindow 1000 (Window.windowPowerSum 2)))
    ]

moduleName :: String
moduleName = "Data.Fold.Window"

main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks value =
        let allBenches =
                   o_1_space_folds value
                ++ o_1_space_scans value
            get x = map snd $ filter ((==) x . fst) allBenches
            o_1_space = get SpaceO_1
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        ]
