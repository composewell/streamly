module Main (main) where

import Control.DeepSeq (NFData)
import Streamly.Data.Fold (Fold)
import Streamly.Prelude (SerialT)
import System.Random (randomRIO)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Ring.Foreign as Ring
import qualified Streamly.Prelude as Stream
import qualified Streamly.Internal.Data.Fold.Window as Window

import Gauge

{-# INLINE source #-}
source :: (Monad m, Stream.IsStream t, Num a, Stream.Enumerable a) =>
    Int -> a -> t m a
source len from =
    Stream.enumerateFromThenTo from (from + 1) (from + fromIntegral len)

{-# INLINE sourceDescending #-}
sourceDescending :: (Monad m, Stream.IsStream t, Num a, Stream.Enumerable a) =>
    Int -> a -> t m a
sourceDescending len from =
    Stream.enumerateFromThenTo
        (from + fromIntegral len)
        (from + fromIntegral (len - 1))
        from

{-# INLINE sourceDescendingInt #-}
sourceDescendingInt :: (Monad m, Stream.IsStream t) => Int -> Int -> t m Int
sourceDescendingInt = sourceDescending

{-# INLINE benchWith #-}
benchWith :: (Num a, NFData b) =>
    (Int -> a -> SerialT IO a) -> Int -> String -> Fold IO a b -> Benchmark
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
    (Int -> a -> SerialT IO a) -> Int -> String -> Fold IO a b -> Benchmark
benchScanWith src len name f =
    bench name
        $ nfIO
        $ randomRIO (1, 1 :: Int)
            >>= Stream.drain . Stream.postscan f . src len . fromIntegral

{-# INLINE benchWithPostscan #-}
benchWithPostscan :: Int -> String -> Fold IO Double a -> Benchmark
benchWithPostscan = benchScanWith source

{-# INLINE numElements #-}
numElements :: Int
numElements = 100000

main :: IO ()
main =
  defaultMain
    [ bgroup
        "fold"
        [ benchWithFold numElements "minimum (window size 100)"
            (Window.minimum 100)
        , benchWithFold numElements "minimum (window size 1000)"
            (Window.minimum 1000)
        , benchWith sourceDescendingInt numElements
            "minimum descending (window size 1000)"
            (Window.minimum 1000)

        , benchWithFold numElements "maximum (window size 100)"
            (Window.maximum 100)
        , benchWithFold numElements "maximum (window size 1000)"
            (Window.maximum 1000)
        , benchWith sourceDescendingInt numElements
            "maximum descending (window size 1000)"
            (Window.maximum 1000)

        , benchWithFold numElements "range (window size 100)"
            (Window.range 100)
        , benchWithFold numElements "range (window size 1000)"
            (Window.range 1000)
        , benchWith sourceDescendingInt numElements
            "range descending (window size 1000)"
            (Window.range 1000)

        , benchWithFoldInt numElements "sumInt (window size 100)"
            (Ring.slidingWindow 100 Window.sumInt)
        , benchWithFoldInt numElements "sum for Int (window size 100)"
            (Ring.slidingWindow 100 Window.sum)
        , benchWithFold numElements "sum (window size 100)"
            (Ring.slidingWindow 100 Window.sum)
        , benchWithFold numElements "sum (window size 1000)"
            (Ring.slidingWindow 1000 Window.sum)
        , benchWithFold numElements "sum (entire stream)"
            (Window.cumulative Window.sum)
        , benchWithFold numElements "sum (Data.Fold)"
            Fold.sum

        , benchWithFold numElements "mean (window size 100)"
            (Ring.slidingWindow 100 Window.mean)
        , benchWithFold numElements "mean (window size 1000)"
            (Ring.slidingWindow 1000 Window.mean)
        , benchWithFold numElements "mean (entire stream)"
            (Window.cumulative Window.mean)
        , benchWithFold numElements "mean (Data.Fold)"
            Fold.mean

        , benchWithFold numElements "powerSum 2 (window size 100)"
            (Ring.slidingWindow 100 (Window.powerSum 2))
        , benchWithFold numElements "powerSum 2 (entire stream)"
            (Window.cumulative (Window.powerSum 2))

        ]
    , bgroup
        "scan"
        [ benchWithPostscan numElements "minimum (window size 10)"
            (Window.minimum 10)
        -- Below window size 30 the linear search based impl performs better
        -- than the dequeue based implementation.
        , benchWithPostscan numElements "minimum (window size 30)"
            (Window.minimum 30)
        , benchWithPostscan numElements "minimum (window size 1000)"
            (Window.minimum 1000)
        , benchScanWith sourceDescendingInt numElements
            "minimum descending (window size 1000)"
            (Window.minimum 1000)

        , benchWithPostscan numElements "maximum (window size 10)"
            (Window.maximum 10)
        , benchWithPostscan numElements "maximum (window size 30)"
            (Window.maximum 30)
        , benchWithPostscan numElements "maximum (window size 1000)"
            (Window.maximum 1000)
        , benchScanWith sourceDescendingInt numElements
            "maximum descending (window size 1000)"
            (Window.maximum 1000)

        , benchWithPostscan numElements "range (window size 10)"
            (Window.range 10)
        , benchWithPostscan numElements "range (window size 30)"
            (Window.range 30)
        , benchWithPostscan numElements "range (window size 1000)"
            (Window.range 1000)
        , benchScanWith sourceDescendingInt numElements
            "range descending (window size 1000)"
            (Window.range 1000)

        , benchWithPostscan numElements "sum (window size 100)"
            (Ring.slidingWindow 100 Window.sum)
        , benchWithPostscan numElements "sum (window size 1000)"
            (Ring.slidingWindow 1000 Window.sum)

        , benchWithPostscan numElements "mean (window size 100)"
            (Ring.slidingWindow 100 Window.mean)
        , benchWithPostscan numElements "mean (window size 1000)"
            (Ring.slidingWindow 1000 Window.mean)

        , benchWithPostscan numElements "powerSum 2 (window size 100)"
            (Ring.slidingWindow 100 (Window.powerSum 2))
        , benchWithPostscan numElements "powerSum 2 (window size 1000)"
            (Ring.slidingWindow 1000 (Window.powerSum 2))
        ]
    ]
