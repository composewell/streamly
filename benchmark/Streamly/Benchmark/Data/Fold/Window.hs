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
benchWith :: (Num a, NFData a) =>
    (Int -> a -> SerialT IO a) -> Int -> String -> Fold IO a a -> Benchmark
benchWith src len name f =
    bench name
        $ nfIO
        $ randomRIO (1, 1 :: Int) >>= Stream.fold f . src len . fromIntegral

{-# INLINE benchWithFold #-}
benchWithFold :: Int -> String -> Fold IO Double Double -> Benchmark
benchWithFold = benchWith source

{-# INLINE benchWithFoldInt #-}
benchWithFoldInt :: Int -> String -> Fold IO Int Int -> Benchmark
benchWithFoldInt = benchWith source

{-# INLINE benchWithPostscan #-}
benchWithPostscan :: Int -> String -> Fold IO Double Double -> Benchmark
benchWithPostscan len name f =
  bench name $ nfIO $ randomRIO (1, 1) >>=
    Stream.drain . Stream.postscan f . source len

{-# INLINE numElements #-}
numElements :: Int
numElements = 100000

main :: IO ()
main =
  defaultMain
    [ bgroup
        "fold"
        [ benchWithFold numElements "minimum (window size 100)"
            (Ring.slidingWindow 100 Window.minimum)
        , benchWithFold numElements "minimum (window size 1000)"
            (Ring.slidingWindow 1000 Window.minimum)
        , benchWith sourceDescendingInt numElements
            "minimum descending (window size 1000)"
            (Ring.slidingWindow 1000 Window.minimum)
        , benchWithFold numElements "maximum (window size 100)"
            (Ring.slidingWindow 100 Window.maximum)
        , benchWithFold numElements "maximum (window size 1000)"
            (Ring.slidingWindow 1000 Window.maximum)
        , benchWith sourceDescendingInt numElements
            "maximum descending (window size 1000)"
            (Ring.slidingWindow 1000 Window.maximum)
        , benchWithFold numElements "range (window size 100)"
            (Ring.slidingWindow 100 Window.range)
        , benchWithFold numElements "range (window size 1000)"
            (Ring.slidingWindow 1000 Window.range)
        , benchWithFoldInt numElements "sumInt (window size 100)"
            (Ring.slidingWindow 100 Window.sumInt)
        , benchWithFoldInt numElements "sum for Int (window size 100)"
            (Ring.slidingWindow 100 Window.sum)
        , benchWithFold numElements "sum (window size 100)"
            (Ring.slidingWindow 100 Window.sum)
        , benchWithFold numElements "sum (window size 1000)"
            (Ring.slidingWindow 1000 Window.sum)
        , benchWithFold numElements "sum (entire stream)"
            (Window.whole Window.sum)
        , benchWithFold numElements "sum (Data.Fold)"
            Fold.sum
        ]
    , bgroup
        "scan"
        [ benchWithPostscan numElements "minimum (window size 100)"
            (Ring.slidingWindow 100 Window.minimum)
        , benchWithPostscan numElements "minimum (window size 1000)"
            (Ring.slidingWindow 1000 Window.minimum)
        , benchWithPostscan numElements "maximum (window size 100)"
            (Ring.slidingWindow 100 Window.maximum)
        , benchWithPostscan numElements "maximum (window size 1000)"
            (Ring.slidingWindow 1000 Window.maximum)
        , benchWithPostscan numElements "range (window size 100)"
            (Ring.slidingWindow 100 Window.range)
        , benchWithPostscan numElements "range (window size 1000)"
            (Ring.slidingWindow 1000 Window.range)
        , benchWithPostscan numElements "sum (window size 100)"
            (Ring.slidingWindow 100 Window.sum)
        , benchWithPostscan numElements "sum (window size 1000)"
            (Ring.slidingWindow 1000 Window.sum)
        ]
    ]
