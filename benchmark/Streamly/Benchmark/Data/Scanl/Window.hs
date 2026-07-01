-- |
-- Module      : Scanl.Type
-- Copyright   : (c) 2024 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

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

{-# INLINE benchIO #-}
benchIO :: String -> IO () -> Benchmark
benchIO name f = bench name $ nfIO f

{-# INLINE withStream #-}
withStream :: (Num a, Stream.Enumerable a) => Int -> (Stream IO a -> IO b) -> IO b
withStream n f = randomRIO (1, 1 :: Int) >>= f . source n . fromIntegral

{-# INLINE withDescStream #-}
withDescStream :: (Num a, Stream.Enumerable a) => Int -> (Stream IO a -> IO b) -> IO b
withDescStream n f = randomRIO (1, 1 :: Int) >>= f . sourceDescending n . fromIntegral

-- | Run a scan over the stream as a postscan and drain the result.
{-# INLINE withPostscanl #-}
withPostscanl :: Int -> Scanl IO Double b -> IO ()
withPostscanl n s = withStream n $ Stream.fold Fold.drain . Stream.postscanl s

{-# INLINE withPostscanlInt #-}
withPostscanlInt :: Int -> Scanl IO Int b -> IO ()
withPostscanlInt n s = withStream n $ Stream.fold Fold.drain . Stream.postscanl s

{-# INLINE withPostscanlDescInt #-}
withPostscanlDescInt :: Int -> Scanl IO Int b -> IO ()
withPostscanlDescInt n s = withDescStream n $ Stream.fold Fold.drain . Stream.postscanl s

-------------------------------------------------------------------------------
-- benchmarks
-------------------------------------------------------------------------------

{-# NOINLINE windowMinimum #-}
windowMinimum :: Int -> Int -> IO ()
windowMinimum win n = withPostscanl n (Scanl.windowMinimum win)

{-# NOINLINE windowMinimumInt #-}
windowMinimumInt :: Int -> Int -> IO ()
windowMinimumInt win n = withPostscanlInt n (Scanl.windowMinimum win)

{-# NOINLINE windowMinimumDesc #-}
windowMinimumDesc :: Int -> Int -> IO ()
windowMinimumDesc win n = withPostscanlDescInt n (Scanl.windowMinimum win)

{-# NOINLINE windowMaximum #-}
windowMaximum :: Int -> Int -> IO ()
windowMaximum win n = withPostscanl n (Scanl.windowMaximum win)

{-# NOINLINE windowMaximumDesc #-}
windowMaximumDesc :: Int -> Int -> IO ()
windowMaximumDesc win n = withPostscanlDescInt n (Scanl.windowMaximum win)

{-# NOINLINE windowRange #-}
windowRange :: Int -> Int -> IO ()
windowRange win n = withPostscanl n (Scanl.windowRange win)

{-# NOINLINE windowRangeDesc #-}
windowRangeDesc :: Int -> Int -> IO ()
windowRangeDesc win n = withPostscanlDescInt n (Scanl.windowRange win)

{-# NOINLINE incrSum #-}
incrSum :: Int -> Int -> IO ()
incrSum win n = withPostscanl n (Scanl.incrScan win Scanl.incrSum)

{-# NOINLINE incrSumCumulative #-}
incrSumCumulative :: Int -> IO ()
incrSumCumulative n = withPostscanl n (Scanl.cumulativeScan Scanl.incrSum)

{-# NOINLINE incrSumInt #-}
incrSumInt :: Int -> Int -> IO ()
incrSumInt win n = withPostscanlInt n (Scanl.incrScan win Scanl.incrSumInt)

{-# NOINLINE incrMean #-}
incrMean :: Int -> Int -> IO ()
incrMean win n = withPostscanl n (Scanl.incrScan win Scanl.incrMean)

{-# NOINLINE incrMeanCumulative #-}
incrMeanCumulative :: Int -> IO ()
incrMeanCumulative n = withPostscanl n (Scanl.cumulativeScan Scanl.incrMean)

{-# NOINLINE incrPowerSum #-}
incrPowerSum :: Int -> Int -> IO ()
incrPowerSum win n = withPostscanl n (Scanl.incrScan win (Scanl.incrPowerSum 2))

{-# NOINLINE incrPowerSumCumulative #-}
incrPowerSumCumulative :: Int -> IO ()
incrPowerSumCumulative n =
    withPostscanl n (Scanl.cumulativeScan (Scanl.incrPowerSum 2))

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks numElements =
    map (SpaceO_1,)
    [ benchIO "windowMinimum 10" (windowMinimum 10 numElements)
    , benchIO "windowMinimumInt 10" (windowMinimumInt 10 numElements)
    -- Below window size 30 the linear search based impl performs better
    -- than the dequeue based implementation.
    , benchIO "windowMinimum 30" (windowMinimum 30 numElements)
    , benchIO "windowMinimum 1000" (windowMinimum 1000 numElements)
    , benchIO "windowMinimum 1000 descending"
        (windowMinimumDesc 1000 numElements)

    , benchIO "windowMaximum 10" (windowMaximum 10 numElements)
    , benchIO "windowMaximum 30" (windowMaximum 30 numElements)
    , benchIO "windowMaximum 1000" (windowMaximum 1000 numElements)
    , benchIO "windowMaximum 1000 descending"
        (windowMaximumDesc 1000 numElements)

    , benchIO "windowRange 10" (windowRange 10 numElements)
    , benchIO "windowRange 30" (windowRange 30 numElements)
    , benchIO "windowRange 1000" (windowRange 1000 numElements)
    , benchIO "windowRange 1000 descending"
        (windowRangeDesc 1000 numElements)

    , benchIO "incrSum 100" (incrSum 100 numElements)
    , benchIO "incrSum 1000" (incrSum 1000 numElements)
    , benchIO "incrSum cumulative" (incrSumCumulative numElements)

    , benchIO "incrSumInt 100" (incrSumInt 100 numElements)
    , benchIO "incrSumInt 1000" (incrSumInt 1000 numElements)

    , benchIO "incrMean 100" (incrMean 100 numElements)
    , benchIO "incrMean 1000" (incrMean 1000 numElements)
    , benchIO "incrMean cumulative" (incrMeanCumulative numElements)

    , benchIO "incrPowerSum 2 100" (incrPowerSum 100 numElements)
    , benchIO "incrPowerSum 2 1000" (incrPowerSum 1000 numElements)
    , benchIO "incrPowerSum 2" (incrPowerSumCumulative numElements)
    ]
