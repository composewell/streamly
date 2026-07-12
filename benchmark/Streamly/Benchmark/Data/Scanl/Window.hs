-- |
-- Module      : Scanl.Window
-- Copyright   : (c) 2024 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

module Scanl.Window (benchmarks) where

import GHC.Classes (IP)
import GHC.Stack (CallStack, SrcLoc)

import qualified Streamly.Internal.Data.Scanl as Scanl

import Fusion.Plugin.Types
import Scanl.Type (benchIO, withPostscanl, withPostscanlDesc, withPostscanlDouble)
import Streamly.Benchmark.Common
import Test.Tasty.Bench

-------------------------------------------------------------------------------
-- benchmarks
-------------------------------------------------------------------------------

{-# ANN windowMinimum (PermitPatternMatches [''Bool,''Double,''IO,''Int]) #-}
{-# ANN windowMinimum (PermitConstructions [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN windowMinimum (PermitTypeClasses [''IP]) #-}
{-# NOINLINE windowMinimum #-}
windowMinimum :: Int -> Int -> Int -> IO ()
windowMinimum win n = withPostscanlDouble n (Scanl.windowMinimum win)

{-# ANN windowMinimumInt (PermitPatternMatches [''IO,''Int]) #-}
{-# ANN windowMinimumInt (PermitConstructions [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN windowMinimumInt (PermitTypeClasses [''IP]) #-}
{-# NOINLINE windowMinimumInt #-}
windowMinimumInt :: Int -> Int -> Int -> IO ()
windowMinimumInt win n = withPostscanl n (Scanl.windowMinimum win)

{-# ANN windowMinimumDesc (PermitPatternMatches [''IO,''Int]) #-}
{-# ANN windowMinimumDesc (PermitConstructions [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN windowMinimumDesc (PermitTypeClasses [''IP]) #-}
{-# NOINLINE windowMinimumDesc #-}
windowMinimumDesc :: Int -> Int -> Int -> IO ()
windowMinimumDesc win n = withPostscanlDesc n (Scanl.windowMinimum win)

{-# ANN windowMaximum (PermitPatternMatches [''Bool,''Double,''IO,''Int]) #-}
{-# ANN windowMaximum (PermitConstructions [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN windowMaximum (PermitTypeClasses [''IP]) #-}
{-# NOINLINE windowMaximum #-}
windowMaximum :: Int -> Int -> Int -> IO ()
windowMaximum win n = withPostscanlDouble n (Scanl.windowMaximum win)

{-# ANN windowMaximumDesc (PermitPatternMatches [''IO,''Int]) #-}
{-# ANN windowMaximumDesc (PermitConstructions [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN windowMaximumDesc (PermitTypeClasses [''IP]) #-}
{-# NOINLINE windowMaximumDesc #-}
windowMaximumDesc :: Int -> Int -> Int -> IO ()
windowMaximumDesc win n = withPostscanlDesc n (Scanl.windowMaximum win)

{-# ANN windowRange (PermitPatternMatches [''Bool,''Double,''IO,''Int]) #-}
{-# ANN windowRange (PermitConstructions [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN windowRange (PermitTypeClasses [''IP]) #-}
{-# NOINLINE windowRange #-}
windowRange :: Int -> Int -> Int -> IO ()
windowRange win n = withPostscanlDouble n (Scanl.windowRange win)

{-# ANN windowRangeDesc (PermitPatternMatches [''IO,''Int]) #-}
{-# ANN windowRangeDesc (PermitConstructions [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN windowRangeDesc (PermitTypeClasses [''IP]) #-}
{-# NOINLINE windowRangeDesc #-}
windowRangeDesc :: Int -> Int -> Int -> IO ()
windowRangeDesc win n = withPostscanlDesc n (Scanl.windowRange win)

{-# ANN incrSum (PermitPatternMatches [''Bool,''Double,''IO,''Int]) #-}
{-# ANN incrSum (PermitConstructions [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN incrSum (PermitTypeClasses [''IP]) #-}
{-# NOINLINE incrSum #-}
incrSum :: Int -> Int -> Int -> IO ()
incrSum win n = withPostscanlDouble n (Scanl.incrScan win Scanl.incrSum)

{-# ANN incrSumCumulative (PermitPatternMatches [''Bool,''Double,''Int]) #-}
{-# ANN incrSumCumulative (PermitConstructions [''()]) #-}
{-# ANN incrSumCumulative (PermitTypeClasses []) #-}
{-# NOINLINE incrSumCumulative #-}
incrSumCumulative :: Int -> Int -> IO ()
incrSumCumulative n = withPostscanlDouble n (Scanl.cumulativeScan Scanl.incrSum)

{-# ANN incrSumInt (PermitPatternMatches [''IO,''Int]) #-}
{-# ANN incrSumInt (PermitConstructions [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN incrSumInt (PermitTypeClasses [''IP]) #-}
{-# NOINLINE incrSumInt #-}
incrSumInt :: Int -> Int -> Int -> IO ()
incrSumInt win n = withPostscanl n (Scanl.incrScan win Scanl.incrSumInt)

{-# ANN incrMean (PermitPatternMatches [''Bool,''Double,''IO,''Int]) #-}
{-# ANN incrMean (PermitConstructions [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN incrMean (PermitTypeClasses [''IP]) #-}
{-# NOINLINE incrMean #-}
incrMean :: Int -> Int -> Int -> IO ()
incrMean win n = withPostscanlDouble n (Scanl.incrScan win Scanl.incrMean)

{-# ANN incrMeanCumulative (PermitPatternMatches [''Bool,''Double,''Int]) #-}
{-# ANN incrMeanCumulative (PermitConstructions [''()]) #-}
{-# ANN incrMeanCumulative (PermitTypeClasses []) #-}
{-# NOINLINE incrMeanCumulative #-}
incrMeanCumulative :: Int -> Int -> IO ()
incrMeanCumulative n = withPostscanlDouble n (Scanl.cumulativeScan Scanl.incrMean)

{-# ANN incrPowerSum (PermitPatternMatches [''Bool,''Double,''IO,''Int]) #-}
{-# ANN incrPowerSum (PermitConstructions [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN incrPowerSum (PermitTypeClasses [''IP]) #-}
{-# NOINLINE incrPowerSum #-}
incrPowerSum :: Int -> Int -> Int -> IO ()
incrPowerSum win n = withPostscanlDouble n (Scanl.incrScan win (Scanl.incrPowerSum 2))

{-# ANN incrPowerSumCumulative (PermitPatternMatches [''Bool,''Double,''Int]) #-}
{-# ANN incrPowerSumCumulative (PermitConstructions [''()]) #-}
{-# ANN incrPowerSumCumulative (PermitTypeClasses []) #-}
{-# NOINLINE incrPowerSumCumulative #-}
incrPowerSumCumulative :: Int -> Int -> IO ()
incrPowerSumCumulative n =
    withPostscanlDouble n (Scanl.cumulativeScan (Scanl.incrPowerSum 2))

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks numElements =
    map (SpaceO_1,)
    [ benchIO "windowMinimum 10" (windowMinimum 10) numElements
    , benchIO "windowMinimumInt 10" (windowMinimumInt 10) numElements
    -- Below window size 30 the linear search based impl performs better
    -- than the dequeue based implementation.
    , benchIO "windowMinimum 30" (windowMinimum 30) numElements
    , benchIO "windowMinimum 1000" (windowMinimum 1000) numElements
    , benchIO "windowMinimum 1000 descending"
        (windowMinimumDesc 1000) numElements

    , benchIO "windowMaximum 10" (windowMaximum 10) numElements
    , benchIO "windowMaximum 30" (windowMaximum 30) numElements
    , benchIO "windowMaximum 1000" (windowMaximum 1000) numElements
    , benchIO "windowMaximum 1000 descending"
        (windowMaximumDesc 1000) numElements

    , benchIO "windowRange 10" (windowRange 10) numElements
    , benchIO "windowRange 30" (windowRange 30) numElements
    , benchIO "windowRange 1000" (windowRange 1000) numElements
    , benchIO "windowRange 1000 descending"
        (windowRangeDesc 1000) numElements

    , benchIO "incrSum 100" (incrSum 100) numElements
    , benchIO "incrSum 1000" (incrSum 1000) numElements
    , benchIO "incrSum cumulative" incrSumCumulative numElements

    , benchIO "incrSumInt 100" (incrSumInt 100) numElements
    , benchIO "incrSumInt 1000" (incrSumInt 1000) numElements

    , benchIO "incrMean 100" (incrMean 100) numElements
    , benchIO "incrMean 1000" (incrMean 1000) numElements
    , benchIO "incrMean cumulative" incrMeanCumulative numElements

    , benchIO "incrPowerSum 2 100" (incrPowerSum 100) numElements
    , benchIO "incrPowerSum 2 1000" (incrPowerSum 1000) numElements
    , benchIO "incrPowerSum 2" incrPowerSumCumulative numElements
    ]
