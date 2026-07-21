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
import Scanl.Type
    (benchIO, withPostscanl, withPostscanlDesc, withPostscanlDouble)
import Streamly.Benchmark.Common
import Test.Tasty.Bench

-------------------------------------------------------------------------------
-- benchmarks
-------------------------------------------------------------------------------

{-# ANN windowMinimum_Double (PermitPatternMatches
    [''Bool,''Double,''IO,''Int]) #-}
{-# ANN windowMinimum_Double (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN windowMinimum_Double (PermitTypeClasses [''IP]) #-}
{-# NOINLINE windowMinimum_Double #-}
windowMinimum_Double :: Int -> Int -> Int -> IO ()
windowMinimum_Double win n = withPostscanlDouble n (Scanl.windowMinimum win)

{-# ANN windowMinimum_Int (PermitPatternMatches [''IO,''Int]) #-}
{-# ANN windowMinimum_Int (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN windowMinimum_Int (PermitTypeClasses [''IP]) #-}
{-# NOINLINE windowMinimum_Int #-}
windowMinimum_Int :: Int -> Int -> Int -> IO ()
windowMinimum_Int win n = withPostscanl n (Scanl.windowMinimum win)

{-# ANN windowMinimum_IntDesc (PermitPatternMatches [''IO,''Int]) #-}
{-# ANN windowMinimum_IntDesc (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN windowMinimum_IntDesc (PermitTypeClasses [''IP]) #-}
{-# NOINLINE windowMinimum_IntDesc #-}
windowMinimum_IntDesc :: Int -> Int -> Int -> IO ()
windowMinimum_IntDesc win n = withPostscanlDesc n (Scanl.windowMinimum win)

{-# ANN windowMaximum_Double (PermitPatternMatches
    [''Bool,''Double,''IO,''Int]) #-}
{-# ANN windowMaximum_Double (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN windowMaximum_Double (PermitTypeClasses [''IP]) #-}
{-# NOINLINE windowMaximum_Double #-}
windowMaximum_Double :: Int -> Int -> Int -> IO ()
windowMaximum_Double win n = withPostscanlDouble n (Scanl.windowMaximum win)

{-# ANN windowMaximum_IntDesc (PermitPatternMatches [''IO,''Int]) #-}
{-# ANN windowMaximum_IntDesc (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN windowMaximum_IntDesc (PermitTypeClasses [''IP]) #-}
{-# NOINLINE windowMaximum_IntDesc #-}
windowMaximum_IntDesc :: Int -> Int -> Int -> IO ()
windowMaximum_IntDesc win n = withPostscanlDesc n (Scanl.windowMaximum win)

{-# ANN windowRange_Double (PermitPatternMatches
    [''Bool,''Double,''IO,''Int]) #-}
{-# ANN windowRange_Double (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN windowRange_Double (PermitTypeClasses [''IP]) #-}
{-# NOINLINE windowRange_Double #-}
windowRange_Double :: Int -> Int -> Int -> IO ()
windowRange_Double win n = withPostscanlDouble n (Scanl.windowRange win)

{-# ANN windowRange_IntDesc (PermitPatternMatches [''IO,''Int]) #-}
{-# ANN windowRange_IntDesc (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN windowRange_IntDesc (PermitTypeClasses [''IP]) #-}
{-# NOINLINE windowRange_IntDesc #-}
windowRange_IntDesc :: Int -> Int -> Int -> IO ()
windowRange_IntDesc win n = withPostscanlDesc n (Scanl.windowRange win)

{-# ANN incrSum_Double (PermitPatternMatches [''Bool,''Double,''IO,''Int]) #-}
{-# ANN incrSum_Double (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN incrSum_Double (PermitTypeClasses [''IP]) #-}
{-# NOINLINE incrSum_Double #-}
incrSum_Double :: Int -> Int -> Int -> IO ()
incrSum_Double win n = withPostscanlDouble n (Scanl.incrScan win Scanl.incrSum)

{-# ANN incrSum_Double_Cumulative (PermitPatternMatches
    [''Bool,''Double,''Int]) #-}
{-# ANN incrSum_Double_Cumulative (PermitConstructions [''()]) #-}
{-# ANN incrSum_Double_Cumulative (PermitTypeClasses []) #-}
{-# NOINLINE incrSum_Double_Cumulative #-}
incrSum_Double_Cumulative :: Int -> Int -> IO ()
incrSum_Double_Cumulative n =
    withPostscanlDouble n (Scanl.cumulativeScan Scanl.incrSum)

{-# ANN incrSum_Int (PermitPatternMatches [''IO,''Int]) #-}
{-# ANN incrSum_Int (PermitConstructions [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN incrSum_Int (PermitTypeClasses [''IP]) #-}
{-# NOINLINE incrSum_Int #-}
incrSum_Int :: Int -> Int -> Int -> IO ()
incrSum_Int win n = withPostscanl n (Scanl.incrScan win Scanl.incrSumInt)

{-# ANN incrMean_Double (PermitPatternMatches [''Bool,''Double,''IO,''Int]) #-}
{-# ANN incrMean_Double (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN incrMean_Double (PermitTypeClasses [''IP]) #-}
{-# NOINLINE incrMean_Double #-}
incrMean_Double :: Int -> Int -> Int -> IO ()
incrMean_Double win n =
    withPostscanlDouble n (Scanl.incrScan win Scanl.incrMean)

{-# ANN incrMean_Double_Cumulative (PermitPatternMatches
    [''Bool,''Double,''Int]) #-}
{-# ANN incrMean_Double_Cumulative (PermitConstructions [''()]) #-}
{-# ANN incrMean_Double_Cumulative (PermitTypeClasses []) #-}
{-# NOINLINE incrMean_Double_Cumulative #-}
incrMean_Double_Cumulative :: Int -> Int -> IO ()
incrMean_Double_Cumulative n =
    withPostscanlDouble n (Scanl.cumulativeScan Scanl.incrMean)

{-# ANN incrPowerSum_Double (PermitPatternMatches
    [''Bool,''Double,''IO,''Int]) #-}
{-# ANN incrPowerSum_Double (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''()]) #-}
{-# ANN incrPowerSum_Double (PermitTypeClasses [''IP]) #-}
{-# NOINLINE incrPowerSum_Double #-}
incrPowerSum_Double :: Int -> Int -> Int -> IO ()
incrPowerSum_Double win n =
    withPostscanlDouble n (Scanl.incrScan win (Scanl.incrPowerSum 2))

{-# ANN incrPowerSum_Double_Cumulative (PermitPatternMatches
    [''Bool,''Double,''Int]) #-}
{-# ANN incrPowerSum_Double_Cumulative (PermitConstructions [''()]) #-}
{-# ANN incrPowerSum_Double_Cumulative (PermitTypeClasses []) #-}
{-# NOINLINE incrPowerSum_Double_Cumulative #-}
incrPowerSum_Double_Cumulative :: Int -> Int -> IO ()
incrPowerSum_Double_Cumulative n =
    withPostscanlDouble n (Scanl.cumulativeScan (Scanl.incrPowerSum 2))

-- Benchmark naming: name each benchmark (and its IO action) after the exported
-- function it benchmarks, using combinator_dimension1_dimension2..., where the
-- dimensions are optional variants/type specializations (used esp. when more
-- than one specialization is benchmarked). Keep extra info in parenthetical
-- notes in the description; these also disambiguate benchmarks that reuse a
-- single IO action with different arguments. If the name has a trailing
-- underscore, add one more underscore.
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks numElements =
    map (SpaceO_1,)
    [ benchIO "windowMinimum_Double (10)" (windowMinimum_Double 10) numElements
    , benchIO "windowMinimum_Int (10)" (windowMinimum_Int 10) numElements
    -- Below window size 30 the linear search based impl performs better
    -- than the dequeue based implementation.
    , benchIO "windowMinimum_Double (30)" (windowMinimum_Double 30) numElements
    , benchIO "windowMinimum_Double (1000)" (windowMinimum_Double 1000)
        numElements
    , benchIO "windowMinimum_IntDesc (1000)"
        (windowMinimum_IntDesc 1000) numElements

    , benchIO "windowMaximum_Double (10)" (windowMaximum_Double 10) numElements
    , benchIO "windowMaximum_Double (30)" (windowMaximum_Double 30) numElements
    , benchIO "windowMaximum_Double (1000)" (windowMaximum_Double 1000)
        numElements
    , benchIO "windowMaximum_IntDesc (1000)"
        (windowMaximum_IntDesc 1000) numElements

    , benchIO "windowRange_Double (10)" (windowRange_Double 10) numElements
    , benchIO "windowRange_Double (30)" (windowRange_Double 30) numElements
    , benchIO "windowRange_Double (1000)" (windowRange_Double 1000) numElements
    , benchIO "windowRange_IntDesc (1000)"
        (windowRange_IntDesc 1000) numElements

    , benchIO "incrSum_Double (100)" (incrSum_Double 100) numElements
    , benchIO "incrSum_Double (1000)" (incrSum_Double 1000) numElements
    , benchIO "incrSum_Double_Cumulative" incrSum_Double_Cumulative numElements

    , benchIO "incrSum_Int (100)" (incrSum_Int 100) numElements
    , benchIO "incrSum_Int (1000)" (incrSum_Int 1000) numElements

    , benchIO "incrMean_Double (100)" (incrMean_Double 100) numElements
    , benchIO "incrMean_Double (1000)" (incrMean_Double 1000) numElements
    , benchIO "incrMean_Double_Cumulative" incrMean_Double_Cumulative
        numElements

    , benchIO "incrPowerSum_Double (2, 100)" (incrPowerSum_Double 100)
        numElements
    , benchIO "incrPowerSum_Double (2, 1000)" (incrPowerSum_Double 1000)
        numElements
    , benchIO "incrPowerSum_Double_Cumulative" incrPowerSum_Double_Cumulative
        numElements
    ]
