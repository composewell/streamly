-- |
-- Module      : Streamly.Benchmark.Data.Scan
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Main (main) where

import Control.DeepSeq (NFData)
import System.Random (randomRIO)
import Streamly.Internal.Data.Stream (Stream)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scan as Scan
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Streamly.Benchmark.Common
import Fusion.Plugin.Types

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection
import qualified Streamly.Internal.Data.Stream as S
#endif

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> Stream m Int
sourceUnfoldrM value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> Int -> IO b
withStream value f = f . sourceUnfoldrM value

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1, 1 :: Int) >>= f

{-# INLINE composeN #-}
composeN ::
       Monad m
    => Int
    -> (Stream m Int -> Stream m Int)
    -> Stream m Int
    -> m ()
composeN n f =
    case n of
        1 -> Stream.fold FL.drain . f
        2 -> Stream.fold FL.drain . f . f
        3 -> Stream.fold FL.drain . f . f . f
        4 -> Stream.fold FL.drain . f . f . f . f
        _ -> undefined

-------------------------------------------------------------------------------
-- Scan benchmarks
-------------------------------------------------------------------------------

{-# INLINE functionM #-}
functionM :: Monad m => Int -> Stream m Int -> m ()
functionM n = composeN n $ Stream.scanr (Scan.functionM return)

{-# INLINE compose #-}
compose :: Monad m => Int -> Stream m Int -> m ()
compose n =
    composeN n $
    Stream.scanr
        (Scan.functionM (\x -> return (x + 1)) `Scan.compose`
         Scan.functionM (\x -> return (x + 2)))

{-# INLINE teeWith #-}
teeWith :: Monad m => Int -> Stream m Int -> m ()
teeWith n =
    composeN n $
    Stream.scanr
        (Scan.teeWith (+) (Scan.functionM (\x -> return (x + 1)))
         (Scan.functionM (\x -> return (x + 2))))

{-# ANN functionM_x1 (PermitPatternMatches []) #-}
{-# ANN functionM_x1 (PermitConstructions [''()]) #-}
{-# ANN functionM_x1 (PermitTypeClasses []) #-}
{-# NOINLINE functionM_x1 #-}
functionM_x1 :: Int -> Int -> IO ()
functionM_x1 value = withStream value (functionM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'functionM_x1
inspect $ 'functionM_x1 `hasNoType` ''S.Step
inspect $ 'functionM_x1 `hasNoType` ''S.RunScanState
inspect $ 'functionM_x1 `hasNoType` ''FL.Step
inspect $ 'functionM_x1 `hasNoType` ''SPEC
#endif

{-# ANN compose_x1 (PermitPatternMatches []) #-}
{-# ANN compose_x1 (PermitConstructions [''()]) #-}
{-# ANN compose_x1 (PermitTypeClasses []) #-}
{-# NOINLINE compose_x1 #-}
compose_x1 :: Int -> Int -> IO ()
compose_x1 value = withStream value (compose 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'compose_x1
inspect $ 'compose_x1 `hasNoType` ''S.Step
inspect $ 'compose_x1 `hasNoType` ''S.RunScanState
inspect $ 'compose_x1 `hasNoType` ''FL.Step
inspect $ 'compose_x1 `hasNoType` ''SPEC
#endif

{-# ANN teeWith_x1 (PermitPatternMatches []) #-}
{-# ANN teeWith_x1 (PermitConstructions [''()]) #-}
{-# ANN teeWith_x1 (PermitTypeClasses []) #-}
{-# NOINLINE teeWith_x1 #-}
teeWith_x1 :: Int -> Int -> IO ()
teeWith_x1 value = withStream value (teeWith 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'teeWith_x1
inspect $ 'teeWith_x1 `hasNoType` ''S.Step
inspect $ 'teeWith_x1 `hasNoType` ''S.RunScanState
inspect $ 'teeWith_x1 `hasNoType` ''FL.Step
inspect $ 'teeWith_x1 `hasNoType` ''SPEC
#endif

{-# ANN functionM_x4 (PermitPatternMatches []) #-}
{-# ANN functionM_x4 (PermitConstructions [''()]) #-}
{-# ANN functionM_x4 (PermitTypeClasses []) #-}
{-# NOINLINE functionM_x4 #-}
functionM_x4 :: Int -> Int -> IO ()
functionM_x4 value = withStream value (functionM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'functionM_x4
inspect $ 'functionM_x4 `hasNoType` ''S.Step
inspect $ 'functionM_x4 `hasNoType` ''S.RunScanState
inspect $ 'functionM_x4 `hasNoType` ''FL.Step
inspect $ 'functionM_x4 `hasNoType` ''SPEC
#endif

{-# ANN compose_x4 (PermitPatternMatches []) #-}
{-# ANN compose_x4 (PermitConstructions [''()]) #-}
{-# ANN compose_x4 (PermitTypeClasses []) #-}
{-# NOINLINE compose_x4 #-}
compose_x4 :: Int -> Int -> IO ()
compose_x4 value = withStream value (compose 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'compose_x4
inspect $ 'compose_x4 `hasNoType` ''S.Step
inspect $ 'compose_x4 `hasNoType` ''S.RunScanState
inspect $ 'compose_x4 `hasNoType` ''FL.Step
inspect $ 'compose_x4 `hasNoType` ''SPEC
#endif

{-# ANN teeWith_x4 (PermitPatternMatches []) #-}
{-# ANN teeWith_x4 (PermitConstructions [''()]) #-}
{-# ANN teeWith_x4 (PermitTypeClasses []) #-}
{-# NOINLINE teeWith_x4 #-}
teeWith_x4 :: Int -> Int -> IO ()
teeWith_x4 value = withStream value (teeWith 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'teeWith_x4
inspect $ 'teeWith_x4 `hasNoType` ''S.Step
inspect $ 'teeWith_x4 `hasNoType` ''S.RunScanState
inspect $ 'teeWith_x4 `hasNoType` ''FL.Step
inspect $ 'teeWith_x4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Scan"

o_1_space :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space value =
    [ (SpaceO_1, benchIO "functionM_x1" $ functionM_x1 value)
    , (SpaceO_1, benchIO "compose_x1 (2 functionM)" $ compose_x1 value)
    , (SpaceO_1, benchIO "teeWith_x1 (2 functionM)" $ teeWith_x1 value)
    , (SpaceO_1, benchIO "functionM_x4" $ functionM_x4 value)
    , (SpaceO_1, benchIO "compose_x4 (2 functionM)" $ compose_x4 value)
    , (SpaceO_1, benchIO "teeWith_x4 (2 functionM)" $ teeWith_x4 value)
    ]

main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks value =
        let allBenches = o_1_space value
            get x = map snd $ filter ((==) x . fst) allBenches
        in
        [ bgroup (o_1_space_prefix moduleName) (get SpaceO_1)
        ]
