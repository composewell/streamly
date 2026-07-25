-- |
-- Module      : Streamly.Benchmark.Data.Pipe
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
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import Prelude hiding (mapM)

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
-- Pipe benchmarks
-------------------------------------------------------------------------------

{-# INLINE mapM #-}
mapM :: Monad m => Int -> Stream m Int -> m ()
mapM n = composeN n $ Stream.pipe (Pipe.mapM return)

{-# INLINE compose #-}
compose :: Monad m => Int -> Stream m Int -> m ()
compose n =
    composeN n $
    Stream.pipe
        (Pipe.mapM (\x -> return (x + 1)) `Pipe.compose`
         Pipe.mapM (\x -> return (x + 2)))

{-# INLINE teeMerge #-}
teeMerge :: Monad m => Int -> Stream m Int -> m ()
teeMerge n =
    composeN n $
    Stream.pipe
        (Pipe.mapM (\x -> return (x + 1)) `Pipe.teeMerge`
         Pipe.mapM (\x -> return (x + 2)))

{-# ANN mapM_x1 (PermitPatternMatches []) #-}
{-# ANN mapM_x1 (PermitConstructions [''()]) #-}
{-# ANN mapM_x1 (PermitTypeClasses []) #-}
{-# NOINLINE mapM_x1 #-}
mapM_x1 :: Int -> Int -> IO ()
mapM_x1 value = withStream value (mapM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapM_x1
inspect $ 'mapM_x1 `hasNoType` ''S.Step
inspect $ 'mapM_x1 `hasNoType` ''S.PipeState
inspect $ 'mapM_x1 `hasNoType` ''FL.Step
inspect $ 'mapM_x1 `hasNoType` ''SPEC
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
inspect $ 'compose_x1 `hasNoType` ''S.PipeState
inspect $ 'compose_x1 `hasNoType` ''FL.Step
inspect $ 'compose_x1 `hasNoType` ''SPEC
#endif

{-# ANN teeMerge_x1 (PermitPatternMatches []) #-}
{-# ANN teeMerge_x1 (PermitConstructions [''()]) #-}
{-# ANN teeMerge_x1 (PermitTypeClasses []) #-}
{-# NOINLINE teeMerge_x1 #-}
teeMerge_x1 :: Int -> Int -> IO ()
teeMerge_x1 value = withStream value (teeMerge 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'teeMerge_x1
inspect $ 'teeMerge_x1 `hasNoType` ''S.Step
inspect $ 'teeMerge_x1 `hasNoType` ''S.PipeState
inspect $ 'teeMerge_x1 `hasNoType` ''FL.Step
inspect $ 'teeMerge_x1 `hasNoType` ''SPEC
#endif

-- XXX this takes 1 GB memory to compile
-- pipeZip :: Int -> IO ()

{-# ANN mapM_x4 (PermitPatternMatches []) #-}
{-# ANN mapM_x4 (PermitConstructions [''()]) #-}
{-# ANN mapM_x4 (PermitTypeClasses []) #-}
{-# NOINLINE mapM_x4 #-}
mapM_x4 :: Int -> Int -> IO ()
mapM_x4 value = withStream value (mapM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapM_x4
inspect $ 'mapM_x4 `hasNoType` ''S.Step
inspect $ 'mapM_x4 `hasNoType` ''S.PipeState
inspect $ 'mapM_x4 `hasNoType` ''FL.Step
inspect $ 'mapM_x4 `hasNoType` ''SPEC
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
inspect $ 'compose_x4 `hasNoType` ''S.PipeState
inspect $ 'compose_x4 `hasNoType` ''FL.Step
inspect $ 'compose_x4 `hasNoType` ''SPEC
#endif

-- XXX requires @-fspec-constr-recursive=16@.
{-# ANN teeMerge_x4 (PermitPatternMatches []) #-}
{-# ANN teeMerge_x4 (PermitConstructions [''()]) #-}
{-# ANN teeMerge_x4 (PermitTypeClasses []) #-}
{-# NOINLINE teeMerge_x4 #-}
teeMerge_x4 :: Int -> Int -> IO ()
teeMerge_x4 value = withStream value (teeMerge 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'teeMerge_x4
inspect $ 'teeMerge_x4 `hasNoType` ''S.Step
inspect $ 'teeMerge_x4 `hasNoType` ''S.PipeState
inspect $ 'teeMerge_x4 `hasNoType` ''FL.Step
inspect $ 'teeMerge_x4 `hasNoType` ''SPEC
#endif

-- XXX this takes 1 GB memory to compile
-- pipeZipX4 :: Int -> IO ()

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Pipe"

o_1_space :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space value =
    [ (SpaceO_1, benchIO "mapM_x1" $ mapM_x1 value)
    , (SpaceO_1, benchIO "compose_x1 (2 mapM)" $ compose_x1 value)
    , (SpaceO_1, benchIO "teeMerge_x1 (2 mapM)" $ teeMerge_x1 value)
    , (SpaceO_1, benchIO "mapM_x4" $ mapM_x4 value)
    , (SpaceO_1, benchIO "compose_x4 (2 mapM)" $ compose_x4 value)
    , (SpaceO_1, benchIO "teeMerge_x4 (2 mapM)" $ teeMerge_x4 value)
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
