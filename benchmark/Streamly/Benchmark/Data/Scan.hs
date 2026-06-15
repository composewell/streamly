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
withStream :: Int -> (Stream IO Int -> IO b) -> IO b
withStream value f = randomRIO (1, 1 :: Int) >>= f . sourceUnfoldrM value

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> IO b -> Benchmark
benchIO name = bench name . nfIO

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

{-# INLINE scanMapM #-}
scanMapM :: Monad m => Int -> Stream m Int -> m ()
scanMapM n = composeN n $ Stream.scanr (Scan.functionM return)

{-# INLINE scanComposeMapM #-}
scanComposeMapM :: Monad m => Int -> Stream m Int -> m ()
scanComposeMapM n =
    composeN n $
    Stream.scanr
        (Scan.functionM (\x -> return (x + 1)) `Scan.compose`
         Scan.functionM (\x -> return (x + 2)))

{-# INLINE scanTeeMapM #-}
scanTeeMapM :: Monad m => Int -> Stream m Int -> m ()
scanTeeMapM n =
    composeN n $
    Stream.scanr
        (Scan.teeWith (+) (Scan.functionM (\x -> return (x + 1)))
         (Scan.functionM (\x -> return (x + 2))))

scansMapM :: Int -> IO ()
scansMapM value = withStream value (scanMapM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scansMapM
inspect $ 'scansMapM `hasNoType` ''S.Step
inspect $ 'scansMapM `hasNoType` ''S.RunScanState
inspect $ 'scansMapM `hasNoType` ''FL.Step
inspect $ 'scansMapM `hasNoType` ''SPEC
#endif

scansCompose :: Int -> IO ()
scansCompose value = withStream value (scanComposeMapM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scansCompose
inspect $ 'scansCompose `hasNoType` ''S.Step
inspect $ 'scansCompose `hasNoType` ''S.RunScanState
inspect $ 'scansCompose `hasNoType` ''FL.Step
inspect $ 'scansCompose `hasNoType` ''SPEC
#endif

scansTee :: Int -> IO ()
scansTee value = withStream value (scanTeeMapM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scansTee
inspect $ 'scansTee `hasNoType` ''S.Step
inspect $ 'scansTee `hasNoType` ''S.RunScanState
inspect $ 'scansTee `hasNoType` ''FL.Step
inspect $ 'scansTee `hasNoType` ''SPEC
#endif

scansMapMX4 :: Int -> IO ()
scansMapMX4 value = withStream value (scanMapM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scansMapMX4
inspect $ 'scansMapMX4 `hasNoType` ''S.Step
inspect $ 'scansMapMX4 `hasNoType` ''S.RunScanState
inspect $ 'scansMapMX4 `hasNoType` ''FL.Step
inspect $ 'scansMapMX4 `hasNoType` ''SPEC
#endif

scansComposeX4 :: Int -> IO ()
scansComposeX4 value = withStream value (scanComposeMapM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scansComposeX4
inspect $ 'scansComposeX4 `hasNoType` ''S.Step
inspect $ 'scansComposeX4 `hasNoType` ''S.RunScanState
inspect $ 'scansComposeX4 `hasNoType` ''FL.Step
inspect $ 'scansComposeX4 `hasNoType` ''SPEC
#endif

scansTeeX4 :: Int -> IO ()
scansTeeX4 value = withStream value (scanTeeMapM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scansTeeX4
inspect $ 'scansTeeX4 `hasNoType` ''S.Step
inspect $ 'scansTeeX4 `hasNoType` ''S.RunScanState
inspect $ 'scansTeeX4 `hasNoType` ''FL.Step
inspect $ 'scansTeeX4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Scan"

o_1_space :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space value =
    [ (SpaceO_1, benchIO "mapM" $ scansMapM value)
    , (SpaceO_1, benchIO "compose" $ scansCompose value)
    , (SpaceO_1, benchIO "tee" $ scansTee value)
    , (SpaceO_1, benchIO "mapM x 4" $ scansMapMX4 value)
    , (SpaceO_1, benchIO "compose x 4" $ scansComposeX4 value)
    , (SpaceO_1, benchIO "tee x 4" $ scansTeeX4 value)
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
