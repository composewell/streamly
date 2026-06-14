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
-- Pipe benchmarks
-------------------------------------------------------------------------------

{-# INLINE transformMapM #-}
transformMapM :: Monad m => Int -> Stream m Int -> m ()
transformMapM n = composeN n $ Stream.pipe (Pipe.mapM return)

{-# INLINE transformComposeMapM #-}
transformComposeMapM :: Monad m => Int -> Stream m Int -> m ()
transformComposeMapM n =
    composeN n $
    Stream.pipe
        (Pipe.mapM (\x -> return (x + 1)) `Pipe.compose`
         Pipe.mapM (\x -> return (x + 2)))

{-# INLINE transformTeeMapM #-}
transformTeeMapM :: Monad m => Int -> Stream m Int -> m ()
transformTeeMapM n =
    composeN n $
    Stream.pipe
        (Pipe.mapM (\x -> return (x + 1)) `Pipe.teeMerge`
         Pipe.mapM (\x -> return (x + 2)))

pipeMapM :: Int -> IO ()
pipeMapM value = withStream value (transformMapM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'pipeMapM
inspect $ 'pipeMapM `hasNoType` ''S.Step
inspect $ 'pipeMapM `hasNoType` ''S.PipeState
inspect $ 'pipeMapM `hasNoType` ''FL.Step
inspect $ 'pipeMapM `hasNoType` ''SPEC
#endif

pipeCompose :: Int -> IO ()
pipeCompose value = withStream value (transformComposeMapM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'pipeCompose
inspect $ 'pipeCompose `hasNoType` ''S.Step
inspect $ 'pipeCompose `hasNoType` ''S.PipeState
inspect $ 'pipeCompose `hasNoType` ''FL.Step
inspect $ 'pipeCompose `hasNoType` ''SPEC
#endif

pipeTee :: Int -> IO ()
pipeTee value = withStream value (transformTeeMapM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'pipeTee
inspect $ 'pipeTee `hasNoType` ''S.Step
inspect $ 'pipeTee `hasNoType` ''S.PipeState
inspect $ 'pipeTee `hasNoType` ''FL.Step
inspect $ 'pipeTee `hasNoType` ''SPEC
#endif

-- XXX this takes 1 GB memory to compile
-- pipeZip :: Int -> IO ()

pipeMapMX4 :: Int -> IO ()
pipeMapMX4 value = withStream value (transformMapM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'pipeMapMX4
inspect $ 'pipeMapMX4 `hasNoType` ''S.Step
inspect $ 'pipeMapMX4 `hasNoType` ''S.PipeState
inspect $ 'pipeMapMX4 `hasNoType` ''FL.Step
inspect $ 'pipeMapMX4 `hasNoType` ''SPEC
#endif

pipeComposeX4 :: Int -> IO ()
pipeComposeX4 value = withStream value (transformComposeMapM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'pipeComposeX4
inspect $ 'pipeComposeX4 `hasNoType` ''S.Step
inspect $ 'pipeComposeX4 `hasNoType` ''S.PipeState
inspect $ 'pipeComposeX4 `hasNoType` ''FL.Step
inspect $ 'pipeComposeX4 `hasNoType` ''SPEC
#endif

-- XXX requires @-fspec-constr-recursive=16@.
pipeTeeX4 :: Int -> IO ()
pipeTeeX4 value = withStream value (transformTeeMapM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'pipeTeeX4
inspect $ 'pipeTeeX4 `hasNoType` ''S.Step
inspect $ 'pipeTeeX4 `hasNoType` ''S.PipeState
inspect $ 'pipeTeeX4 `hasNoType` ''FL.Step
inspect $ 'pipeTeeX4 `hasNoType` ''SPEC
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
    [ (SpaceO_1, benchIO "mapM" $ pipeMapM value)
    , (SpaceO_1, benchIO "compose" $ pipeCompose value)
    , (SpaceO_1, benchIO "tee" $ pipeTee value)
    , (SpaceO_1, benchIO "mapM x 4" $ pipeMapMX4 value)
    , (SpaceO_1, benchIO "compose x 4" $ pipeComposeX4 value)
    , (SpaceO_1, benchIO "tee x 4" $ pipeTeeX4 value)
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
