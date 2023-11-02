-- |
-- Module      : Stream.Lift
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

#ifdef USE_PRELUDE
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif

module Stream.Lift (benchmarks) where

import Control.DeepSeq (NFData(..))
import Control.Monad.State.Strict (StateT, get, put)
import Data.Functor.Identity (Identity)
import Stream.Common (sourceUnfoldr, sourceUnfoldrM, benchIOSrc)
import System.Random (randomRIO)

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Fold as Fold

#ifdef USE_PRELUDE
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
#else
import Streamly.Internal.Data.Stream (Stream)
import qualified Streamly.Internal.Data.Stream as Stream
#ifdef USE_STREAMK
import Stream.Common (benchIO, drain)
import Streamly.Internal.Data.StreamK (StreamK)
import qualified Streamly.Internal.Data.StreamK as StreamK
import qualified Control.Monad.State.Strict as State
#endif
#endif

import Test.Tasty.Bench
import Streamly.Benchmark.Common
import Prelude hiding (reverse, tail)

#ifdef USE_PRELUDE
type Stream = Stream.SerialT
#endif

-------------------------------------------------------------------------------
-- Monad transformation (hoisting etc.)
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldrState #-}
sourceUnfoldrState :: Common.MonadAsync m =>
    Int -> Int -> Stream (StateT Int m) Int
sourceUnfoldrState value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else do
            s <- get
            put (s + 1)
            return (Just (s, cnt + 1))

{-# INLINE evalStateT #-}
evalStateT :: Common.MonadAsync m => Int -> Int -> Stream m Int
evalStateT value n =
    Stream.evalStateT (return 0) (sourceUnfoldrState value n)

{-# INLINE withState #-}
withState :: Common.MonadAsync m => Int -> Int -> Stream m Int
withState value n =
    Stream.evalStateT
        (return (0 :: Int)) (Stream.liftInner (sourceUnfoldrM value n))

{-# INLINE benchHoistSink #-}
benchHoistSink
    :: (NFData b)
    => Int -> String -> (Stream Identity Int -> IO b) -> Benchmark
benchHoistSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f .  sourceUnfoldr value

o_1_space_hoisting :: Int -> [Benchmark]
o_1_space_hoisting value =
    [ bgroup "hoisting"
        [ benchIOSrc "evalState" (evalStateT value)
        , benchIOSrc "withState" (withState value)
#ifndef USE_PRELUDE
        , benchHoistSink value "generalizeInner"
            ((\xs -> Stream.fold Fold.length xs :: IO Int)
                . Stream.generalizeInner)
#endif
        ]
    ]

#ifdef USE_STREAMK
{-# INLINE iterateStateIO #-}
iterateStateIO ::
       Monad m
    => Int
    -> StateT Int m Int
iterateStateIO n = do
    x <- get
    if x > n
    then do
        put (x - 1)
        iterateStateIO n
    else return x

-- XXX This is basically testing the perf of concatEffect, change it to just
-- use concatEffect and move it along with other concatMap benchmarks.
{-# INLINE iterateStateT #-}
iterateStateT :: Int -> StreamK (StateT Int IO) Int
iterateStateT n = StreamK.concatEffect $ do
    x <- get
    if x > n
    then do
        put (x - 1)
        return $ iterateStateT n
    else return $ StreamK.fromPure x

o_n_heap_transformer :: Int -> [Benchmark]
o_n_heap_transformer value =
    [ bgroup "transformer"
        [ benchIO "StateT Int IO (n times) (baseline)" $ \n ->
            State.evalStateT (iterateStateIO n) value
        , benchIO "Stream (StateT Int IO) (n times)" $ \n ->
            State.evalStateT (drain $ Common.toStream (iterateStateT n)) value
        ]
    ]
#endif

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
benchmarks :: String -> Int -> [Benchmark]
benchmarks moduleName size =
        [ bgroup (o_1_space_prefix moduleName) (o_1_space_hoisting size)
#ifdef USE_STREAMK
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_transformer size)
#endif
        ]
