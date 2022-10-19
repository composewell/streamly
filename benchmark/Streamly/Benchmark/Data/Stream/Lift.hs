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
import Control.Monad.Trans.Class (lift)
import Control.Monad.State.Strict (StateT, get, put, MonadState)
import Data.Functor.Identity (Identity)
import Stream.Common
    (benchIO, sourceUnfoldr, sourceUnfoldrM, benchIOSrc, drain)
import System.Random (randomRIO)
#ifdef USE_PRELUDE
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
#else
import qualified Streamly.Internal.Data.Stream as Stream
#endif
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Stream.Common as Common
import qualified Control.Monad.State.Strict as State

import Gauge
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Benchmark.Common

import Prelude hiding (reverse, tail)

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
        , benchHoistSink value "generally"
            ((\xs -> Stream.fold Fold.length xs :: IO Int) . Stream.generally)
        ]
    ]

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

{-# INLINE iterateStateT #-}
iterateStateT :: Int -> Stream (StateT Int IO) Int
iterateStateT n = do
    x <- lift get
    if x > n
    then do
        lift $ put (x - 1)
        iterateStateT n
    else return x

{-# INLINE iterateState #-}
{-# SPECIALIZE iterateState :: Int -> Stream (StateT Int IO) Int #-}
iterateState ::
       MonadState Int m
    => Int
    -> Stream m Int
iterateState n = do
    x <- get
    if x > n
    then do
        put (x - 1)
        iterateState n
    else return x

o_n_heap_transformer :: Int -> [Benchmark]
o_n_heap_transformer value =
    [ bgroup "transformer"
        [ benchIO "StateT Int IO (n times) (baseline)" $ \n ->
            State.evalStateT (iterateStateIO n) value
        , benchIO "Stream (StateT Int IO) (n times)" $ \n ->
            State.evalStateT (drain (iterateStateT n)) value
        , benchIO "MonadState Int m => Stream m Int" $ \n ->
            State.evalStateT (drain (iterateState n)) value
        ]
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
benchmarks :: String -> Int -> [Benchmark]
benchmarks moduleName size =
        [ bgroup (o_1_space_prefix moduleName) (o_1_space_hoisting size)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_transformer size)
        ]
