-- |
-- Module      : Serial.Lift
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Serial.Lift (benchmarks) where

import Control.Monad.State.Strict (StateT, get, put, MonadState)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Class (lift)

import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Stream.IsStream as Internal

import Gauge
import Streamly.Prelude (fromSerial)
import Streamly.Internal.Data.Stream.Serial.Type (SerialT)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude
import Prelude hiding (reverse, tail)

-------------------------------------------------------------------------------
-- Monad transformation (hoisting etc.)
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldrState #-}
sourceUnfoldrState :: (S.IsStream t, S.MonadAsync m)
                   => Int -> Int -> t (StateT Int m) Int
sourceUnfoldrState value n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else do
            s <- get
            put (s + 1)
            return (Just (s, cnt + 1))

{-# INLINE evalStateT #-}
evalStateT :: S.MonadAsync m => Int -> Int -> SerialT m Int
evalStateT value n =
    Internal.evalStateT (return 0) (sourceUnfoldrState value n)

{-# INLINE withState #-}
withState :: S.MonadAsync m => Int -> Int -> SerialT m Int
withState value n =
    Internal.evalStateT
        (return (0 :: Int)) (Internal.liftInner (sourceUnfoldrM value n))

o_1_space_hoisting :: Int -> [Benchmark]
o_1_space_hoisting value =
    [ bgroup "hoisting"
        [ benchIOSrc fromSerial "evalState" (evalStateT value)
        , benchIOSrc fromSerial "withState" (withState value)
        ]
    ]

{-# INLINE iterateStateIO #-}
iterateStateIO ::
       (S.MonadAsync m)
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
iterateStateT :: Int -> SerialT (StateT Int IO) Int
iterateStateT n = do
    x <- lift get
    if x > n
    then do
        lift $ put (x - 1)
        iterateStateT n
    else return x

{-# INLINE iterateState #-}
{-# SPECIALIZE iterateState :: Int -> SerialT (StateT Int IO) Int #-}
iterateState ::
       (S.MonadAsync m, MonadState Int m)
    => Int
    -> SerialT m Int
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
        , benchIO "SerialT (StateT Int IO) (n times)" $ \n ->
            State.evalStateT (S.drain (iterateStateT n)) value
        , benchIO "MonadState Int m => SerialT m Int" $ \n ->
            State.evalStateT (S.drain (iterateState n)) value
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
