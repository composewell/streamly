-- |
-- Module      : Stream.Lift
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Stream.Lift (benchmarks) where

#ifdef USE_PRELUDE
import Control.Monad.Trans.Class (lift)
import Control.Monad.State.Strict (StateT, get, put, MonadState)
import Streamly.Prelude (fromSerial)
import Streamly.Benchmark.Prelude
import qualified Control.Monad.State.Strict as State
import qualified Streamly.Prelude  as Stream
import qualified Streamly.Internal.Data.Stream.IsStream as Internal
#else
import Control.DeepSeq (NFData(..))
import Data.Functor.Identity (Identity)
import Stream.Common (sourceUnfoldr, sourceUnfoldrM, benchIOSrc)
import System.Random (randomRIO)
import qualified Streamly.Internal.Data.Stream as Stream
#endif

import Gauge
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Benchmark.Common

import Prelude hiding (reverse, tail)

-------------------------------------------------------------------------------
-- Monad transformation (hoisting etc.)
-------------------------------------------------------------------------------
#ifdef USE_PRELUDE
{-# INLINE sourceUnfoldrState #-}
sourceUnfoldrState :: (Stream.IsStream t, Stream.MonadAsync m)
                   => Int -> Int -> t (StateT Int m) Int
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
evalStateT :: Stream.MonadAsync m => Int -> Int -> SerialT m Int
evalStateT value n =
    Internal.evalStateT (return 0) (sourceUnfoldrState value n)

{-# INLINE withState #-}
withState :: Stream.MonadAsync m => Int -> Int -> SerialT m Int
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
       (Stream.MonadAsync m)
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
       (Stream.MonadAsync m, MonadState Int m)
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
            State.evalStateT (Stream.drain (iterateStateT n)) value
        , benchIO "MonadState Int m => SerialT m Int" $ \n ->
            State.evalStateT (Stream.drain (iterateState n)) value
        ]
    ]
#else
{-# INLINE benchHoistSink #-}
benchHoistSink
    :: (NFData b)
    => Int -> String -> (SerialT Identity Int -> IO b) -> Benchmark
benchHoistSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f .  sourceUnfoldr value

-- XXX We should be using sourceUnfoldrM for fair comparison with IO monad, but
-- we can't use it as it requires MonadAsync constraint.

{-# INLINE liftInner #-}
liftInner :: Monad m => Int -> Int -> SerialT m Int
liftInner value n =
    Stream.evalStateT
        (return (0 :: Int)) (Stream.liftInner (sourceUnfoldrM value n))

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
    [ bgroup "lift"
        [ benchHoistSink value "length . generally"
            ((\(_ :: SerialT IO Int) -> return 8 :: IO Int) . Stream.generally)

        , benchIOSrc "liftInner/evalStateT" (liftInner value)
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
        [
#ifdef USE_PRELUDE
          bgroup (o_1_space_prefix moduleName) (o_1_space_hoisting size)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_transformer size)
#else
          bgroup (o_1_space_prefix moduleName) (o_1_space_generation size)
#endif
        ]
