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

import Control.DeepSeq (NFData(..))
import Control.Monad.State.Strict (StateT, get, put)
import Data.Functor.Identity (Identity)
import Stream.Common (sourceUnfoldr, sourceUnfoldrM, benchIOSrc)
import System.Random (randomRIO)
import Streamly.Internal.Data.Stream (Stream)

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
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
        , benchHoistSink value "generalizeInner"
            ((\xs -> Stream.fold Fold.length xs :: IO Int)
                . Stream.generalizeInner)
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
        ]
