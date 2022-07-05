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
import Data.Functor.Identity (Identity)
import System.Random (randomRIO)

import Stream.Common (sourceUnfoldr, sourceUnfoldrM, benchIOSrc)
import Streamly.Internal.Data.Stream.Type ( Stream )
import qualified Streamly.Internal.Data.Stream.Lift as Stream
    ( generally, liftInner, evalStateT )

import Prelude hiding (length, sum, or, and, any, all, notElem, elem, (!!),
    lookup, repeat, minimum, maximum, product, last, mapM_, init)

import Gauge
import Streamly.Benchmark.Common

{-# INLINE benchHoistSink #-}
benchHoistSink
    :: (NFData b)
    => Int -> String -> (Stream Identity Int -> IO b) -> Benchmark
benchHoistSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f .  sourceUnfoldr value

-- XXX We should be using sourceUnfoldrM for fair comparison with IO monad, but
-- we can't use it as it requires MonadAsync constraint.

{-# INLINE liftInner #-}
liftInner :: Monad m => Int -> Int -> Stream m Int
liftInner value n =
    Stream.evalStateT
        (return (0 :: Int)) (Stream.liftInner (sourceUnfoldrM value n))

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
    [ bgroup "lift"
        [ benchHoistSink value "length . generally"
            ((\(_ :: Stream IO Int) -> return 8 :: IO Int) . Stream.generally)

        , benchIOSrc "liftInner/evalStateT" (liftInner value)
        ]
    ]

benchmarks :: String -> Int -> [Benchmark]
benchmarks moduleName size =
    [ bgroup (o_1_space_prefix moduleName) (o_1_space_generation size)
    ]
