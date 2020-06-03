-- |
-- Module      : Streamly.Benchmark.Data.Fold
-- Copyright   : (c) 2018 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.DeepSeq (NFData(..))

import System.Random (randomRIO)

import Gauge

import Prelude hiding (concat)

import qualified Prelude

import Streamly.Benchmark.Common
import Streamly.Benchmark.Data.NestedUnfoldOps

{-# INLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-------------------------------------------------------------------------------
-- Outer product
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Unfold"

o_1_space_serial :: Int -> [Benchmark]
o_1_space_serial value =
    [ bgroup "outer-product"
        [ benchIO "toNull" $ toNull value
        , benchIO "toNull3" $ toNull3 value
        , benchIO "concat" $ concat value
        , benchIO "filterAllOut" $ filterAllOut value
        , benchIO "filterAllIn" $ filterAllIn value
        , benchIO "filterSome" $ filterSome value
        , benchIO "breakAfterSome" $ breakAfterSome value
        ]
    ]

o_n_space_serial :: Int -> [Benchmark]
o_n_space_serial value =
    [ bgroup "outer-product"
        [ benchIO "toList" $ toList value
        , benchIO "toListSome" $ toListSome value
        ]
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    value `seq` runMode (mode cfg) cfg benches (allBenchmarks value)

    where

    allBenchmarks value =
        [ bgroup (o_1_space_prefix moduleName) $ Prelude.concat
            [o_1_space_serial value]
        , bgroup (o_n_space_prefix moduleName) $ Prelude.concat
            [o_n_space_serial value]
        ]
