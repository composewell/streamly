--
-- Module      : Streamly.Unicode.Utf8
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Char (chr)
import Streamly.Internal.Unicode.Utf8 (Utf8)

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common

import qualified Streamly.Internal.Unicode.Utf8 as Utf8

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Unicode.Stream"

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

{-# INLINE pack #-}
pack :: Int -> Utf8
pack i = Utf8.pack $ chr <$> [1 .. i]

{-# INLINE packUnpack #-}
packUnpack :: Int -> String
packUnpack = Utf8.unpack . pack

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks value =
        [ bgroup
              moduleName
              [ bench "pack" $ whnf pack value
              , bench "pack + unpack" $ whnf packUnpack value
              ]
        ]
