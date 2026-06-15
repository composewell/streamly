
-- |
-- Module      : Streamly.Benchmark.FileSystem.Handle
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handle.Read
    (allBenchmarks)
where

import System.IO (Handle)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Data.Stream.Prelude as S

import Test.Tasty.Bench hiding (env)
import Prelude hiding (last, length)
import Streamly.Benchmark.Common.Handle

-- TBD reading with unfold

-------------------------------------------------------------------------------
-- unfold read
-------------------------------------------------------------------------------

-- XXX When we mark this with INLINE and we have two benchmarks using S.drain
-- in one benchmark group then somehow GHC ends up delaying the inlining of
-- readDrain. Since S.drain has an INLINE[2] for proper rule firing, that does
-- not work well because of delyaed inlining and the code does not fuse. We
-- need some way of propagating the inline phase information up so that we can
-- expedite inlining of the callers too automatically. The minimal example for
-- the problem can be created by using just two benchmarks in a bench group
-- both using "readDrain". Either GHC should be fixed or we can use
-- fusion-plugin to propagate INLINE phase information such that this problem
-- does not occur.
readDrain :: Handle -> IO ()
readDrain inh = S.fold Fold.drain $ S.unfold FH.reader inh

-- Benchmarks file reading fused with a trivial drain fold, primarily to
-- measure the raw read throughput of FH.reader and verify fusion.
allBenchmarks :: BenchEnv -> [Benchmark]
allBenchmarks env =
    -- read raw bytes without any decoding
    [ mkBench "Fold.drain" env $ \inh _ ->
        readDrain inh
    ]
