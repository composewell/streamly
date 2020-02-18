-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Control.DeepSeq (NFData(..))
import Data.Functor.Identity (Identity)
import System.Random (randomRIO)

import qualified Streamly.Benchmark.Prelude as Ops

import qualified NestedOps as Nested
import qualified NestedUnfoldOps as NestedUnfold

import Streamly
import Gauge
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Benchmarking utilities
-------------------------------------------------------------------------------

-- XXX we should replace these with the ones in the Common module. We should
-- not pass the generation or drain function to he benchmarking function. If we
-- generate and terminate the whole pipeline in the benchmarked op itself we
-- can use inspection tests on it.
--
-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: (IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f = bench name $ nfIO $ randomRIO (1,1) >>= f . Ops.source value

-- XXX We should be using sourceUnfoldrM for fair comparison with IO monad, but
-- we can't use it as it requires MonadAsync constraint.
{-# INLINE benchIdentitySink #-}
benchIdentitySink
    :: (IsStream t, NFData b)
    => Int -> String -> (t Identity Int -> Identity b) -> Benchmark
benchIdentitySink value name f = bench name $ nf (f . Ops.sourceUnfoldr value) 1

{-# INLINE benchPureSinkIO #-}
benchPureSinkIO
    :: NFData b
    => Int -> String -> (SerialT Identity Int -> IO b) -> Benchmark
benchPureSinkIO value name f =
    bench name $ nfIO $ randomRIO (1, 1) >>= f . Ops.sourceUnfoldr value

benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    size <- limitStreamSize value

    size `seq` runMode (mode cfg) cfg benches
        -- Both heap and stack space are O(n)
        [ bgroup "toList" -- < 2MB
          [
          -- Converting the stream to a list or pure stream in a strict monad
            benchIOSink size "foldrMToList" Ops.foldrMBuild
          , benchIOSink size "toList" Ops.toList
          , benchIOSink size "toListRev" Ops.toListRev
          -- , benchIOSink size "toPure" Ops.toPure
          -- , benchIOSink size "toPureRev" Ops.toPureRev
          ]
        , bgroup "outer-product-streams"
          [ benchIO "toList"         $ Nested.toList size         serially
          , benchIO "toListSome"     $ Nested.toListSome size     serially
          ]
        , bgroup "outer-product-unfolds"
          [ benchIO "toList"         $ NestedUnfold.toList size
          , benchIO "toListSome"     $ NestedUnfold.toListSome size
          ]
        , bgroup "outer-product-wserial"
          [ benchIO "toList"         $ Nested.toList size         wSerially
          , benchIO "toListSome"     $ Nested.toListSome  size    wSerially
          ]
        -- Buffering operations using heap proportional to number of elements.
        , bgroup "traversable" -- < 2MB
          [ -- Traversable instance
            benchPureSinkIO size "traverse" Ops.traversableTraverse
          , benchPureSinkIO size "sequenceA" Ops.traversableSequenceA
          , benchPureSinkIO size "mapM" Ops.traversableMapM
          , benchPureSinkIO size "sequence" Ops.traversableSequence
          ]
        -- Head recursive strict right folds.
        , bgroup "foldr"
          [ -- < 2MB
          -- accumulation due to strictness of IO monad
            benchIOSink size "foldrM/build/IO" Ops.foldrMBuild
          -- Right folds for reducing are inherently non-streaming as the
          -- expression needs to be fully built before it can be reduced.
          , benchIdentitySink size "foldrM/reduce/Identity" Ops.foldrMReduce

          -- takes < 4MB
          , benchIOSink size "foldrM/reduce/IO" Ops.foldrMReduce

          -- XXX the definitions of minimumBy and maximumBy in Data.Foldable use
          -- foldl1 which does not work in constant memory for our implementation.
          -- It works in constant memory for lists but even for lists it takes 15x
          -- more time compared to our foldl' based implementation.
          -- XXX these take < 16M stack space
          , bench "minimumBy" $ nf (flip Ops.foldableMinBy 1) size
          , bench "maximumBy" $ nf (flip Ops.foldableMaxBy 1) size
          , bench "minimumByList" $ nf (flip Ops.foldableListMinBy 1) size
          ]
        ]
