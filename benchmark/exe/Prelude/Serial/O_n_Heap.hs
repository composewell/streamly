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

import qualified Streamly.Data.Fold as FL
import qualified Streamly.Memory.Array as A
import qualified Streamly.Prelude as S

import qualified Streamly.Internal.Data.Fold as IFL
import qualified Streamly.Internal.Memory.Array as IA
import qualified Streamly.Internal.Prelude as IP

import Gauge
import Streamly
import Common

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: (IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . Ops.source value

-- XXX We should be using sourceUnfoldrM for fair comparison with IO monad, but
-- we can't use it as it requires MonadAsync constraint.
{-# INLINE benchIdentitySink #-}
benchIdentitySink
    :: (IsStream t, NFData b)
    => Int -> String -> (t Identity Int -> Identity b) -> Benchmark
benchIdentitySink value name f = bench name $ nf (f . Ops.sourceUnfoldr value) 1

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
main :: IO ()
main = do
    -- XXX Fix indentation
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    size <- limitStreamSize value

    size `seq` runMode (mode cfg) cfg benches
    -- Operations using O(1) stack space and O(n) heap space.
    -- Tail recursive left folds
        [ bgroup "foldl"
          [
          -- Left folds for building a structure are inherently non-streaming
          -- as the structure cannot be lazily consumed until fully built.
            benchIOSink size "foldl'/build/IO" Ops.foldl'Build
          , benchIdentitySink size "foldl'/build/Identity" Ops.foldl'Build
          , benchIOSink size "foldlM'/build/IO" Ops.foldlM'Build
          , benchIdentitySink size "foldlM'/build/Identity" Ops.foldlM'Build

          , benchIOSink size "toStream" (S.fold IP.toStream)
          , benchIOSink size "toStreamRev" (S.fold IP.toStreamRev)

          , benchIOSink size "toList" (S.fold FL.toList)
          , benchIOSink size "toListRevF" (S.fold IFL.toListRevF)

          -- Converting the stream to an array
          , benchIOSink size "lastN.Max" (S.fold (IA.lastN (size + 1)))
          , benchIOSink size "writeN" (S.fold (A.writeN size))

          -- Reversing/sorting a stream
          , benchIOSink size "reverse" (Ops.reverse 1)
          , benchIOSink size "reverse'" (Ops.reverse' 1)
          ]
        , bgroup "buffering"
          [
            -- Buffers the output of show/read.
            -- XXX can the outputs be streaming? Can we have special read/show
            -- style type classes, readM/showM supporting streaming effects?
            bench "readsPrec pure streams" $
                nf Ops.readInstance (mkString size)
          , bench "readsPrec Haskell lists" $
                nf Ops.readInstanceList (mkListString size)
          , bench "showPrec Haskell lists" $
                nf Ops.showInstanceList (mkList size)

          -- interleave x/4 streams of 4 elements each. Needs to buffer
          -- proportional to x/4. This is different from WSerial because
          -- WSerial expands slowly because of binary interleave behavior and
          -- this expands immediately because of Nary interleave behavior.
          , benchIOSrc1 "concatUnfoldInterleaveRepl (x/4,4)"
                    (Ops.concatUnfoldInterleaveRepl4xN size)
          , benchIOSrc1 "concatUnfoldRoundrobinRepl (x/4,4)"
                    (Ops.concatUnfoldRoundrobinRepl4xN size)
          ]
        ]
