-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Control.DeepSeq (NFData(..))
import System.Random (randomRIO)

import Common -- (parseCLIOpts)

import qualified Streamly.Benchmark.Prelude as Ops

import Streamly
import Gauge

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
benchIOSink value name f = bench name $ nfIO $ randomRIO (1,1) >>= f . Ops.source value

-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchIOSrc #-}
benchIOSrc
    :: (t IO a -> SerialT IO a)
    -> String
    -> (Int -> t IO a)
    -> Benchmark
benchIOSrc t name f =
    bench name $ nfIO $ randomRIO (1,1) >>= Ops.toNull t . f

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    size <- limitStreamSize value

    -- Operations using O(n) stack space but O(1) heap space.
    size `seq` runMode (mode cfg) cfg benches
        -- Head recursive operations.
        [ bgroup "iterated"
          [ benchIOSrc serially "mapMx10K"           Ops.iterateMapM
          , benchIOSrc serially "scanx100"           Ops.iterateScan
          , benchIOSrc serially "scanl1x100"         Ops.iterateScanl1
          , benchIOSrc serially "filterEvenx10K"     Ops.iterateFilterEven
          , benchIOSrc serially "takeAllx10K"        (Ops.iterateTakeAll size)
          , benchIOSrc serially "dropOnex10K"        Ops.iterateDropOne
          , benchIOSrc serially "dropWhileFalsex10K"
                (Ops.iterateDropWhileFalse size)
          , benchIOSrc serially "dropWhileTruex10K"
                (Ops.iterateDropWhileTrue size)

          , benchIOSink size "tail" Ops.tail
          , benchIOSink size "nullHeadTail" Ops.nullHeadTail
          ]
        ]
