-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

-- Rate benchmarks are kept separate because they need more running time to
-- provide stable results.

-- import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)
import qualified Streamly.Benchmark.Prelude as Ops

import Streamly
import Gauge

-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchSrcIO #-}
benchSrcIO
    :: (t IO Int -> SerialT IO Int)
    -> String
    -> (Int -> t IO Int)
    -> Benchmark
benchSrcIO t name f
    = bench name $ nfIO $ randomRIO (1,1) >>= Ops.toNull t . f

{-
_benchId :: NFData b => String -> (Ops.Stream m Int -> Identity b) -> Benchmark
_benchId name f = bench name $ nf (runIdentity . f) (Ops.source 10)
-}

main :: IO ()
main =
  defaultMain
    -- XXX arbitrarily large rate should be the same as rate Nothing
    [ bgroup "avgrate"
      [ bgroup "asyncly"
        [ -- benchIO "unfoldr" $ Ops.toNull asyncly
          benchSrcIO asyncly "unfoldrM" Ops.sourceUnfoldrM
        , benchSrcIO asyncly "unfoldrM/Nothing"
            (rate Nothing . Ops.sourceUnfoldrM)
        , benchSrcIO asyncly "unfoldrM/1,000,000"
            (avgRate 1000000 . Ops.sourceUnfoldrM)
        , benchSrcIO asyncly "unfoldrM/3,000,000"
            (avgRate 3000000 . Ops.sourceUnfoldrM)
        , benchSrcIO asyncly "unfoldrM/10,000,000/maxThreads1"
            (maxThreads 1 . avgRate 10000000 . Ops.sourceUnfoldrM)
        , benchSrcIO asyncly "unfoldrM/10,000,000"
            (avgRate 10000000 . Ops.sourceUnfoldrM)
        , benchSrcIO asyncly "unfoldrM/20,000,000"
            (avgRate 20000000 . Ops.sourceUnfoldrM)
        ]
      , bgroup "aheadly"
        [
          benchSrcIO aheadly "unfoldrM/1,000,000"
            (avgRate 1000000 . Ops.sourceUnfoldrM)
        ]
      ]
    ]
