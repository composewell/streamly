-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

import Control.DeepSeq (NFData)
-- import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)
import qualified LinearOps as Ops

import Streamly
import Gauge

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.
--
-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIO #-}
benchIO :: (IsStream t, NFData b) => String -> (t IO Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f . Ops.source

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
    [ bgroup "asyncly"
        [ -- benchIO "unfoldr" $ Ops.toNull asyncly
          benchSrcIO asyncly "unfoldrM" Ops.sourceUnfoldrM
        -- , benchSrcIO asyncly "fromFoldable" Ops.sourceFromFoldable
        , benchSrcIO asyncly "fromFoldableM" Ops.sourceFromFoldableM
        -- , benchSrcIO asyncly "foldMapWith" Ops.sourceFoldMapWith
        , benchSrcIO asyncly "foldMapWithM" Ops.sourceFoldMapWithM
        , benchIO "mapM"   $ Ops.mapM asyncly 1
        , benchSrcIO asyncly "unfoldrM maxThreads 1"
            (maxThreads 1 . Ops.sourceUnfoldrM)
        , benchSrcIO asyncly "unfoldrM maxBuffer 1 (1000 ops)"
            (maxBuffer 1 . Ops.sourceUnfoldrMN 1000)
        ]
      , bgroup "wAsyncly"
        [ -- benchIO "unfoldr" $ Ops.toNull wAsyncly
          benchSrcIO wAsyncly "unfoldrM" Ops.sourceUnfoldrM
        -- , benchSrcIO wAsyncly "fromFoldable" Ops.sourceFromFoldable
        , benchSrcIO wAsyncly "fromFoldableM" Ops.sourceFromFoldableM
        -- , benchSrcIO wAsyncly "foldMapWith" Ops.sourceFoldMapWith
        , benchSrcIO wAsyncly "foldMapWithM" Ops.sourceFoldMapWithM
        , benchIO "mapM"   $ Ops.mapM wAsyncly 1
        ]
      -- unfoldr and fromFoldable are always serial and thereofore the same for
      -- all stream types.
      , bgroup "aheadly"
        [ -- benchIO "unfoldr" $ Ops.toNull aheadly
          benchSrcIO aheadly "unfoldrM" Ops.sourceUnfoldrM
        , benchSrcIO aheadly "fromFoldableM" Ops.sourceFromFoldableM
        -- , benchSrcIO aheadly "foldMapWith" Ops.sourceFoldMapWith
        , benchSrcIO aheadly "foldMapWithM" Ops.sourceFoldMapWithM
        , benchIO       "mapM"  $ Ops.mapM aheadly 1
        , benchSrcIO aheadly "unfoldrM maxThreads 1"
            (maxThreads 1 . Ops.sourceUnfoldrM)
        , benchSrcIO aheadly "unfoldrM maxBuffer 1 (1000 ops)"
            (maxBuffer 1 . Ops.sourceUnfoldrMN 1000)
        -- , benchSrcIO aheadly "fromFoldable" Ops.sourceFromFoldable
        ]
     -- XXX need to use smaller streams to finish in reasonable time
      , bgroup "parallely"
        [ --benchIO "unfoldr" $ Ops.toNull parallely
          benchSrcIO parallely "unfoldrM" Ops.sourceUnfoldrM
        --, benchSrcIO parallely "fromFoldable" Ops.sourceFromFoldable
        , benchSrcIO parallely "fromFoldableM" Ops.sourceFromFoldableM
        -- , benchSrcIO parallely "foldMapWith" Ops.sourceFoldMapWith
        , benchSrcIO parallely "foldMapWithM" Ops.sourceFoldMapWithM
        , benchIO "mapM" $ Ops.mapM parallely 1
        -- Zip has only one parallel flavor
        , benchIO "zip" Ops.zipAsync
        , benchIO "zipM" Ops.zipAsyncM
        ]
      ]
