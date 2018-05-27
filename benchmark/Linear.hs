-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

import Control.DeepSeq (NFData)
import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)
import qualified LinearOps as Ops

import Streamly
import Gauge

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.
benchIO :: (IsStream t, NFData b) => String -> (t IO Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1000) >>= f . Ops.source

benchSrcIO
    :: (t IO Int -> SerialT IO Int)
    -> String
    -> (Int -> t IO Int)
    -> Benchmark
benchSrcIO t name f
    = bench name $ nfIO $ randomRIO (1,1000) >>= Ops.toNull t . f

benchIOAppend :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIOAppend name f = bench name $ nfIO $ randomRIO (1,1000) >>= f

_benchId :: NFData b => String -> (Ops.Stream m Int -> Identity b) -> Benchmark
_benchId name f = bench name $ nf (runIdentity . f) (Ops.source 10)

main :: IO ()
main = do
  defaultMain
    [ bgroup "generation"
      [ bgroup "serially"
          [ benchIO "unfoldr" $ Ops.toNull serially
          , benchSrcIO serially "fromFoldable" Ops.sourceFromFoldable
          , benchSrcIO serially "foldMapWith" Ops.sourceFoldMapWith
          , benchSrcIO serially "unfoldrM" Ops.sourceUnfoldrM
          , benchSrcIO serially "fromFoldableM" Ops.sourceFromFoldableM
          , benchSrcIO serially "foldMapWithM" Ops.sourceFoldMapWithM
          ]
      -- unfoldr and fromFoldable are always serial and thereofore the same for
      -- all stream types.
      , bgroup "aheadly"
          [ -- benchIO "unfoldr" $ Ops.toNull aheadly
          -- , benchSrcIO aheadly "fromFoldable" Ops.sourceFromFoldable
            benchSrcIO aheadly "foldMapWith" Ops.sourceFoldMapWith
          , benchSrcIO aheadly "unfoldrM" Ops.sourceUnfoldrM
          , benchSrcIO aheadly "fromFoldableM" Ops.sourceFromFoldableM
          , benchSrcIO aheadly "foldMapWithM" Ops.sourceFoldMapWithM
          ]
      , bgroup "asyncly"
          [ -- benchIO "unfoldr" $ Ops.toNull asyncly
          -- , benchSrcIO asyncly "fromFoldable" Ops.sourceFromFoldable
            benchSrcIO asyncly "foldMapWith" Ops.sourceFoldMapWith
          , benchSrcIO asyncly "unfoldrM" Ops.sourceUnfoldrM
          , benchSrcIO asyncly "fromFoldableM" Ops.sourceFromFoldableM
          , benchSrcIO asyncly "foldMapWithM" Ops.sourceFoldMapWithM
          ]
     -- XXX need to use smaller streams to finish in reasonable time
      , bgroup "parallely"
          [ --benchIO "unfoldr" $ Ops.toNull parallely
          --, benchSrcIO parallely "fromFoldable" Ops.sourceFromFoldable
          --, benchSrcIO parallely "foldMapWith" Ops.sourceFoldMapWith
          -- , benchSrcIO parallely "unfoldrM" Ops.sourceUnfoldrM
          -- , benchSrcIO parallely "fromFoldableM" Ops.sourceFromFoldableM
          -- , benchSrcIO parallely "foldMapWithM" Ops.sourceFoldMapWithM
          ]
      ]
    -- XXX this is the same foldMapWith in the above benchmarks, need to dedup
    -- this one provides a single place comparsion of all types and uses
    -- smaller stream size for faster benchmarking. We can use the same for
    -- above benchmarks and remove this.
    , bgroup "append"
      [ benchIOAppend "serially"   $ Ops.append serially
      , benchIOAppend "wSerially"  $ Ops.append wSerially
      , benchIOAppend "aheadly"    $ Ops.append aheadly
      , benchIOAppend "asyncly"    $ Ops.append asyncly
      , benchIOAppend "wAsyncly"   $ Ops.append wAsyncly
      , benchIOAppend "parallely"  $ Ops.append parallely
      ]
    , bgroup "elimination"
      [ benchIO "toNull" $ Ops.toNull serially
      , benchIO "toList" Ops.toList
      , benchIO "fold" Ops.foldl
      , benchIO "last" Ops.last
      ]
    , bgroup "transformation"
      [ benchIO "scan" Ops.scan
      , benchIO "map" Ops.map
      , benchIO "mapM" Ops.mapM
      , benchIO "concat" Ops.concat
      ]
    , bgroup "filtering"
      [ benchIO "filter-even" Ops.filterEven
      , benchIO "filter-all-out" Ops.filterAllOut
      , benchIO "filter-all-in" Ops.filterAllIn
      , benchIO "take-all" Ops.takeAll
      , benchIO "takeWhile-true" Ops.takeWhileTrue
      , benchIO "drop-all" Ops.dropAll
      , benchIO "dropWhile-true" Ops.dropWhileTrue
      ]
    , benchIO "zip" Ops.zip
    , bgroup "compose"
      [ benchIO "mapM" Ops.composeMapM
      , benchIO "map-with-all-in-filter" Ops.composeMapAllInFilter
      , benchIO "all-in-filters" Ops.composeAllInFilters
      , benchIO "all-out-filters" Ops.composeAllOutFilters
      ]
    , bgroup "compose-scaling"
      -- Scaling with same operation in sequence
      [ benchIO "1" $ Ops.composeScaling 1
      , benchIO "2" $ Ops.composeScaling 2
      , benchIO "3" $ Ops.composeScaling 3
      , benchIO "4" $ Ops.composeScaling 4
      ]
   ]
