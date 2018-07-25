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
benchIO name f = bench name $ nfIO $ randomRIO (1,1000) >>= f . Ops.source

-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchSrcIO #-}
benchSrcIO
    :: (t IO Int -> SerialT IO Int)
    -> String
    -> (Int -> t IO Int)
    -> Benchmark
benchSrcIO t name f
    = bench name $ nfIO $ randomRIO (1,1000) >>= Ops.toNull t . f

{-
_benchId :: NFData b => String -> (Ops.Stream m Int -> Identity b) -> Benchmark
_benchId name f = bench name $ nf (runIdentity . f) (Ops.source 10)
-}

main :: IO ()
main = do
  defaultMain
    [ bgroup "serially"
      [ bgroup "generation"
        [ -- Most basic, barely stream continuations running
          benchSrcIO serially "unfoldr" $ Ops.sourceUnfoldr
        , benchSrcIO serially "unfoldrM" Ops.sourceUnfoldrM
        , benchSrcIO serially "fromList" Ops.sourceFromList
        , benchSrcIO serially "fromListM" Ops.sourceFromListM
        -- These are essentially cons and consM
        , benchSrcIO serially "fromFoldable" Ops.sourceFromFoldable
        , benchSrcIO serially "fromFoldableM" Ops.sourceFromFoldableM
        -- These are essentially appends
        , benchSrcIO serially "foldMapWith" Ops.sourceFoldMapWith
        , benchSrcIO serially "foldMapWithM" Ops.sourceFoldMapWithM
        ]
      , bgroup "elimination"
        [ benchIO "toNull" $ Ops.toNull serially
        , benchIO "uncons" Ops.uncons
        , benchIO "init" Ops.init
        , benchIO "tail" Ops.tail
        , benchIO "nullHeadTail" Ops.nullHeadTail
        , benchIO "mapM_" Ops.mapM_
        , benchIO "toList" Ops.toList
        , benchIO "foldr" Ops.foldr
        , benchIO "foldr1" Ops.foldr1
        , benchIO "foldrM" Ops.foldrM
        , benchIO "foldl'" Ops.foldl'
        , benchIO "foldl1'" Ops.foldl1'

        , benchIO "last" Ops.last
        , benchIO "length" Ops.length
        , benchIO "elem" Ops.elem
        , benchIO "notElem" Ops.notElem
        , benchIO "all" Ops.all
        , benchIO "any" Ops.any
        , benchIO "and" Ops.and
        , benchIO "or" Ops.or
        , benchIO "find" Ops.find
        , benchIO "findIndex" Ops.findIndex
        , benchIO "elemIndex" Ops.elemIndex
        , benchIO "maximum" Ops.maximum
        , benchIO "minimum" Ops.minimum
        , benchIO "sum" Ops.sum
        , benchIO "product" Ops.product
        ]
      , bgroup "transformation"
        [ benchIO "scan" Ops.scan
        , benchIO "map" Ops.map
        , benchIO "fmap" Ops.fmap
        , benchIO "mapM" (Ops.mapM serially)
        , benchIO "mapMaybe" Ops.mapMaybe
        , benchIO "mapMaybeM" Ops.mapMaybeM
        , bench "sequence" $ nfIO $ randomRIO (1,1000) >>= \n ->
            (Ops.sequence serially) (Ops.sourceUnfoldrMAction n)
        , benchIO "findIndices" Ops.findIndices
        , benchIO "elemIndices" Ops.elemIndices
        , benchIO "concat" Ops.concat
        ]
      , bgroup "filtering"
        [ benchIO "filter-even" Ops.filterEven
        , benchIO "filter-all-out" Ops.filterAllOut
        , benchIO "filter-all-in" Ops.filterAllIn
        , benchIO "take-all" Ops.takeAll
        , benchIO "takeWhile-true" Ops.takeWhileTrue
        , benchIO "takeWhileM-true" Ops.takeWhileMTrue
        , benchIO "drop-all" Ops.dropAll
        , benchIO "dropWhile-true" Ops.dropWhileTrue
        , benchIO "dropWhileM-true" Ops.dropWhileMTrue
        ]
      , benchIO "zip" $ Ops.zip
      , benchIO "zipM" $ Ops.zipM
      , bgroup "compose"
        [ benchIO "mapM" Ops.composeMapM
        , benchIO "map-with-all-in-filter" Ops.composeMapAllInFilter
        , benchIO "all-in-filters" Ops.composeAllInFilters
        , benchIO "all-out-filters" Ops.composeAllOutFilters
        ]
        -- Scaling with same operation in sequence
      , bgroup "compose-scaling"
        [ benchIO "1" $ Ops.composeScaling 1
        , benchIO "2" $ Ops.composeScaling 2
        , benchIO "3" $ Ops.composeScaling 3
        , benchIO "4" $ Ops.composeScaling 4
        ]
      ]
      , bgroup "asyncly"
        [ -- benchIO "unfoldr" $ Ops.toNull asyncly
          benchSrcIO asyncly "unfoldrM" Ops.sourceUnfoldrM
        -- , benchSrcIO asyncly "fromFoldable" Ops.sourceFromFoldable
        , benchSrcIO asyncly "fromFoldableM" Ops.sourceFromFoldableM
        -- , benchSrcIO asyncly "foldMapWith" Ops.sourceFoldMapWith
        , benchSrcIO asyncly "foldMapWithM" Ops.sourceFoldMapWithM
        , benchIO "mapM"   $ Ops.mapM asyncly
        ]
      , bgroup "wAsyncly"
        [ -- benchIO "unfoldr" $ Ops.toNull wAsyncly
          benchSrcIO wAsyncly "unfoldrM" Ops.sourceUnfoldrM
        -- , benchSrcIO wAsyncly "fromFoldable" Ops.sourceFromFoldable
        , benchSrcIO wAsyncly "fromFoldableM" Ops.sourceFromFoldableM
        -- , benchSrcIO wAsyncly "foldMapWith" Ops.sourceFoldMapWith
        , benchSrcIO wAsyncly "foldMapWithM" Ops.sourceFoldMapWithM
        , benchIO "mapM"   $ Ops.mapM wAsyncly
        ]
      -- unfoldr and fromFoldable are always serial and thereofore the same for
      -- all stream types.
      , bgroup "aheadly"
        [ -- benchIO "unfoldr" $ Ops.toNull aheadly
          benchSrcIO aheadly "unfoldrM" Ops.sourceUnfoldrM
        -- , benchSrcIO aheadly "fromFoldable" Ops.sourceFromFoldable
        , benchSrcIO aheadly "fromFoldableM" Ops.sourceFromFoldableM
        -- , benchSrcIO aheadly "foldMapWith" Ops.sourceFoldMapWith
        , benchSrcIO aheadly "foldMapWithM" Ops.sourceFoldMapWithM
        , benchIO       "mapM"  $ Ops.mapM aheadly
        ]
     -- XXX need to use smaller streams to finish in reasonable time
      , bgroup "parallely"
        [ --benchIO "unfoldr" $ Ops.toNull parallely
          benchSrcIO parallely "unfoldrM" Ops.sourceUnfoldrM
        --, benchSrcIO parallely "fromFoldable" Ops.sourceFromFoldable
        , benchSrcIO parallely "fromFoldableM" Ops.sourceFromFoldableM
        -- , benchSrcIO parallely "foldMapWith" Ops.sourceFoldMapWith
        , benchSrcIO parallely "foldMapWithM" Ops.sourceFoldMapWithM
        , benchIO "mapM" $ Ops.mapM parallely
        -- Zip has only one parallel flavor
        , benchIO "zip" $ Ops.zipAsync
        , benchIO "zipM" $ Ops.zipAsyncM
        ]
      ]
