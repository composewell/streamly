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
    [ bgroup "serially"
      [ bgroup "generation"
        [ -- Most basic, barely stream continuations running
          benchSrcIO serially "unfoldr" Ops.sourceUnfoldr
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
        [ benchIO "scan" (Ops.scan 1)
        , benchIO "map" (Ops.map 1)
        , benchIO "fmap" (Ops.fmap 1)
        , benchIO "mapM" (Ops.mapM serially 1)
        , benchIO "mapMaybe" (Ops.mapMaybe 1)
        , benchIO "mapMaybeM" (Ops.mapMaybeM 1)
        , bench "sequence" $ nfIO $ randomRIO (1,1000) >>= \n ->
            Ops.sequence serially (Ops.sourceUnfoldrMAction n)
        , benchIO "findIndices" (Ops.findIndices 1)
        , benchIO "elemIndices" (Ops.elemIndices 1)
        -- , benchIO "concat" Ops.concat
        ]
      , bgroup "transformationX4"
        [ benchIO "scan" (Ops.scan 4)
        , benchIO "map" (Ops.map 4)
        , benchIO "fmap" (Ops.fmap 4)
        , benchIO "mapM" (Ops.mapM serially 4)
        , benchIO "mapMaybe" (Ops.mapMaybe 4)
        , benchIO "mapMaybeM" (Ops.mapMaybeM 4)
        -- , bench "sequence" $ nfIO $ randomRIO (1,1000) >>= \n ->
            -- Ops.sequence serially (Ops.sourceUnfoldrMAction n)
        , benchIO "findIndices" (Ops.findIndices 4)
        , benchIO "elemIndices" (Ops.elemIndices 4)
        -- , benchIO "concat" Ops.concat
        ]
      , bgroup "filtering"
        [ benchIO "filter-even"     (Ops.filterEven 1)
        , benchIO "filter-all-out"  (Ops.filterAllOut 1)
        , benchIO "filter-all-in"   (Ops.filterAllIn 1)
        , benchIO "take-all"        (Ops.takeAll 1)
        , benchIO "takeWhile-true"  (Ops.takeWhileTrue 1)
        --, benchIO "takeWhileM-true" (Ops.takeWhileMTrue 1)
        , benchIO "drop-one"        (Ops.dropOne 1)
        , benchIO "drop-all"        (Ops.dropAll 1)
        , benchIO "dropWhile-true"  (Ops.dropWhileTrue 1)
        --, benchIO "dropWhileM-true" (Ops.dropWhileMTrue 1)
        , benchIO "dropWhile-false" (Ops.dropWhileFalse 1)
        ]
      , bgroup "filteringX4"
        [ benchIO "filter-even"     (Ops.filterEven 4)
        , benchIO "filter-all-out"  (Ops.filterAllOut 4)
        , benchIO "filter-all-in"   (Ops.filterAllIn 4)
        , benchIO "take-all"        (Ops.takeAll 4)
        , benchIO "takeWhile-true"  (Ops.takeWhileTrue 4)
        --, benchIO "takeWhileM-true" (Ops.takeWhileMTrue 4)
        , benchIO "drop-one"        (Ops.dropOne 4)
        , benchIO "drop-all"        (Ops.dropAll 4)
        , benchIO "dropWhile-true"  (Ops.dropWhileTrue 4)
        --, benchIO "dropWhileM-true" (Ops.dropWhileMTrue 4)
        , benchIO "dropWhile-false" (Ops.dropWhileFalse 4)
        ]
      , benchIO "zip" Ops.zip
      , benchIO "zipM" Ops.zipM
    , bgroup "mixedX4"
      [ benchIO "scan-map"    (Ops.scanMap 4)
      , benchIO "drop-map"    (Ops.dropMap 4)
      , benchIO "drop-scan"   (Ops.dropScan 4)
      , benchIO "take-drop"   (Ops.takeDrop 4)
      , benchIO "take-scan"   (Ops.takeScan 4)
      , benchIO "take-map"    (Ops.takeMap 4)
      , benchIO "filter-drop" (Ops.filterDrop 4)
      , benchIO "filter-take" (Ops.filterTake 4)
      , benchIO "filter-scan" (Ops.filterScan 4)
      , benchIO "filter-map"  (Ops.filterMap 4)
      ]
    , bgroup "iterated"
      [ benchSrcIO serially "mapM"           Ops.iterateMapM
      , benchSrcIO serially "scan(1/100)"    Ops.iterateScan
      , benchSrcIO serially "filterEven"     Ops.iterateFilterEven
      , benchSrcIO serially "takeAll"        Ops.iterateTakeAll
      , benchSrcIO serially "dropOne"        Ops.iterateDropOne
      , benchSrcIO serially "dropWhileFalse" Ops.iterateDropWhileFalse
      , benchSrcIO serially "dropWhileTrue"  Ops.iterateDropWhileTrue
      ]
      ]
    ]
