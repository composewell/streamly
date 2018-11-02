-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE CPP                       #-}

import Control.DeepSeq (NFData)
-- import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)

import Gauge
import qualified StreamDOps as D
import qualified StreamKOps as K

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.
{-# INLINE benchIO #-}
benchIO :: String -> (a IO Int -> IO ()) -> (Int -> a IO Int) -> Benchmark
benchIO name run f = bench name $ nfIO $ randomRIO (1,1000) >>= run . f

benchFold :: NFData b
    => String -> (t IO Int -> IO b) -> (Int -> t IO Int) -> Benchmark
benchFold name f src = bench name $ nfIO $ randomRIO (1,1000) >>= f . src

{-
_benchId :: NFData b => String -> (Ops.Stream m Int -> Identity b) -> Benchmark
_benchId name f = bench name $ nf (runIdentity . f) (Ops.source 10)
-}

main :: IO ()
main =
  defaultMain
    [ bgroup "streamD"
      [ bgroup "generation"
        [ benchIO "unfoldr"      D.toNull D.sourceUnfoldr
        , benchIO "unfoldrM"     D.toNull D.sourceUnfoldrM
        , benchIO "fromEnum"     D.toNull D.sourceFromEnum

        , benchIO "fromList" D.toNull D.sourceFromList
        -- , benchIO "fromFoldableM" D.sourceFromFoldableM
        ]
      , bgroup "elimination"
        [ benchIO "toNull"   D.toNull D.sourceUnfoldrM
        , benchIO "uncons"   D.uncons D.sourceUnfoldrM
        , benchFold "tail"   D.tail   D.sourceUnfoldrM
        , benchIO "nullTail" D.nullTail D.sourceUnfoldrM
        , benchIO "headTail" D.headTail D.sourceUnfoldrM
        , benchFold "toList" K.toList K.sourceUnfoldrM
        , benchFold "fold"   K.foldl  K.sourceUnfoldrM
        , benchFold "last"   K.last   K.sourceUnfoldrM
        ]
      , bgroup "transformation"
        [ benchIO "scan"      (D.scan      1) D.sourceUnfoldrM
        , benchIO "map"       (D.map       1) D.sourceUnfoldrM
        , benchIO "fmap"      (D.fmap      1) D.sourceUnfoldrM
        , benchIO "mapM"      (D.mapM      1) D.sourceUnfoldrM
        , benchIO "mapMaybe"  (D.mapMaybe  1) D.sourceUnfoldrM
        , benchIO "mapMaybeM" (D.mapMaybeM 1) D.sourceUnfoldrM
        ]
      , bgroup "transformationN"
        [ benchIO "scan"      (D.scan      4) D.sourceUnfoldrM
        , benchIO "map"       (D.map       4) D.sourceUnfoldrM
        , benchIO "fmap"      (D.fmap      4) D.sourceUnfoldrM
        , benchIO "mapM"      (D.mapM      4) D.sourceUnfoldrM
        , benchIO "mapMaybe"  (D.mapMaybe  4) D.sourceUnfoldrM
        , benchIO "mapMaybeM" (D.mapMaybeM 4) D.sourceUnfoldrM
        ]
      , bgroup "filtering"
        [ benchIO "filter-even"     (D.filterEven     1) D.sourceUnfoldrM
        , benchIO "filter-all-out"  (D.filterAllOut   1) D.sourceUnfoldrM
        , benchIO "filter-all-in"   (D.filterAllIn    1) D.sourceUnfoldrM
        , benchIO "take-all"        (D.takeAll        1) D.sourceUnfoldrM
        , benchIO "takeWhile-true"  (D.takeWhileTrue  1) D.sourceUnfoldrM
        , benchIO "drop-one"        (D.dropOne        1) D.sourceUnfoldrM
        , benchIO "drop-all"        (D.dropAll        1) D.sourceUnfoldrM
        , benchIO "dropWhile-true"  (D.dropWhileTrue  1) D.sourceUnfoldrM
        , benchIO "dropWhile-false" (D.dropWhileFalse 1) D.sourceUnfoldrM
        ]
      , bgroup "filteringN"
        [ benchIO "filter-even"     (D.filterEven     4) D.sourceUnfoldrM
        , benchIO "filter-all-out"  (D.filterAllOut   4) D.sourceUnfoldrM
        , benchIO "filter-all-in"   (D.filterAllIn    4) D.sourceUnfoldrM
        , benchIO "take-all"        (D.takeAll        4) D.sourceUnfoldrM
        , benchIO "takeWhile-true"  (D.takeWhileTrue  4) D.sourceUnfoldrM
        , benchIO "drop-one"        (D.dropOne        4) D.sourceUnfoldrM
        , benchIO "drop-all"        (D.dropAll        4) D.sourceUnfoldrM
        , benchIO "dropWhile-true"  (D.dropWhileTrue  4) D.sourceUnfoldrM
        , benchIO "dropWhile-false" (D.dropWhileFalse 4) D.sourceUnfoldrM
        ]
      , benchIO "zip" D.zip D.sourceUnfoldrM
      , bgroup "composed"
        [ benchIO "scan-map"    (D.scanMap    4) D.sourceUnfoldrM
        , benchIO "drop-map"    (D.dropMap    4) D.sourceUnfoldrM
        , benchIO "drop-scan"   (D.dropScan   4) D.sourceUnfoldrM
        , benchIO "take-drop"   (D.takeDrop   4) D.sourceUnfoldrM
        , benchIO "take-scan"   (D.takeScan   4) D.sourceUnfoldrM
        , benchIO "take-map"    (D.takeMap    4) D.sourceUnfoldrM
        , benchIO "filter-drop" (D.filterDrop 4) D.sourceUnfoldrM
        , benchIO "filter-take" (D.filterTake 4) D.sourceUnfoldrM
        , benchIO "filter-scan" (D.filterScan 4) D.sourceUnfoldrM
        , benchIO "filter-map"  (D.filterMap  4) D.sourceUnfoldrM
        ]
      ]
    , bgroup "streamK"
      [ bgroup "generation"
        [ benchIO "unfoldr"       K.toNull K.sourceUnfoldr
        , benchIO "unfoldrM"      K.toNull K.sourceUnfoldrM
        -- , benchIO "fromEnum"     K.toNull K.sourceFromEnum

        , benchIO "fromFoldable"  K.toNull K.sourceFromFoldable
        -- , benchIO "fromFoldableM" K.toNull K.sourceFromFoldableM

        -- appends
        , benchIO "foldMapWith"  K.toNull K.sourceFoldMapWith
        , benchIO "foldMapWithM" K.toNull K.sourceFoldMapWithM
        ]
      , bgroup "elimination"
        [ benchIO "toNull" K.toNull K.sourceUnfoldrM
        , benchIO "uncons" K.uncons K.sourceUnfoldrM
        , benchFold "init" K.init   K.sourceUnfoldrM
        , benchFold "tail" K.tail   K.sourceUnfoldrM
        , benchIO "nullTail" K.nullTail K.sourceUnfoldrM
        , benchIO "headTail" K.headTail K.sourceUnfoldrM
        , benchFold "toList" K.toList K.sourceUnfoldrM
        , benchFold "fold"   K.foldl  K.sourceUnfoldrM
        , benchFold "last"   K.last   K.sourceUnfoldrM
        ]
      , bgroup "transformation"
        [ benchIO "scan"   (K.scan 1) K.sourceUnfoldrM
        , benchIO "map"    (K.map  1) K.sourceUnfoldrM
        , benchIO "mapM"   (K.mapM 1) K.sourceUnfoldrM
        -- , benchIO "concat" K.concat K.sourceUnfoldrM
        ]
      , bgroup "transformationN"
        [ benchIO "scan"   (K.scan 4) K.sourceUnfoldrM
        , benchIO "map"    (K.map  4) K.sourceUnfoldrM
        , benchIO "mapM"   (K.mapM 4) K.sourceUnfoldrM
        -- , benchIO "concat" K.concat K.sourceUnfoldrM
        ]
      , bgroup "filtering"
        [ benchIO "filter-even"     (K.filterEven     1) K.sourceUnfoldrM
        , benchIO "filter-all-out"  (K.filterAllOut   1) K.sourceUnfoldrM
        , benchIO "filter-all-in"   (K.filterAllIn    1) K.sourceUnfoldrM
        , benchIO "take-all"        (K.takeAll        1) K.sourceUnfoldrM
        , benchIO "takeWhile-true"  (K.takeWhileTrue  1) K.sourceUnfoldrM
        , benchIO "drop-one"        (K.dropOne        1) K.sourceUnfoldrM
        , benchIO "drop-all"        (K.dropAll        1) K.sourceUnfoldrM
        , benchIO "dropWhile-true"  (K.dropWhileTrue  1) K.sourceUnfoldrM
        , benchIO "dropWhile-false" (K.dropWhileFalse 1) K.sourceUnfoldrM
        ]
      , bgroup "filteringN"
        [ benchIO "filter-even"     (K.filterEven     4) K.sourceUnfoldrM
        , benchIO "filter-all-out"  (K.filterAllOut   4) K.sourceUnfoldrM
        , benchIO "filter-all-in"   (K.filterAllIn    4) K.sourceUnfoldrM
        , benchIO "take-all"        (K.takeAll        4) K.sourceUnfoldrM
        , benchIO "takeWhile-true"  (K.takeWhileTrue  4) K.sourceUnfoldrM
        , benchIO "drop-one"        (K.dropOne        4) K.sourceUnfoldrM
        , benchIO "drop-all"        (K.dropAll        4) K.sourceUnfoldrM
        , benchIO "dropWhile-true"  (K.dropWhileTrue  4) K.sourceUnfoldrM
        , benchIO "dropWhile-false" (K.dropWhileFalse 4) K.sourceUnfoldrM
        ]
      , benchIO "zip" K.zip K.sourceUnfoldrM
      , bgroup "composed"
        [ benchIO "scan-map"    (K.scanMap    4) K.sourceUnfoldrM
        , benchIO "drop-map"    (K.dropMap    4) K.sourceUnfoldrM
        , benchIO "drop-scan"   (K.dropScan   4) K.sourceUnfoldrM
        , benchIO "take-drop"   (K.takeDrop   4) K.sourceUnfoldrM
        , benchIO "take-scan"   (K.takeScan   4) K.sourceUnfoldrM
        , benchIO "take-map"    (K.takeMap    4) K.sourceUnfoldrM
        , benchIO "filter-drop" (K.filterDrop 4) K.sourceUnfoldrM
        , benchIO "filter-take" (K.filterTake 4) K.sourceUnfoldrM
        , benchIO "filter-scan" (K.filterScan 4) K.sourceUnfoldrM
        , benchIO "filter-map"  (K.filterMap  4) K.sourceUnfoldrM
        ]
      ]
    ]
