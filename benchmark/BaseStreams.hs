-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP                       #-}

import Control.DeepSeq (NFData(..))
-- import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)

import Gauge
import qualified Streamly.Benchmark.Data.Stream.StreamD as D
import qualified Streamly.Benchmark.Data.Stream.StreamK as K
import qualified Streamly.Benchmark.Data.Stream.StreamDK as DK
import qualified Data.List as List

#if !MIN_VERSION_deepseq(1,4,3)
instance NFData Ordering where rnf = (`seq` ())
#endif

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.
{-# INLINE benchFold #-}
benchFold :: NFData b
    => String -> (t IO Int -> IO b) -> (Int -> t IO Int) -> Benchmark
benchFold name f src = bench name $ nfIO $ randomRIO (1,1) >>= f . src

{-# INLINE benchPure #-}
benchPure :: String -> ([Int] -> [Int]) -> (Int -> [Int]) -> Benchmark
benchPure name run f = bench name $ nfIO $ randomRIO (1,1) >>= return . run . f

#if defined(O_N_STACK)
-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchD #-}
benchD :: String -> (Int -> D.Stream IO Int) -> Benchmark
benchD name f = bench name $ nfIO $ randomRIO (1,1) >>= D.toNull . f

{-# INLINE benchK #-}
benchK :: String -> (Int -> K.Stream IO Int) -> Benchmark
benchK name f = bench name $ nfIO $ randomRIO (1,1) >>= K.toNull . f
#endif

main :: IO ()
main =
  defaultMain
#ifdef O_1_SPACE
    [ bgroup "streamD"
      [ bgroup "generation"
        [ benchFold "unfoldr"      D.toNull D.sourceUnfoldr
        , benchFold "unfoldrM"     D.toNull D.sourceUnfoldrM
        , benchFold "intFromTo"    D.toNull D.sourceIntFromTo

        , benchFold "fromList" D.toNull D.sourceFromList
        -- , benchFold "fromFoldableM" D.sourceFromFoldableM
        ]
      , bgroup "elimination"
        [ benchFold "toNull"   D.toNull   D.sourceUnfoldrM
        , benchFold "mapM_"    D.mapM_    D.sourceUnfoldrM
        , benchFold "uncons"   D.uncons   D.sourceUnfoldrM
        , benchFold "foldl'" D.foldl    D.sourceUnfoldrM
        , benchFold "last"   D.last     D.sourceUnfoldrM
        ]
      , bgroup "nested"
        [ benchFold "toNullAp" D.toNullApNested (D.sourceUnfoldrMN D.value2)
        , benchFold "toNull"   D.toNullNested   (D.sourceUnfoldrMN D.value2)
        , benchFold "toNull3"  D.toNullNested3  (D.sourceUnfoldrMN D.value3)
        , benchFold "filterAllIn"  D.filterAllInNested  (D.sourceUnfoldrMN K.value2)
        , benchFold "filterAllOut"  D.filterAllOutNested  (D.sourceUnfoldrMN K.value2)
        , benchFold "toNullApPure" D.toNullApNested (D.sourceUnfoldrN K.value2)
        , benchFold "toNullPure"   D.toNullNested   (D.sourceUnfoldrN K.value2)
        , benchFold "toNull3Pure"  D.toNullNested3  (D.sourceUnfoldrN K.value3)
        , benchFold "filterAllInPure"  D.filterAllInNested  (D.sourceUnfoldrN K.value2)
        , benchFold "filterAllOutPure"  D.filterAllOutNested  (D.sourceUnfoldrN K.value2)
        ]
      , bgroup "transformation"
        [ benchFold "scan"      (D.scan      1) D.sourceUnfoldrM
        , benchFold "map"       (D.map       1) D.sourceUnfoldrM
        , benchFold "fmap"      (D.fmap      1) D.sourceUnfoldrM
        , benchFold "mapM"      (D.mapM      1) D.sourceUnfoldrM
        , benchFold "mapMaybe"  (D.mapMaybe  1) D.sourceUnfoldrM
        , benchFold "mapMaybeM" (D.mapMaybeM 1) D.sourceUnfoldrM
        , benchFold "concatMapNxN" (D.concatMap 1) (D.sourceUnfoldrMN D.value2)
        , benchFold "concatMapRepl4xN" D.concatMapRepl4xN
            (D.sourceUnfoldrMN (D.value `div` 4))
        , benchFold "concatMapPureNxN" (D.concatMap 1) (D.sourceUnfoldrN D.value2)
        , benchFold "concatMapURepl4xN" D.concatMapURepl4xN
            (D.sourceUnfoldrMN (D.value `div` 4))
        -- , benchFold "intersperse" (D.intersperse 1) (D.sourceUnfoldrMN D.value2)
        -- , benchFold "interspersePure" (D.intersperse 1) (D.sourceUnfoldrN D.value2)
        -- , benchFold "foldrS"    (D.foldrS    1) D.sourceUnfoldrM
        -- This has horrible performance, never finishes
        -- , benchFold "foldlS"    (D.foldlS    1) D.sourceUnfoldrM
        ]
      , bgroup "transformationX4"
        [ benchFold "scan"      (D.scan      4) D.sourceUnfoldrM
        , benchFold "map"       (D.map       4) D.sourceUnfoldrM
        , benchFold "fmap"      (D.fmap      4) D.sourceUnfoldrM
        , benchFold "mapM"      (D.mapM      4) D.sourceUnfoldrM
        , benchFold "mapMaybe"  (D.mapMaybe  4) D.sourceUnfoldrM
        , benchFold "mapMaybeM" (D.mapMaybeM 4) D.sourceUnfoldrM
        -- , benchFold "concatMap" (D.concatMap 4) (D.sourceUnfoldrMN D.value16)
        -- , benchFold "intersperse" (D.intersperse 4) (D.sourceUnfoldrMN D.value16)
        ]
      , bgroup "filtering"
        [ benchFold "filter-even"     (D.filterEven     1) D.sourceUnfoldrM
        , benchFold "filter-all-out"  (D.filterAllOut   1) D.sourceUnfoldrM
        , benchFold "filter-all-in"   (D.filterAllIn    1) D.sourceUnfoldrM
        , benchFold "take-all"        (D.takeAll        1) D.sourceUnfoldrM
        , benchFold "takeWhile-true"  (D.takeWhileTrue  1) D.sourceUnfoldrM
        , benchFold "drop-one"        (D.dropOne        1) D.sourceUnfoldrM
        , benchFold "drop-all"        (D.dropAll        1) D.sourceUnfoldrM
        , benchFold "dropWhile-true"  (D.dropWhileTrue  1) D.sourceUnfoldrM
        , benchFold "dropWhile-false" (D.dropWhileFalse 1) D.sourceUnfoldrM
        ]
      , bgroup "filteringX4"
        [ benchFold "filter-even"     (D.filterEven     4) D.sourceUnfoldrM
        , benchFold "filter-all-out"  (D.filterAllOut   4) D.sourceUnfoldrM
        , benchFold "filter-all-in"   (D.filterAllIn    4) D.sourceUnfoldrM
        , benchFold "take-all"        (D.takeAll        4) D.sourceUnfoldrM
        , benchFold "takeWhile-true"  (D.takeWhileTrue  4) D.sourceUnfoldrM
        , benchFold "drop-one"        (D.dropOne        4) D.sourceUnfoldrM
        , benchFold "drop-all"        (D.dropAll        4) D.sourceUnfoldrM
        , benchFold "dropWhile-true"  (D.dropWhileTrue  4) D.sourceUnfoldrM
        , benchFold "dropWhile-false" (D.dropWhileFalse 4) D.sourceUnfoldrM
        ]
      , bgroup "zipping"
        [ benchFold "eqBy"  D.eqBy  D.sourceUnfoldrM
        , benchFold "cmpBy" D.cmpBy D.sourceUnfoldrM
        , benchFold   "zip"   D.zip   D.sourceUnfoldrM
        ]
      , bgroup "mixed"
        [ benchFold "scan-map"    (D.scanMap    1) D.sourceUnfoldrM
        , benchFold "drop-map"    (D.dropMap    1) D.sourceUnfoldrM
        , benchFold "drop-scan"   (D.dropScan   1) D.sourceUnfoldrM
        , benchFold "take-drop"   (D.takeDrop   1) D.sourceUnfoldrM
        , benchFold "take-scan"   (D.takeScan   1) D.sourceUnfoldrM
        , benchFold "take-map"    (D.takeMap    1) D.sourceUnfoldrM
        , benchFold "filter-drop" (D.filterDrop 1) D.sourceUnfoldrM
        , benchFold "filter-take" (D.filterTake 1) D.sourceUnfoldrM
        , benchFold "filter-scan" (D.filterScan 1) D.sourceUnfoldrM
        , benchFold "filter-map"  (D.filterMap  1) D.sourceUnfoldrM
        ]
      , bgroup "mixedX2"
        [ benchFold "scan-map"    (D.scanMap    2) D.sourceUnfoldrM
        , benchFold "drop-map"    (D.dropMap    2) D.sourceUnfoldrM
        , benchFold "drop-scan"   (D.dropScan   2) D.sourceUnfoldrM
        , benchFold "take-drop"   (D.takeDrop   2) D.sourceUnfoldrM
        , benchFold "take-scan"   (D.takeScan   2) D.sourceUnfoldrM
        , benchFold "take-map"    (D.takeMap    2) D.sourceUnfoldrM
        , benchFold "filter-drop" (D.filterDrop 2) D.sourceUnfoldrM
        , benchFold "filter-take" (D.filterTake 2) D.sourceUnfoldrM
        , benchFold "filter-scan" (D.filterScan 2) D.sourceUnfoldrM
        , benchFold "filter-map"  (D.filterMap  2) D.sourceUnfoldrM
        ]
      , bgroup "mixedX4"
        [ benchFold "scan-map"    (D.scanMap    4) D.sourceUnfoldrM
        , benchFold "drop-map"    (D.dropMap    4) D.sourceUnfoldrM
        , benchFold "drop-scan"   (D.dropScan   4) D.sourceUnfoldrM
        , benchFold "take-drop"   (D.takeDrop   4) D.sourceUnfoldrM
        , benchFold "take-scan"   (D.takeScan   4) D.sourceUnfoldrM
        , benchFold "take-map"    (D.takeMap    4) D.sourceUnfoldrM
        , benchFold "filter-drop" (D.filterDrop 4) D.sourceUnfoldrM
        , benchFold "filter-take" (D.filterTake 4) D.sourceUnfoldrM
        , benchFold "filter-scan" (D.filterScan 4) D.sourceUnfoldrM
        , benchFold "filter-map"  (D.filterMap  4) D.sourceUnfoldrM
        ]
      ]
    , bgroup "list"
      [ bgroup "elimination"
        [ benchPure "last" (\xs -> [List.last xs]) (K.sourceUnfoldrList K.value)
        ]
      , bgroup "nested"
        [ benchPure "toNullAp" K.toNullApNestedList (K.sourceUnfoldrList K.value2)
        , benchPure "toNull"   K.toNullNestedList (K.sourceUnfoldrList K.value2)
        , benchPure "toNull3"  K.toNullNestedList3 (K.sourceUnfoldrList K.value3)
        , benchPure "filterAllIn"  K.filterAllInNestedList (K.sourceUnfoldrList K.value2)
        , benchPure "filterAllOut"  K.filterAllOutNestedList (K.sourceUnfoldrList K.value2)
        ]
      ]
    , bgroup "streamK"
      [ bgroup "generation"
        [ benchFold "unfoldr"       K.toNull K.sourceUnfoldr
        , benchFold "unfoldrM"      K.toNull K.sourceUnfoldrM
        -- , benchFold "fromEnum"     K.toNull K.sourceFromEnum

        , benchFold "fromFoldable"  K.toNull K.sourceFromFoldable
        -- , benchFold "fromFoldableM" K.toNull K.sourceFromFoldableM

        -- appends
        , benchFold "foldMapWith"  K.toNull K.sourceFoldMapWith
        , benchFold "foldMapWithM" K.toNull K.sourceFoldMapWithM
        ]
      , bgroup "elimination"
        [ benchFold "toNull"   K.toNull   K.sourceUnfoldrM
        , benchFold "mapM_"    K.mapM_    K.sourceUnfoldrM
        , benchFold "uncons"   K.uncons   K.sourceUnfoldrM
        , benchFold "init"   K.init     K.sourceUnfoldrM
        , benchFold "foldl'" K.foldl    K.sourceUnfoldrM
        , benchFold "last"   K.last     K.sourceUnfoldrM
        ]
      , bgroup "nested"
        [ benchFold "toNullAp" K.toNullApNested (K.sourceUnfoldrMN K.value2)
        , benchFold "toNull"   K.toNullNested   (K.sourceUnfoldrMN K.value2)
        , benchFold "toNull3"  K.toNullNested3  (K.sourceUnfoldrMN K.value3)
        , benchFold "filterAllIn"  K.filterAllInNested  (K.sourceUnfoldrMN K.value2)
        , benchFold "filterAllOut" K.filterAllOutNested (K.sourceUnfoldrMN K.value2)
        , benchFold "toNullApPure" K.toNullApNested (K.sourceUnfoldrN K.value2)
        , benchFold "toNullPure"   K.toNullNested   (K.sourceUnfoldrN K.value2)
        , benchFold "toNull3Pure"  K.toNullNested3  (K.sourceUnfoldrN K.value3)
        , benchFold "filterAllInPure"  K.filterAllInNested  (K.sourceUnfoldrN K.value2)
        , benchFold "filterAllOutPure" K.filterAllOutNested (K.sourceUnfoldrN K.value2)
        ]
      , bgroup "transformation"
        [ benchFold "scan"   (K.scan 1) K.sourceUnfoldrM
        , benchFold "map"    (K.map  1) K.sourceUnfoldrM
        , benchFold "fmap"   (K.fmap 1) K.sourceUnfoldrM
        , benchFold "mapM"   (K.mapM 1) K.sourceUnfoldrM
        , benchFold "mapMSerial"  (K.mapMSerial 1) K.sourceUnfoldrM
        -- , benchFoldSrcK "concatMap" K.concatMap
        , benchFold "concatMapNxN" (K.concatMap 1) (K.sourceUnfoldrMN K.value2)
        , benchFold "concatMapPureNxN" (K.concatMap 1) (K.sourceUnfoldrN K.value2)
        , benchFold "concatMapRepl4xN" K.concatMapRepl4xN
            (K.sourceUnfoldrMN (K.value `div` 4))
        -- , benchFold "intersperse" (K.intersperse 1) (K.sourceUnfoldrMN K.value2)
        -- , benchFold "interspersePure" (K.intersperse 1) (K.sourceUnfoldrN K.value2)
        ]
      , bgroup "transformationX4"
        [ benchFold "scan"   (K.scan 4) K.sourceUnfoldrM
        , benchFold "map"    (K.map  4) K.sourceUnfoldrM
        , benchFold "fmap"   (K.fmap 4) K.sourceUnfoldrM
        , benchFold "mapM"   (K.mapM 4) K.sourceUnfoldrM
        , benchFold "mapMSerial" (K.mapMSerial 4) K.sourceUnfoldrM
        -- , benchFold "concatMap" (K.concatMap 4) (K.sourceUnfoldrMN K.value16)
        -- , benchFold "intersperse" (K.intersperse 4) (K.sourceUnfoldrMN K.value16)
        ]
      , bgroup "filtering"
        [ benchFold "filter-even"     (K.filterEven     1) K.sourceUnfoldrM
        , benchFold "filter-all-out"  (K.filterAllOut   1) K.sourceUnfoldrM
        , benchFold "filter-all-in"   (K.filterAllIn    1) K.sourceUnfoldrM
        , benchFold "take-all"        (K.takeAll        1) K.sourceUnfoldrM
        , benchFold "takeWhile-true"  (K.takeWhileTrue  1) K.sourceUnfoldrM
        , benchFold "drop-one"        (K.dropOne        1) K.sourceUnfoldrM
        , benchFold "drop-all"        (K.dropAll        1) K.sourceUnfoldrM
        , benchFold "dropWhile-true"  (K.dropWhileTrue  1) K.sourceUnfoldrM
        , benchFold "dropWhile-false" (K.dropWhileFalse 1) K.sourceUnfoldrM
        ]
      , bgroup "filteringX4"
        [ benchFold "filter-even"     (K.filterEven     4) K.sourceUnfoldrM
        , benchFold "filter-all-out"  (K.filterAllOut   4) K.sourceUnfoldrM
        , benchFold "filter-all-in"   (K.filterAllIn    4) K.sourceUnfoldrM
        , benchFold "take-all"        (K.takeAll        4) K.sourceUnfoldrM
        , benchFold "takeWhile-true"  (K.takeWhileTrue  4) K.sourceUnfoldrM
        , benchFold "drop-one"        (K.dropOne        4) K.sourceUnfoldrM
        , benchFold "drop-all"        (K.dropAll        4) K.sourceUnfoldrM
        , benchFold "dropWhile-true"  (K.dropWhileTrue  4) K.sourceUnfoldrM
        , benchFold "dropWhile-false" (K.dropWhileFalse 4) K.sourceUnfoldrM
        ]
      , bgroup "zipping"
        [ benchFold "zip" K.zip K.sourceUnfoldrM
        ]
      , bgroup "mixed"
        [ benchFold "scan-map"    (K.scanMap    1) K.sourceUnfoldrM
        , benchFold "drop-map"    (K.dropMap    1) K.sourceUnfoldrM
        , benchFold "drop-scan"   (K.dropScan   1) K.sourceUnfoldrM
        , benchFold "take-drop"   (K.takeDrop   1) K.sourceUnfoldrM
        , benchFold "take-scan"   (K.takeScan   1) K.sourceUnfoldrM
        , benchFold "take-map"    (K.takeMap    1) K.sourceUnfoldrM
        , benchFold "filter-drop" (K.filterDrop 1) K.sourceUnfoldrM
        , benchFold "filter-take" (K.filterTake 1) K.sourceUnfoldrM
        , benchFold "filter-scan" (K.filterScan 1) K.sourceUnfoldrM
        , benchFold "filter-map"  (K.filterMap  1) K.sourceUnfoldrM
        ]
      , bgroup "mixedX2"
        [ benchFold "scan-map"    (K.scanMap    2) K.sourceUnfoldrM
        , benchFold "drop-map"    (K.dropMap    2) K.sourceUnfoldrM
        , benchFold "drop-scan"   (K.dropScan   2) K.sourceUnfoldrM
        , benchFold "take-drop"   (K.takeDrop   2) K.sourceUnfoldrM
        , benchFold "take-scan"   (K.takeScan   2) K.sourceUnfoldrM
        , benchFold "take-map"    (K.takeMap    2) K.sourceUnfoldrM
        , benchFold "filter-drop" (K.filterDrop 2) K.sourceUnfoldrM
        , benchFold "filter-take" (K.filterTake 2) K.sourceUnfoldrM
        , benchFold "filter-scan" (K.filterScan 2) K.sourceUnfoldrM
        , benchFold "filter-map"  (K.filterMap  2) K.sourceUnfoldrM
        ]
      , bgroup "mixedX4"
        [ benchFold "scan-map"    (K.scanMap    4) K.sourceUnfoldrM
        , benchFold "drop-map"    (K.dropMap    4) K.sourceUnfoldrM
        , benchFold "drop-scan"   (K.dropScan   4) K.sourceUnfoldrM
        , benchFold "take-drop"   (K.takeDrop   4) K.sourceUnfoldrM
        , benchFold "take-scan"   (K.takeScan   4) K.sourceUnfoldrM
        , benchFold "take-map"    (K.takeMap    4) K.sourceUnfoldrM
        , benchFold "filter-drop" (K.filterDrop 4) K.sourceUnfoldrM
        , benchFold "filter-take" (K.filterTake 4) K.sourceUnfoldrM
        , benchFold "filter-scan" (K.filterScan 4) K.sourceUnfoldrM
        , benchFold "filter-map"  (K.filterMap  4) K.sourceUnfoldrM
        ]
      ]
    , bgroup "streamDK"
      [ bgroup "generation"
        [ benchFold "unfoldr"       DK.toNull DK.sourceUnfoldr
        , benchFold "unfoldrM"      DK.toNull DK.sourceUnfoldrM
        ]
      , bgroup "elimination"
        [ benchFold "toNull"   DK.toNull   DK.sourceUnfoldrM
        , benchFold "uncons"   DK.uncons   DK.sourceUnfoldrM
        ]
      ]
    ]
#elif defined(O_N_HEAP)
    [ bgroup "streamK"
      [ bgroup "transformation"
        [ benchFold "foldlS" (K.foldlS 1) K.sourceUnfoldrM
        ]
      ]
    ]
#elif defined(O_N_STACK)
    [ bgroup "streamD"
      [ bgroup "elimination"
        [ benchFold "tail"   D.tail     D.sourceUnfoldrM
        , benchFold "nullTail" D.nullTail D.sourceUnfoldrM
        , benchFold "headTail" D.headTail D.sourceUnfoldrM
        ]
      , bgroup "iterated"
        [ benchD "mapM"                 D.iterateMapM
        , benchD "scan(1/10)"           D.iterateScan
        , benchD "filterEven"           D.iterateFilterEven
        , benchD "takeAll"              D.iterateTakeAll
        , benchD "dropOne"              D.iterateDropOne
        , benchD "dropWhileFalse(1/10)" D.iterateDropWhileFalse
        , benchD "dropWhileTrue"        D.iterateDropWhileTrue
        , benchD "iterateM"             D.iterateM
        ]
      ]
    , bgroup "streamK"
      [ bgroup "elimination"
        [ benchFold "tail"   K.tail     K.sourceUnfoldrM
        , benchFold "nullTail" K.nullTail K.sourceUnfoldrM
        , benchFold "headTail" K.headTail K.sourceUnfoldrM
        ]
      , bgroup "iterated"
        [ benchK "mapM"                 K.iterateMapM
        , benchK "scan(1/10)"           K.iterateScan
        , benchK "filterEven"           K.iterateFilterEven
        , benchK "takeAll"              K.iterateTakeAll
        , benchK "dropOne"              K.iterateDropOne
        , benchK "dropWhileFalse(1/10)" K.iterateDropWhileFalse
        , benchK "dropWhileTrue"        K.iterateDropWhileTrue
        ]
      ]
   ]
#elif defined(O_N_SPACE)
    [ bgroup "streamD"
      [ bgroup "elimination"
        [ benchFold "toList" D.toList   D.sourceUnfoldrM
        ]
      ]
    , bgroup "streamK"
      [ bgroup "elimination"
        [ benchFold "toList" K.toList   K.sourceUnfoldrM
        ]
      ]
   ]
#else
#error "One of O_1_SPACE/O_N_HEAP/O_N_STACK/O_N_SPACE must be defined"
#endif
