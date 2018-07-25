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
main = do
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
        [ benchIO "toNull" D.toNull D.sourceUnfoldrM
        , benchIO "uncons" D.uncons D.sourceUnfoldrM
        , benchIO "nullHeadTail" D.nullHeadTail D.sourceUnfoldrM
        ]
      , bgroup "transformation"
        [ benchIO "scanlM'" D.scan D.sourceUnfoldrM
        , benchIO "map"  D.map D.sourceUnfoldrM
        , benchIO "mapM" D.mapM D.sourceUnfoldrM
        ]
      , bgroup "filtering"
        [ benchIO "filter-even"    D.filterEven D.sourceUnfoldrM
        , benchIO "filter-all-out" D.filterAllOut D.sourceUnfoldrM
        , benchIO "filter-all-in"  D.filterAllIn D.sourceUnfoldrM
        , benchIO "take-all"       D.takeAll D.sourceUnfoldrM
        , benchIO "takeWhile-true" D.takeWhileTrue D.sourceUnfoldrM
        , benchIO "drop-all"       D.dropAll D.sourceUnfoldrM
        , benchIO "dropWhile-true" D.dropWhileTrue D.sourceUnfoldrM
        ]
      , benchIO "zip" D.zip D.sourceUnfoldrM
      , bgroup "compose"
        [ benchIO "mapM" D.composeMapM D.sourceUnfoldrM
#if __GLASGOW_HASKELL__ != 802
        , benchIO "map-with-all-in-filter" D.composeMapAllInFilter D.sourceUnfoldrM
        , benchIO "all-in-filters" D.composeAllInFilters D.sourceUnfoldrM
        , benchIO "all-out-filters" D.composeAllOutFilters D.sourceUnfoldrM
#endif
        ]
        -- Scaling with same operation in sequence
      , bgroup "compose-scaling"
        [ benchIO "1" (D.composeScaling 1) D.sourceUnfoldrM
        , benchIO "2" (D.composeScaling 2) D.sourceUnfoldrM
        , benchIO "3" (D.composeScaling 3) D.sourceUnfoldrM
        , benchIO "4" (D.composeScaling 4) D.sourceUnfoldrM
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
        , benchIO "nullHeadTail" K.nullHeadTail K.sourceUnfoldrM
        , benchFold "toList" K.toList K.sourceUnfoldrM
        , benchFold "fold"   K.foldl  K.sourceUnfoldrM
        , benchFold "last"   K.last   K.sourceUnfoldrM
        ]
      , bgroup "transformation"
        [ benchIO "scan"   K.scan K.sourceUnfoldrM
        , benchIO "map"    K.map K.sourceUnfoldrM
        , benchIO "mapM"   K.mapM K.sourceUnfoldrM
        -- , benchIO "concat" K.concat K.sourceUnfoldrM
        ]
      , bgroup "filtering"
        [ benchIO "filter-even"    K.filterEven K.sourceUnfoldrM
        , benchIO "filter-all-out" K.filterAllOut K.sourceUnfoldrM
        , benchIO "filter-all-in"  K.filterAllIn K.sourceUnfoldrM
        , benchIO "take-all"       K.takeAll K.sourceUnfoldrM
        , benchIO "takeWhile-true" K.takeWhileTrue K.sourceUnfoldrM
        , benchIO "drop-all"       K.dropAll K.sourceUnfoldrM
        , benchIO "dropWhile-true" K.dropWhileTrue K.sourceUnfoldrM
        ]
      , benchIO "zip" K.zip K.sourceUnfoldrM
      , bgroup "compose"
        [ benchIO "mapM" K.composeMapM K.sourceUnfoldrM
        , benchIO "map-with-all-in-filter" K.composeMapAllInFilter K.sourceUnfoldrM
        , benchIO "all-in-filters" K.composeAllInFilters K.sourceUnfoldrM
        , benchIO "all-out-filters" K.composeAllOutFilters K.sourceUnfoldrM
        ]
        -- Scaling with same operation in sequence
      , bgroup "compose-scaling"
        [ benchIO "1" (K.composeScaling 1) K.sourceUnfoldrM
        , benchIO "2" (K.composeScaling 2) K.sourceUnfoldrM
        , benchIO "3" (K.composeScaling 3) K.sourceUnfoldrM
        , benchIO "4" (K.composeScaling 4) K.sourceUnfoldrM
        ]
      ]
    ]
