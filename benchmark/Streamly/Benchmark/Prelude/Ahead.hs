-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Prelude hiding (mapM)

import Streamly.Prelude (aheadly, serially, ahead, maxBuffer, maxThreads)
import qualified Streamly.Prelude as S

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Gauge

moduleName :: String
moduleName = "Prelude.Ahead"

-------------------------------------------------------------------------------
-- Benchmark groups
-------------------------------------------------------------------------------

-- unfoldr and fromFoldable are always serial and therefore the same for
-- all stream types. They can be removed to reduce the number of benchmarks.
o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
    [ bgroup "generation"
        [ benchIOSrc aheadly "unfoldr" (sourceUnfoldr value)
        , benchIOSrc aheadly "unfoldrM" (sourceUnfoldrM value)
        , benchIOSrc aheadly "fromFoldable" (sourceFromFoldable value)
        , benchIOSrc aheadly "fromFoldableM" (sourceFromFoldableM value)
        , benchIOSrc aheadly "unfoldrM maxThreads 1"
            (maxThreads 1 . sourceUnfoldrM value)
        , benchIOSrc aheadly "unfoldrM maxBuffer 1 (x/10 ops)"
            (maxBuffer 1 . sourceUnfoldrM (value `div` 10))
        ]
    ]

o_1_space_mapping :: Int -> [Benchmark]
o_1_space_mapping value =
    [ bgroup "mapping"
        [ benchIOSink value "map" $ mapN aheadly 1
        , benchIOSink value "fmap" $ fmapN aheadly 1
        , benchIOSink value "mapM" $ mapM aheadly 1 . serially
        ]
    ]

o_1_space_concatFoldable :: Int -> [Benchmark]
o_1_space_concatFoldable value =
    [ bgroup
        "concat-foldable"
        [ benchIOSrc aheadly "foldMapWith (<>) (List)"
            (sourceFoldMapWith value)
        , benchIOSrc aheadly "foldMapWith (<>) (Stream)"
            (sourceFoldMapWithStream value)
        , benchIOSrc aheadly "foldMapWithM (<>) (List)"
            (sourceFoldMapWithM value)
        , benchIOSrc aheadly "foldMapM (List)" (sourceFoldMapM value)
        ]
    ]

o_1_space_concatMap :: Int -> [Benchmark]
o_1_space_concatMap value =
    value2 `seq`
        [ bgroup "concat"
            -- This is for comparison with foldMapWith
            [ benchIOSrc serially "concatMapWithId (n of 1) (fromFoldable)"
                (S.concatMapWith ahead id . sourceConcatMapId value)

            , benchIO "concatMapWith (n of 1)"
                  (concatStreamsWith ahead value 1)
            , benchIO "concatMapWith (sqrt x of sqrt x)"
                  (concatStreamsWith ahead value2 value2)
            , benchIO "concatMapWith (1 of n)"
                  (concatStreamsWith ahead 1 value)
            ]
        ]

    where

    value2 = round $ sqrt (fromIntegral value :: Double)

-------------------------------------------------------------------------------
-- Monadic outer product
-------------------------------------------------------------------------------

o_1_space_outerProduct :: Int -> [Benchmark]
o_1_space_outerProduct value =
    [ bgroup "monad-outer-product"
        [ benchIO "toNullAp"       $ toNullAp value aheadly
        , benchIO "toNull"         $ toNullM value aheadly
        , benchIO "toNull3"        $ toNullM3 value aheadly
        , benchIO "filterAllOut"   $ filterAllOutM value aheadly
        , benchIO "filterAllIn"    $ filterAllInM value aheadly
        , benchIO "filterSome"     $ filterSome value aheadly
        , benchIO "breakAfterSome" $ breakAfterSome value aheadly

        ]
    ]

o_n_space_outerProduct :: Int -> [Benchmark]
o_n_space_outerProduct value =
    [ bgroup "monad-outer-product"
        [ benchIO "toList"         $ toListM value aheadly
        , benchIO "toListSome"     $ toListSome value aheadly
        ]
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    value `seq` runMode (mode cfg) cfg benches (allBenchmarks value)

    where

    allBenchmarks value =
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [ o_1_space_generation value
            , o_1_space_mapping value
            , o_1_space_concatFoldable value
            , o_1_space_concatMap value
            , o_1_space_outerProduct value
            ]
        , bgroup (o_n_space_prefix moduleName) (o_n_space_outerProduct value)
        ]
