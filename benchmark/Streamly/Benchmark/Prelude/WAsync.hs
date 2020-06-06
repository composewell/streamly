-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Prelude hiding (mapM)

import Streamly (wAsyncly, wAsync, maxBuffer, maxThreads)

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Gauge

moduleName :: String
moduleName = "Prelude.WAsync"

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
    [ bgroup "generation"
        [ benchIOSrc wAsyncly "unfoldr" (sourceUnfoldr value)
        , benchIOSrc wAsyncly "unfoldrM" (sourceUnfoldrM value)
        , benchIOSrc wAsyncly "fromFoldable" (sourceFromFoldable value)
        , benchIOSrc wAsyncly "fromFoldableM" (sourceFromFoldableM value)
        , benchIOSrc wAsyncly "unfoldrM maxThreads 1"
            (maxThreads 1 . sourceUnfoldrM value)
        , benchIOSrc wAsyncly "unfoldrM maxBuffer 1 (x/10 ops)"
            (maxBuffer 1 . sourceUnfoldrMN (value `div` 10))
        ]
    ]

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

o_1_space_mapping :: Int -> [Benchmark]
o_1_space_mapping value =
    [ bgroup "mapping"
        [ benchIOSink value "map" $ mapN wAsyncly 1
        , benchIOSink value "fmap" $ fmapN wAsyncly 1
        , benchIOSink value "mapM" $ mapM wAsyncly 1
        ]
    ]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

o_1_space_concatFoldable :: Int -> [Benchmark]
o_1_space_concatFoldable value =
    [ bgroup "concat-foldable"
        [ benchIOSrc wAsyncly "foldMapWith" (sourceFoldMapWith value)
        , benchIOSrc wAsyncly "foldMapWithM" (sourceFoldMapWithM value)
        , benchIOSrc wAsyncly "foldMapM" (sourceFoldMapM value)
        ]
    ]

-- When we merge streams using wAsync the size of the queue increases
-- slowly because of the binary composition adding just one more item
-- to the work queue only after every scheduling pass through the
-- work queue.
--
-- We should see the memory consumption increasing slowly if these
-- benchmarks are left to run on infinite number of streams of infinite
-- sizes.
o_1_space_concatMap :: Int -> [Benchmark]
o_1_space_concatMap value =
    value2 `seq`
        [ bgroup "concat"
              [ benchIO "concatMapWith (2,x/2)"
                    (concatStreamsWith wAsync 2 (value `div` 2))
              , benchIO "concatMapWith (sqrt x,sqrt x)"
                    (concatStreamsWith wAsync value2 value2)
              , benchIO "concatMapWith (sqrt x * 2,sqrt x / 2)"
                    (concatStreamsWith wAsync (value2 * 2) (value2 `div` 2))
              ]
        ]

    where

    value2 = round $ sqrt (fromIntegral value :: Double)

-------------------------------------------------------------------------------
-- Monadic outer product
-------------------------------------------------------------------------------

o_n_heap_outerProduct :: Int -> [Benchmark]
o_n_heap_outerProduct value =
    [ bgroup "monad-outer-product"
        [ benchIO "toNullAp"       $ toNullAp value wAsyncly
        , benchIO "toNull"         $ toNullM value wAsyncly
        , benchIO "toNull3"        $ toNullM3 value wAsyncly
        , benchIO "filterAllOut"   $ filterAllOutM value wAsyncly
        , benchIO "filterAllIn"    $ filterAllInM value wAsyncly
        , benchIO "filterSome"     $ filterSome value wAsyncly
        , benchIO "breakAfterSome" $ breakAfterSome value wAsyncly

        ]
    ]

o_n_space_outerProduct :: Int -> [Benchmark]
o_n_space_outerProduct value =
    [ bgroup "monad-outer-product"
        [ benchIO "toList"         $ toListM value wAsyncly
        , benchIO "toListSome"     $ toListSome value wAsyncly
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
            ]
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_outerProduct value)
        , bgroup (o_n_space_prefix moduleName) (o_n_space_outerProduct value)
        ]
