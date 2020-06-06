-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Prelude hiding (mapM)

import Streamly (asyncly, async, maxBuffer, maxThreads)

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Gauge

moduleName :: String
moduleName = "Prelude.Async"

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
    [ bgroup "generation"
        [ benchIOSrc asyncly "unfoldr" (sourceUnfoldr value)
        , benchIOSrc asyncly "unfoldrM" (sourceUnfoldrM value)
        , benchIOSrc asyncly "fromFoldable" (sourceFromFoldable value)
        , benchIOSrc asyncly "fromFoldableM" (sourceFromFoldableM value)
        , benchIOSrc asyncly "unfoldrM maxThreads 1"
            (maxThreads 1 . sourceUnfoldrM value)
        , benchIOSrc asyncly "unfoldrM maxBuffer 1 (x/10 ops)"
            (maxBuffer 1 . sourceUnfoldrMN (value `div` 10))
        ]
    ]

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

o_1_space_mapping :: Int -> [Benchmark]
o_1_space_mapping value =
    [ bgroup "mapping"
        [ benchIOSink value "map" $ mapN asyncly 1
        , benchIOSink value "fmap" $ fmapN asyncly 1
        , benchIOSink value "mapM" $ mapM asyncly 1
        ]
    ]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

o_1_space_concatFoldable :: Int -> [Benchmark]
o_1_space_concatFoldable value =
    [ bgroup "concat-foldable"
        [ benchIOSrc asyncly "foldMapWith" (sourceFoldMapWith value)
        , benchIOSrc asyncly "foldMapWithM" (sourceFoldMapWithM value)
        , benchIOSrc asyncly "foldMapM" (sourceFoldMapM value)
        ]
    ]

o_1_space_concatMap :: Int -> [Benchmark]
o_1_space_concatMap value =
    value2 `seq`
        [ bgroup "concatMap"
            [ benchIO "concatMapWith (2,x/2)"
                  (concatStreamsWith async 2 (value `div` 2))
            , benchIO "concatMapWith (sqrt x,sqrt x)"
                  (concatStreamsWith async value2 value2)
            , benchIO "concatMapWith (sqrt x * 2,sqrt x / 2)"
                  (concatStreamsWith async (value2 * 2) (value2 `div` 2))
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
        [ benchIO "toNullAp"       $ toNullAp value asyncly
        , benchIO "toNull"         $ toNullM value asyncly
        , benchIO "toNull3"        $ toNullM3 value asyncly
        , benchIO "filterAllOut"   $ filterAllOutM value asyncly
        , benchIO "filterAllIn"    $ filterAllInM value asyncly
        , benchIO "filterSome"     $ filterSome value asyncly
        , benchIO "breakAfterSome" $ breakAfterSome value asyncly

        ]
    ]

o_n_space_outerProduct :: Int -> [Benchmark]
o_n_space_outerProduct value =
    [ bgroup "monad-outer-product"
        [ benchIO "toList"         $ toListM value asyncly
        , benchIO "toListSome"     $ toListSome value asyncly
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
