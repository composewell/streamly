{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Prelude hiding (mapM)

import Streamly.Prelude (fromAhead, fromSerial, ahead, maxBuffer, maxThreads)
import qualified Streamly.Prelude as S

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Test.Tasty.Bench

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
        [ benchIOSrc fromAhead "unfoldr" (sourceUnfoldr value)
        , benchIOSrc fromAhead "unfoldrM" (sourceUnfoldrM value)
        , benchIOSrc fromAhead "fromFoldable" (sourceFromFoldable value)
        , benchIOSrc fromAhead "fromFoldableM" (sourceFromFoldableM value)
        , benchIOSrc fromAhead "unfoldrM maxThreads 1"
            (maxThreads 1 . sourceUnfoldrM value)
        , benchIOSrc fromAhead "unfoldrM maxBuffer 1 (x/10 ops)"
            (maxBuffer 1 . sourceUnfoldrM (value `div` 10))
        ]
    ]

o_1_space_mapping :: Int -> [Benchmark]
o_1_space_mapping value =
    [ bgroup "mapping"
        [ benchIOSink value "map" $ mapN fromAhead 1
        , benchIOSink value "fmap" $ fmapN fromAhead 1
        , benchIOSink value "mapM" $ mapM fromAhead 1 . fromSerial
        ]
    ]

o_1_space_concatFoldable :: Int -> [Benchmark]
o_1_space_concatFoldable value =
    [ bgroup
        "concat-foldable"
        [ benchIOSrc fromAhead "foldMapWith (<>) (List)"
            (sourceFoldMapWith value)
        , benchIOSrc fromAhead "foldMapWith (<>) (Stream)"
            (sourceFoldMapWithStream value)
        , benchIOSrc fromAhead "foldMapWithM (<>) (List)"
            (sourceFoldMapWithM value)
        , benchIOSrc fromSerial "S.concatFoldableWith (<>) (List)"
            (concatFoldableWith value)
        , benchIOSrc fromSerial "S.concatForFoldableWith (<>) (List)"
            (concatForFoldableWith value)
        , benchIOSrc fromAhead "foldMapM (List)" (sourceFoldMapM value)
        ]
    ]

o_1_space_concatMap :: Int -> [Benchmark]
o_1_space_concatMap value =
    value2 `seq`
        [ bgroup "concat"
            -- This is for comparison with foldMapWith
            [ benchIOSrc fromSerial "concatMapWithId (n of 1) (fromFoldable)"
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
        [ benchIO "toNullAp"       $ toNullAp value fromAhead
        , benchIO "toNull"         $ toNullM value fromAhead
        , benchIO "toNull3"        $ toNullM3 value fromAhead
        , benchIO "filterAllOut"   $ filterAllOutM value fromAhead
        , benchIO "filterAllIn"    $ filterAllInM value fromAhead
        , benchIO "filterSome"     $ filterSome value fromAhead
        , benchIO "breakAfterSome" $ breakAfterSome value fromAhead

        ]
    ]

o_n_space_outerProduct :: Int -> [Benchmark]
o_n_space_outerProduct value =
    [ bgroup "monad-outer-product"
        [ benchIO "toList"         $ toListM value fromAhead
        , benchIO "toListSome"     $ toListSome value fromAhead
        ]
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks

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
