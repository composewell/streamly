-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Prelude hiding (mapM)

import Streamly.Prelude (asyncly, async, maxBuffer, maxThreads, serially)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.StreamK.Type as Internal

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
    -- These basically test the performance of consMAsync
    [ bgroup "generation"
        [ benchIOSrc asyncly "unfoldr" (sourceUnfoldr value)
        , benchIOSrc asyncly "unfoldrM" (sourceUnfoldrM value)
        , benchIOSrc asyncly "fromListM" (sourceFromListM value)
        , benchIOSrc asyncly "fromFoldable (List)" (sourceFromFoldable value)
        , benchIOSrc asyncly "fromFoldableM (List)" (sourceFromFoldableM value)
        , benchIOSrc asyncly "unfoldrM maxThreads 1"
            (maxThreads 1 . sourceUnfoldrM value)
        , benchIOSrc asyncly "unfoldrM maxBuffer 1 (x/10 ops)"
            (maxBuffer 1 . sourceUnfoldrM (value `div` 10))
        ]
    ]

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

{-# INLINE foldrSShared #-}
foldrSShared :: Int -> Int -> IO ()
foldrSShared count n =
      S.drain
    $ asyncly
    $ Internal.foldrSShared (\x xs -> S.consM (return x) xs) S.nil
    $ serially
    $ sourceUnfoldrM count n

o_1_space_mapping :: Int -> [Benchmark]
o_1_space_mapping value =
    [ bgroup "mapping"
        [ benchIOSink value "map" $ mapN asyncly 1
        , benchIOSink value "fmap" $ fmapN asyncly 1
        , benchIOSrc1 "foldrSShared" (foldrSShared value)
        -- This basically tests the performance of consMAsync
        , benchIOSink value "mapM" $ mapM asyncly 1 . serially
        ]
    ]

-------------------------------------------------------------------------------
-- Joining
-------------------------------------------------------------------------------

{-# INLINE async2 #-}
async2 :: Int -> Int -> IO ()
async2 count n =
    S.drain $
        (sourceUnfoldrM count n) `async` (sourceUnfoldrM count (n + 1))

{-# INLINE async4 #-}
async4 :: Int -> Int -> IO ()
async4 count n =
    S.drain $
                  (sourceUnfoldrM count (n + 0))
        `async` (sourceUnfoldrM count (n + 1))
        `async` (sourceUnfoldrM count (n + 2))
        `async` (sourceUnfoldrM count (n + 3))

{-# INLINE async2n2 #-}
async2n2 :: Int -> Int -> IO ()
async2n2 count n =
    S.drain $
        ((sourceUnfoldrM count (n + 0))
            `async` (sourceUnfoldrM count (n + 1)))
        `async` ((sourceUnfoldrM count (n + 2))
            `async` (sourceUnfoldrM count (n + 3)))

o_1_space_joining :: Int -> [Benchmark]
o_1_space_joining value =
    [ bgroup "joining"
        [ benchIOSrc1 "async (2 of n/2)" (async2 (value `div` 2))
        , benchIOSrc1 "async (4 of n/4)" (async4 (value `div` 4))
        , benchIOSrc1 "async (2 of (2 of n/4)" (async2n2 (value `div` 4))
        ]
    ]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

-- These basically test the performance of folding streams with `async`
o_1_space_concatFoldable :: Int -> [Benchmark]
o_1_space_concatFoldable value =
    [ bgroup "concat-foldable"
        [ benchIOSrc asyncly "foldMapWith (<>) (List)"
            (sourceFoldMapWith value)
        , benchIOSrc asyncly "foldMapWith (<>) (Stream)"
            (sourceFoldMapWithStream value)
        , benchIOSrc asyncly "foldMapWithM (<>) (List)"
            (sourceFoldMapWithM value)
        , benchIOSrc asyncly "foldMapM (List)" (sourceFoldMapM value)
        ]
    ]

-- These basically test the performance of concating streams with `async`
o_1_space_concatMap :: Int -> [Benchmark]
o_1_space_concatMap value =
    value2 `seq`
        [ bgroup "concat"
            -- This is for comparison with foldMapWith
            [ benchIOSrc serially "concatMapWithId (n of 1) (fromFoldable)"
                (S.concatMapWith async id . sourceConcatMapId value)

            , benchIO "concatMapWith (n of 1)"
                  (concatStreamsWith async value 1)
            , benchIO "concatMapWith (sqrt x of sqrt x)"
                  (concatStreamsWith async value2 value2)
            , benchIO "concatMapWith (1 of n)"
                  (concatStreamsWith async 1 value)
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
            , o_1_space_joining value
            ]
        , bgroup (o_n_space_prefix moduleName) (o_n_space_outerProduct value)
        ]
