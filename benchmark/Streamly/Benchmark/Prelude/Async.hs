{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Prelude hiding (mapM)

import Streamly.Prelude (fromAsync, async, maxBuffer, maxThreads, fromSerial)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream.Transform as Transform

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Test.Tasty.Bench

moduleName :: String
moduleName = "Prelude.Async"

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
    -- These basically test the performance of consMAsync
    [ bgroup "generation"
        [ benchIOSrc fromAsync "unfoldr" (sourceUnfoldr value)
        , benchIOSrc fromAsync "unfoldrM" (sourceUnfoldrM value)
        , benchIOSrc fromAsync "fromListM" (sourceFromListM value)
        , benchIOSrc fromAsync "fromFoldable (List)" (sourceFromFoldable value)
        , benchIOSrc fromAsync "fromFoldableM (List)" (sourceFromFoldableM value)
        , benchIOSrc fromAsync "unfoldrM maxThreads 1"
            (maxThreads 1 . sourceUnfoldrM value)
        , benchIOSrc fromAsync "unfoldrM maxBuffer 1 (x/10 ops)"
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
    $ fromAsync
    $ Transform.foldrSShared (\x xs -> S.consM (return x) xs) S.nil
    $ fromSerial
    $ sourceUnfoldrM count n

o_1_space_mapping :: Int -> [Benchmark]
o_1_space_mapping value =
    [ bgroup "mapping"
        [ benchIOSink value "map" $ mapN fromAsync 1
        , benchIOSink value "fmap" $ fmapN fromAsync 1
        , benchIOSrc1 "foldrSShared" (foldrSShared value)
        -- This basically tests the performance of consMAsync
        , benchIOSink value "mapM" $ mapM fromAsync 1 . fromSerial
        ]
    ]

-------------------------------------------------------------------------------
-- Size conserving transformations (reordering, buffering, etc.)
-------------------------------------------------------------------------------

o_n_heap_buffering :: Int -> [Benchmark]
o_n_heap_buffering value =
    [bgroup "buffered" [benchIOSink value "mkAsync" (mkAsync fromAsync)]]

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
        [ benchIOSrc fromAsync "foldMapWith (<>) (List)"
            (sourceFoldMapWith value)
        , benchIOSrc fromAsync "foldMapWith (<>) (Stream)"
            (sourceFoldMapWithStream value)
        , benchIOSrc fromAsync "foldMapWithM (<>) (List)"
            (sourceFoldMapWithM value)
        , benchIOSrc fromAsync "S.concatFoldableWith (<>) (List)"
            (concatFoldableWith value)
        , benchIOSrc fromAsync "S.concatForFoldableWith (<>) (List)"
            (concatForFoldableWith value)
        , benchIOSrc fromAsync "foldMapM (List)" (sourceFoldMapM value)
        ]
    ]

-- These basically test the performance of concating streams with `async`
o_1_space_concatMap :: Int -> [Benchmark]
o_1_space_concatMap value =
    value2 `seq`
        [ bgroup "concat"
            -- This is for comparison with foldMapWith
            [ benchIOSrc fromSerial "concatMapWithId (n of 1) (fromFoldable)"
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
        [ benchIO "toNullAp"       $ toNullAp value fromAsync
        , benchIO "toNull"         $ toNullM value fromAsync
        , benchIO "toNull3"        $ toNullM3 value fromAsync
        , benchIO "filterAllOut"   $ filterAllOutM value fromAsync
        , benchIO "filterAllIn"    $ filterAllInM value fromAsync
        , benchIO "filterSome"     $ filterSome value fromAsync
        , benchIO "breakAfterSome" $ breakAfterSome value fromAsync

        ]
    ]

o_n_space_outerProduct :: Int -> [Benchmark]
o_n_space_outerProduct value =
    [ bgroup "monad-outer-product"
        [ benchIO "toList"         $ toListM value fromAsync
        , benchIO "toListSome"     $ toListSome value fromAsync
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
            , o_1_space_joining value
            ]
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_buffering value)
        , bgroup (o_n_space_prefix moduleName) (o_n_space_outerProduct value)
        ]
