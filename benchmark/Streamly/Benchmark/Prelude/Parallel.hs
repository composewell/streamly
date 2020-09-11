-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (mapM)

import Streamly.Prelude
       ( SerialT, parallely, parallel, serially, maxBuffer, maxThreads)

import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.Parallel as Par
import qualified Streamly.Internal.Data.Stream.IsStream as Internal

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Gauge

moduleName :: String
moduleName = "Prelude.Parallel"

-------------------------------------------------------------------------------
-- Merging
-------------------------------------------------------------------------------

{-# INLINE mergeAsyncByM #-}
mergeAsyncByM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
mergeAsyncByM count n =
    S.mergeAsyncByM
        (\a b -> return (a `compare` b))
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

{-# INLINE mergeAsyncBy #-}
mergeAsyncBy :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
mergeAsyncBy count n =
    S.mergeAsyncBy
        compare
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

-------------------------------------------------------------------------------
-- Application/fold
-------------------------------------------------------------------------------

{-# INLINE parAppMap #-}
parAppMap :: S.MonadAsync m => SerialT m Int -> m ()
parAppMap src = S.drain $ S.map (+1) S.|$ src

{-# INLINE parAppSum #-}
parAppSum :: S.MonadAsync m => SerialT m Int -> m ()
parAppSum src = (S.sum S.|$. src) >>= \x -> seq x (return ())

-------------------------------------------------------------------------------
-- Tapping
-------------------------------------------------------------------------------

{-# INLINE tapAsyncS #-}
tapAsyncS :: S.MonadAsync m => Int -> SerialT m Int -> m ()
tapAsyncS n = composeN n $ Par.tapAsync S.sum

{-# INLINE tapAsync #-}
tapAsync :: S.MonadAsync m => Int -> SerialT m Int -> m ()
tapAsync n = composeN n $ Internal.tapAsync FL.sum

o_1_space_merge_app_tap :: Int -> [Benchmark]
o_1_space_merge_app_tap value =
    [ bgroup "merge-app-tap"
        [ benchIOSrc serially "mergeAsyncBy (2,x/2)"
              (mergeAsyncBy (value `div` 2))
        , benchIOSrc serially "mergeAsyncByM (2,x/2)"
              (mergeAsyncByM (value `div` 2))
        -- Parallel stages in a pipeline
        , benchIOSink value "parAppMap" parAppMap
        , benchIOSink value "parAppSum" parAppSum
        , benchIOSink value "tapAsync" (tapAsync 1)
        , benchIOSink value "tapAsyncS" (tapAsyncS 1)
        ]
    ]

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

o_n_heap_generation :: Int -> [Benchmark]
o_n_heap_generation value =
    [ bgroup
        "generation"
        [ benchIOSrc parallely "unfoldr" (sourceUnfoldr value)
        , benchIOSrc parallely "unfoldrM" (sourceUnfoldrM value)
        , benchIOSrc parallely "fromFoldable" (sourceFromFoldable value)
        , benchIOSrc parallely "fromFoldableM" (sourceFromFoldableM value)
        , benchIOSrc parallely "unfoldrM maxThreads 1"
              (maxThreads 1 . sourceUnfoldrM value)
        , benchIOSrc parallely "unfoldrM maxBuffer 1 (x/10 ops)"
              (maxBuffer 1 . sourceUnfoldrM (value `div` 10))
        ]
    ]

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

o_n_heap_mapping :: Int -> [Benchmark]
o_n_heap_mapping value =
    [ bgroup "mapping"
        [ benchIOSink value "map" $ mapN parallely 1
        , benchIOSink value "fmap" $ fmapN parallely 1
        , benchIOSink value "mapM" $ mapM parallely 1 . serially
        ]
    ]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

o_n_heap_concatFoldable :: Int -> [Benchmark]
o_n_heap_concatFoldable value =
    [ bgroup
        "concat-foldable"
        [ benchIOSrc parallely "foldMapWith (<>) (List)"
            (sourceFoldMapWith value)
        , benchIOSrc parallely "foldMapWith (<>) (Stream)"
            (sourceFoldMapWithStream value)
        , benchIOSrc parallely "foldMapWithM (<>) (List)"
            (sourceFoldMapWithM value)
        , benchIOSrc parallely "foldMapM (List)" (sourceFoldMapM value)
        ]
    ]

o_n_heap_concat :: Int -> [Benchmark]
o_n_heap_concat value =
    value2 `seq`
        [ bgroup "concat"
            -- This is for comparison with foldMapWith
            [ benchIOSrc serially "concatMapWithId (n of 1) (fromFoldable)"
                (S.concatMapWith parallel id . sourceConcatMapId value)

            , benchIO "concatMapWith (n of 1)"
                  (concatStreamsWith parallel value 1)
            , benchIO "concatMapWith (sqrt x of sqrt x)"
                  (concatStreamsWith parallel value2 value2)
            , benchIO "concatMapWith (1 of n)"
                  (concatStreamsWith parallel 1 value)
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
        [ benchIO "toNullAp" $ toNullAp value parallely
        , benchIO "toNull" $ toNullM value parallely
        , benchIO "toNull3" $ toNullM3 value parallely
        , benchIO "filterAllOut" $ filterAllOutM value parallely
        , benchIO "filterAllIn" $ filterAllInM value parallely
        , benchIO "filterSome" $ filterSome value parallely
        , benchIO "breakAfterSome" $ breakAfterSome value parallely
        ]
    ]

o_n_space_outerProduct :: Int -> [Benchmark]
o_n_space_outerProduct value =
    [ bgroup "monad-outer-product"
        [ benchIO "toList" $ toListM value parallely
        -- XXX disabled due to a bug for now
        -- , benchIO "toListSome" $ toListSome value parallely
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
        [ bgroup (o_1_space_prefix moduleName) (o_1_space_merge_app_tap value)
        , bgroup (o_n_heap_prefix moduleName) $ concat
            [ o_n_heap_generation value
            , o_n_heap_mapping value
            , o_n_heap_concatFoldable value
            , o_n_heap_concat value
            , o_n_heap_outerProduct value
            ]
        , bgroup (o_n_space_prefix moduleName) (o_n_space_outerProduct value)
        ]
