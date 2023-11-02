{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (mapM)

import Data.Function ((&))
import Streamly.Prelude
       ( SerialT, fromParallel, parallel, fromSerial, maxBuffer, maxThreads)

import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.IsStream as Internal

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Test.Tasty.Bench

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

{-# INLINE (|&) #-}
(|&) :: S.MonadAsync m => SerialT m Int -> m ()
(|&) src = src S.|& S.map (+ 1) & S.drain

{-# INLINE (|&.) #-}
(|&.) :: S.MonadAsync m => SerialT m Int -> m ()
(|&.) src = (src S.|&. S.sum) >>= \x -> seq x (return ())

-------------------------------------------------------------------------------
-- Tapping
-------------------------------------------------------------------------------

{-# INLINE tapAsyncS #-}
tapAsyncS :: S.MonadAsync m => Int -> SerialT m Int -> m ()
tapAsyncS n = composeN n $ Internal.tapAsyncK S.sum

{-# INLINE tapAsync #-}
tapAsync :: S.MonadAsync m => Int -> SerialT m Int -> m ()
tapAsync n = composeN n $ Internal.tapAsync FL.sum

o_1_space_merge_app_tap :: Int -> [Benchmark]
o_1_space_merge_app_tap value =
    [ bgroup "merge-app-tap"
        [ benchIOSrc fromSerial "mergeAsyncBy (2,x/2)"
              (mergeAsyncBy (value `div` 2))
        , benchIOSrc fromSerial "mergeAsyncByM (2,x/2)"
              (mergeAsyncByM (value `div` 2))
        -- Parallel stages in a pipeline
        , benchIOSink value "parAppMap" parAppMap
        , benchIOSink value "parAppSum" parAppSum
        , benchIOSink value "(|&)" (|&)
        , benchIOSink value "(|&.)" (|&.)
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
        [ benchIOSrc fromParallel "unfoldr" (sourceUnfoldr value)
        , benchIOSrc fromParallel "unfoldrM" (sourceUnfoldrM value)
        , benchIOSrc fromParallel "fromFoldable" (sourceFromFoldable value)
        , benchIOSrc fromParallel "fromFoldableM" (sourceFromFoldableM value)
        , benchIOSrc fromParallel "unfoldrM maxThreads 1"
              (maxThreads 1 . sourceUnfoldrM value)
        , benchIOSrc fromParallel "unfoldrM maxBuffer 1 (x/10 ops)"
              (maxBuffer 1 . sourceUnfoldrM (value `div` 10))
        ]
    ]

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

o_n_heap_mapping :: Int -> [Benchmark]
o_n_heap_mapping value =
    [ bgroup "mapping"
        [ benchIOSink value "map" $ mapN fromParallel 1
        , benchIOSink value "fmap" $ fmapN fromParallel 1
        , benchIOSink value "mapM" $ mapM fromParallel 1 . fromSerial
        ]
    ]


-------------------------------------------------------------------------------
-- Joining
-------------------------------------------------------------------------------

{-# INLINE parallel2 #-}
parallel2 :: Int -> Int -> IO ()
parallel2 count n =
    S.drain $
        (sourceUnfoldrM count n) `parallel` (sourceUnfoldrM count (n + 1))

o_1_space_joining :: Int -> [Benchmark]
o_1_space_joining value =
    [ bgroup "joining"
        [ benchIOSrc1 "parallel (2 of n/2)" (parallel2 (value `div` 2))
        ]
    ]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

o_n_heap_concatFoldable :: Int -> [Benchmark]
o_n_heap_concatFoldable value =
    [ bgroup
        "concat-foldable"
        [ benchIOSrc fromParallel "foldMapWith (<>) (List)"
            (sourceFoldMapWith value)
        , benchIOSrc fromParallel "foldMapWith (<>) (Stream)"
            (sourceFoldMapWithStream value)
        , benchIOSrc fromParallel "foldMapWithM (<>) (List)"
            (sourceFoldMapWithM value)
        , benchIOSrc fromSerial "S.concatFoldableWith (<>) (List)"
            (concatFoldableWith value)
        , benchIOSrc fromSerial "S.concatForFoldableWith (<>) (List)"
            (concatForFoldableWith value)
        , benchIOSrc fromParallel "foldMapM (List)" (sourceFoldMapM value)
        ]
    ]

o_n_heap_concat :: Int -> [Benchmark]
o_n_heap_concat value =
    value2 `seq`
        [ bgroup "concat"
            -- This is for comparison with foldMapWith
            [ benchIOSrc fromSerial "concatMapWithId (n of 1) (fromFoldable)"
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
        [ benchIO "toNullAp" $ toNullAp value fromParallel
        , benchIO "toNull" $ toNullM value fromParallel
        , benchIO "toNull3" $ toNullM3 value fromParallel
        , benchIO "filterAllOut" $ filterAllOutM value fromParallel
        , benchIO "filterAllIn" $ filterAllInM value fromParallel
        , benchIO "filterSome" $ filterSome value fromParallel
        , benchIO "breakAfterSome" $ breakAfterSome value fromParallel
        ]
    ]

o_n_space_outerProduct :: Int -> [Benchmark]
o_n_space_outerProduct value =
    [ bgroup "monad-outer-product"
        [ benchIO "toList" $ toListM value fromParallel
        -- XXX disabled due to a bug for now
        -- , benchIO "toListSome" $ toListSome value fromParallel
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
            [ o_1_space_merge_app_tap value
            , o_1_space_joining value
            ]
        , bgroup (o_n_heap_prefix moduleName) $ concat
            [ o_n_heap_generation value
            , o_n_heap_mapping value
            , o_n_heap_concatFoldable value
            , o_n_heap_concat value
            , o_n_heap_outerProduct value
            ]
        , bgroup (o_n_space_prefix moduleName) (o_n_space_outerProduct value)
        ]
