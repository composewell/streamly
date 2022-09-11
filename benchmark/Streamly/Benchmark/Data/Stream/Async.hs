{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Stream.Common
    (composeN, benchIO, benchIOSink, benchIOSrc, sourceUnfoldrM)
import Streamly.Data.Stream (Stream)
import Streamly.Internal.Data.Stream.Async (MonadAsync)

import qualified Data.List as List
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.Async as Async

import Gauge
import Prelude hiding (mapM)
import Streamly.Benchmark.Common

moduleName :: String
moduleName = "Data.Stream.Async"

-- XXX Write inspection tests to make sure no dictionaries are being passed
-- around to find specialization issues. Could be really bad for perf.

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

{-# INLINE mapM #-}
mapM ::
       MonadAsync m
    => Int
    -> Stream m Int
    -> m ()
mapM n = composeN n $ Async.mapM return

o_1_space_mapping :: Int -> [Benchmark]
o_1_space_mapping value =
    [ bgroup "mapping"
        [ benchIOSink value "mapM" $ mapM 1
        ]
    ]

-------------------------------------------------------------------------------
-- Size conserving transformations (reordering, buffering, etc.)
-------------------------------------------------------------------------------

o_n_heap_buffering :: Int -> [Benchmark]
o_n_heap_buffering value =
    [ bgroup "buffered"
        [benchIOSink value "mkAsync" (Stream.fold Fold.drain . Async.eval)]
    ]

-------------------------------------------------------------------------------
-- Joining
-------------------------------------------------------------------------------

{-# INLINE async2 #-}
async2 :: Int -> Int -> IO ()
async2 count n =
    Stream.fold Fold.drain $
        sourceUnfoldrM count n `Async.append` sourceUnfoldrM count (n + 1)

o_1_space_joining :: Int -> [Benchmark]
o_1_space_joining value =
    [ bgroup "joining"
        [ benchIOSrc1 "async (2 of n/2)" (async2 (value `div` 2))
        ]
    ]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

{-# INLINE sourceFoldMapWith #-}
sourceFoldMapWith :: Int -> Int -> Stream IO Int
sourceFoldMapWith value n =
    Async.concatMap Stream.fromPure $ Stream.fromList [n..n+value]

{-# INLINE sourceFoldMapWithStream #-}
sourceFoldMapWithStream :: Int -> Int -> Stream IO Int
sourceFoldMapWithStream value n =
    Async.concatMap Stream.fromPure $ Stream.enumerateFromTo n (n + value)

{-# INLINE concatFoldableWith #-}
concatFoldableWith :: Int -> Int -> Stream IO Int
concatFoldableWith value n =
    let step x =
            if x <= n + value
            then Just (Stream.fromPure x, x + 1)
            else Nothing
        list = List.unfoldr step n
     in Async.concatList list

o_1_space_concatFoldable :: Int -> [Benchmark]
o_1_space_concatFoldable value =
    [ bgroup "concat-foldable"
        [ benchIOSrc "foldMapWith (<>) (List)"
            (sourceFoldMapWith value)
        , benchIOSrc "foldMapWith (<>) (Stream)"
            (sourceFoldMapWithStream value)
        , benchIOSrc "S.concatFoldableWith (<>) (List)"
            (concatFoldableWith value)
        ]
    ]

{-# INLINE concatMapStreamsWith #-}
concatMapStreamsWith
    :: Int
    -> Int
    -> Int
    -> IO ()
concatMapStreamsWith outer inner n =
    Stream.fold Fold.drain
        $ Async.concatMap (sourceUnfoldrM inner) (sourceUnfoldrM outer n)

{-# INLINE concatFmapStreamsWith #-}
concatFmapStreamsWith
    :: Int
    -> Int
    -> Int
    -> IO ()
concatFmapStreamsWith outer inner n =
    Stream.fold Fold.drain
        $ Async.concat
        $ fmap (sourceUnfoldrM inner) (sourceUnfoldrM outer n)

o_1_space_concatMap :: Int -> [Benchmark]
o_1_space_concatMap value =
    value2 `seq`
        [ bgroup "concat"
            [ benchIO "concatMapWith (n of 1)"
                  (concatMapStreamsWith value 1)
            , benchIO "concatMapWith (sqrt x of sqrt x)"
                  (concatMapStreamsWith value2 value2)
            , benchIO "concatMapWith (1 of n)"
                  (concatMapStreamsWith 1 value)
            , benchIO "concat . fmap (n of 1)"
                  (concatFmapStreamsWith value 1)
            ]
        ]

    where

    value2 = round $ sqrt (fromIntegral value :: Double)

-------------------------------------------------------------------------------
-- Monadic outer product
-------------------------------------------------------------------------------

{-# INLINE toNullAp #-}
toNullAp :: Int -> Int -> IO ()
toNullAp linearCount start =
    Stream.fold Fold.drain
        $ Async.apply
            (fmap (+) (sourceUnfoldrM nestedCount2 start))
            (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

o_1_space_outerProduct :: Int -> [Benchmark]
o_1_space_outerProduct value =
    [ bgroup "monad-outer-product"
        [ benchIO "toNullAp" $ toNullAp value
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
            [ o_1_space_mapping value
            , o_1_space_concatFoldable value
            , o_1_space_concatMap value
            , o_1_space_outerProduct value
            , o_1_space_joining value
            ]
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_buffering value)
        ]
