{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

module Stream.ConcurrentCommon
    ( allBenchmarks
    )
where

import Stream.Common
    (composeN, benchIO, benchIOSink, benchIOSrc, sourceUnfoldrM)
import Streamly.Data.Stream (Stream)
import Streamly.Internal.Data.Stream.Prelude (MonadAsync, Config)

import qualified Data.List as List
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Fold.Prelude as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.Prelude as Async
import qualified Streamly.Internal.Data.Stream.Prelude as Stream

import Test.Tasty.Bench
import Prelude hiding (mapM)
import Streamly.Benchmark.Common
import Streamly.Data.Stream.MkType

-- XXX Write inspection tests to make sure no dictionaries are being passed
-- around to find specialization issues. Could be really bad for perf.

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

{-# INLINE mapM #-}
mapM ::
       MonadAsync m
    => (Config -> Config)
    -> Int
    -> Stream m Int
    -> m ()
mapM f n = composeN n $ Async.parMapM f return

o_1_space_mapping :: Int -> (Config -> Config) -> [Benchmark]
o_1_space_mapping value f =
    [ bgroup "mapping"
        [ benchIOSink value "mapM" $ mapM f 1
        ]
    ]

-------------------------------------------------------------------------------
-- Size conserving transformations (reordering, buffering, etc.)
-------------------------------------------------------------------------------

o_n_heap_buffering :: Int -> (Config -> Config) -> [Benchmark]
o_n_heap_buffering value f =
    [ bgroup "buffered"
        [ benchIOSink value "mkAsync"
            (Stream.fold Fold.drain . Async.parBuffered f)
        , benchIOSink value "fmap"
            (Stream.fold Fold.drain . fmap (+1) . Async.parBuffered f)
        ]
    ]

-------------------------------------------------------------------------------
-- Joining
-------------------------------------------------------------------------------

{-# INLINE async2 #-}
async2 :: (Config -> Config) -> Int -> Int -> IO ()
async2 f count n =
    Stream.fold Fold.drain
        $ Async.parTwo f
            (sourceUnfoldrM count n) (sourceUnfoldrM count (n + 1))

{-# INLINE concatAsync2 #-}
concatAsync2 :: (Config -> Config) -> Int -> Int -> IO ()
concatAsync2 f count n =
    Stream.fold Fold.drain
        $ Async.parConcat f
        $ Stream.fromList
            [sourceUnfoldrM count n, sourceUnfoldrM count (n + 1)]

{-# INLINE parMergeByM #-}
parMergeByM :: (Config -> Config) -> Int -> Int -> IO ()
parMergeByM f count n =
    Stream.fold Fold.drain
        $ Async.parMergeByM f
        (\a b -> return (a `compare` b))
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

{-# INLINE parMergeBy #-}
parMergeBy :: (Config -> Config) -> Int -> Int -> IO ()
parMergeBy f count n =
    Stream.fold Fold.drain
        $ Async.parMergeBy f
        compare
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

{-# INLINE parZipWithM #-}
parZipWithM :: (Config -> Config) -> Int -> Int -> IO ()
parZipWithM f count n =
    Stream.fold Fold.drain
        $ Async.parZipWithM f
        (curry return)
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

{-# INLINE parZipWith #-}
parZipWith :: (Config -> Config) -> Int -> Int -> IO ()
parZipWith f count n =
    Stream.fold Fold.drain
        $ Async.parZipWith f
        (,)
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

{-# INLINE parTap #-}
parTap :: (Fold.Config -> Fold.Config) -> Int -> Int -> IO ()
parTap f count n =
    Stream.fold Fold.drain
        $ Stream.tap (Fold.parBuffered f Fold.sum) (sourceUnfoldrM count n)

o_1_space_joining :: Int -> (Config -> Config) -> [Benchmark]
o_1_space_joining value f =
    [ bgroup "joining (2 of n/2)"
        [ benchIOSrc1 "parTwo" (async2 f (value `div` 2))
        , benchIOSrc1 "parConcat" (concatAsync2 f (value `div` 2))
        , benchIOSrc1 "parMergeByM" (parMergeByM f (value `div` 2))
        , benchIOSrc1 "parMergeBy" (parMergeBy f (value `div` 2))
        , benchIOSrc1 "parZipWithM" (parZipWithM f (value `div` 2))
        , benchIOSrc1 "parZipWith" (parZipWith f (value `div` 2))
        ]
    -- XXX use configurable modifier, put this in concurrent fold benchmarks
    , benchIOSrc1 "tap (Fold.parBuffered id Fold.sum)" (parTap id value)
    ]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

{-# INLINE sourceFoldMapWith #-}
sourceFoldMapWith :: (Config -> Config) -> Int -> Int -> Stream IO Int
sourceFoldMapWith f value n =
    Async.parConcatMap f Stream.fromPure $ Stream.fromList [n..n+value]

{-# INLINE sourceFoldMapWithStream #-}
sourceFoldMapWithStream :: (Config -> Config) -> Int -> Int -> Stream IO Int
sourceFoldMapWithStream f value n =
    Async.parConcatMap f Stream.fromPure
        $ Stream.enumerateFromTo n (n + value)

{-# INLINE concatFoldableWith #-}
concatFoldableWith :: (Config -> Config) -> Int -> Int -> Stream IO Int
concatFoldableWith f value n =
    let step x =
            if x <= n + value
            then Just (Stream.fromPure x, x + 1)
            else Nothing
        list = List.unfoldr step n
     in Async.parConcat f (Stream.fromList  list)

o_1_space_concatFoldable :: Int -> (Config -> Config) -> [Benchmark]
o_1_space_concatFoldable value f =
    [ bgroup "concat-foldable"
        [ benchIOSrc "foldMapWith (<>) (List)"
            (sourceFoldMapWith f value)
        , benchIOSrc "foldMapWith (<>) (Stream)"
            (sourceFoldMapWithStream f value)
        , benchIOSrc "S.concatFoldableWith (<>) (List)"
            (concatFoldableWith f value)
        ]
    ]

{-# INLINE concatMapStreamsWith #-}
concatMapStreamsWith
    :: (Config -> Config)
    -> Int
    -> Int
    -> Int
    -> IO ()
concatMapStreamsWith f outer inner n =
    Stream.fold Fold.drain
        $ Async.parConcatMap f (sourceUnfoldrM inner) (sourceUnfoldrM outer n)

{-# INLINE concatFmapStreamsWith #-}
concatFmapStreamsWith
    :: (Config -> Config)
    -> Int
    -> Int
    -> Int
    -> IO ()
concatFmapStreamsWith f outer inner n =
    Stream.fold Fold.drain
        $ Async.parConcat f
        $ fmap (sourceUnfoldrM inner) (sourceUnfoldrM outer n)

o_1_space_concatMap :: String -> Int -> (Config -> Config) -> [Benchmark]
o_1_space_concatMap label value f =
    value2 `seq`
        [ bgroup ("concat" ++ label)
            [ benchIO "parConcatMap (n of 1)"
                  (concatMapStreamsWith f value 1)
            , benchIO "parConcatMap (sqrt x of sqrt x)"
                  (concatMapStreamsWith f value2 value2)
            , benchIO "parConcatMap (1 of n)"
                  (concatMapStreamsWith f 1 value)
            , benchIO "concat . fmap (n of 1)"
                  (concatFmapStreamsWith f value 1)
            ]
        ]

    where

    value2 = round $ sqrt (fromIntegral value :: Double)

-------------------------------------------------------------------------------
-- Monadic outer product
-------------------------------------------------------------------------------

{-# INLINE drainApply #-}
drainApply :: (Config -> Config) -> Int -> Int -> IO ()
drainApply f linearCount start =
    Stream.fold Fold.drain
        $ Async.parCrossApply f
            (fmap (+) (sourceUnfoldrM nestedCount2 start))
            (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

-- XXX Move to a Zip specific module so that it is not added to all concurrent
-- stream types.
parCrossApply :: MonadAsync m => Stream m (a -> b) -> Stream m a -> Stream m b
parCrossApply = Stream.parCrossApply id

$(mkZipType "ParZip" "parCrossApply" True)

{-# INLINE zipApplicative #-}
zipApplicative
    :: MonadAsync m => Int -> Int -> m ()
zipApplicative count start =
    Stream.fold Fold.drain $ unParZip $
        (+) <$> mkParZip (sourceUnfoldrM count start)
            <*> mkParZip (sourceUnfoldrM count (start + 1))

o_1_space_outerProduct :: Int -> (Config -> Config) -> [Benchmark]
o_1_space_outerProduct value f =
    [ bgroup "outer-product"
        [ benchIO "parCrossApply" $ drainApply f value
        , benchIO "ZipApplicative" $ zipApplicative value
        ]
    ]

-------------------------------------------------------------------------------
-- Benchmark sets
-------------------------------------------------------------------------------

allBenchmarks :: String -> Bool -> (Config -> Config) -> Int -> [Benchmark]
allBenchmarks moduleName wide modifier value =
    [ bgroup (o_1_space_prefix moduleName) $ concat
        [ o_1_space_mapping value modifier
        , o_1_space_concatFoldable value modifier
        , o_1_space_concatMap "" value modifier
        , o_1_space_concatMap "-maxThreads-1" value (modifier . Async.maxThreads 1)
        , o_1_space_concatMap "-maxBuffer-1 1/10" (value `div` 10) (modifier . Async.maxBuffer 1)
        , o_1_space_concatMap "-rate-Nothing" value (modifier . Async.rate Nothing)
        , o_1_space_joining value modifier
        ] ++ if wide then [] else o_1_space_outerProduct value modifier
    , bgroup (o_n_heap_prefix moduleName) $ concat
        [ if wide then o_1_space_outerProduct value modifier else []
        , o_n_heap_buffering value modifier
        ]
    ]
