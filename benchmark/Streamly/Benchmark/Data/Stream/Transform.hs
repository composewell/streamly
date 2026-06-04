-- |
-- Module      : Stream.Transform
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-orphans #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Stream.Transform (benchmarks) where

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection
#endif

import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold

import Test.Tasty.Bench
import Stream.Common hiding (scanl')
import Streamly.Benchmark.Common
import Prelude hiding (sequence, mapM)

-------------------------------------------------------------------------------
-- Pipelines (stream-to-stream transformations)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- one-to-one transformations
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- maps and scans
-------------------------------------------------------------------------------

{-# INLINE scanl' #-}
scanl' :: MonadIO m => Int -> Stream m Int -> m ()
scanl' n = composeN n $ Stream.scanl' (+) 0

{-# INLINE scanlM' #-}
scanlM' :: MonadIO m => Int -> Stream m Int -> m ()
scanlM' n = composeN n $ Stream.scanlM' (\b a -> return $ b + a) (return 0)

{-# INLINE scanl1' #-}
scanl1' :: MonadIO m => Int -> Stream m Int -> m ()
scanl1' n = composeN n $ Stream.scanl1' (+)

{-# INLINE scanl1M' #-}
scanl1M' :: MonadIO m => Int -> Stream m Int -> m ()
scanl1M' n = composeN n $ Stream.scanl1M' (\b a -> return $ b + a)

{-# INLINE scan #-}
scan :: MonadIO m => Int -> Stream m Int -> m ()
scan n = composeN n $ Stream.scanl Scanl.sum

{-# INLINE postscan #-}
postscan :: MonadIO m => Int -> Stream m Int -> m ()
postscan n = composeN n $ Stream.postscanl Scanl.sum

{-# INLINE postscanl' #-}
postscanl' :: MonadIO m => Int -> Stream m Int -> m ()
postscanl' n = composeN n $ Stream.postscanl' (+) 0

{-# INLINE postscanlM' #-}
postscanlM' :: MonadIO m => Int -> Stream m Int -> m ()
postscanlM' n = composeN n $ Stream.postscanlM' (\b a -> return $ b + a) (return 0)

{-# INLINE sequence #-}
sequence :: MonadAsync m => Stream m (m Int) -> m ()
sequence = Common.drain . Stream.sequence

{-# INLINE tap #-}
tap :: MonadIO m => Int -> Stream m Int -> m ()
tap n = composeN n $ Stream.tap FL.sum

{-# INLINE _timestamped #-}
_timestamped :: MonadIO m => Stream m Int -> m ()
_timestamped = Stream.drain . Stream.timestamped
{-
{-# INLINE foldrT #-}
foldrT :: MonadIO m => Int -> Stream m Int -> m ()
foldrT n = composeN n (unCrossStream . Stream.foldrT cns (CrossStream Stream.nil))

    where cns x (CrossStream xs) = CrossStream (Stream.cons x xs)

{-# INLINE foldrTMap #-}
foldrTMap :: MonadIO m => Int -> Stream m Int -> m ()
foldrTMap n = composeN n $ Stream.foldrT (\x xs -> x + 1 `Stream.cons` xs) Stream.nil
-}

{-# INLINE trace #-}
trace :: MonadAsync m => Int -> Stream m Int -> m ()
trace n = composeN n $ Stream.trace return

o_1_space_mapping :: Int -> [Benchmark]
o_1_space_mapping value =
    [ bgroup
        "mapping"
        [
        -- , benchIOSink value "foldrT" (foldrT 1)
        -- , benchIOSink value "foldrTMap" (foldrTMap 1)

        -- Mapping
          benchIOSink value "map" (mapN 1)
        , bench "sequence" $ nfIO $ randomRIO (1, 1000) >>= \n ->
              sequence (sourceUnfoldrAction value n)
        , benchIOSink value "mapM" (mapM 1)
        , benchIOSink value "tap" (tap 1)
        -- XXX tasty-bench hangs benchmarking this
        -- , benchIOSink value "timestamped" _timestamped
        -- Scanning
        , benchIOSink value "scanl'" (scanl' 1)
        , benchIOSink value "scanl1'" (scanl1' 1)
        , benchIOSink value "scanlM'" (scanlM' 1)
        , benchIOSink value "scanl1M'" (scanl1M' 1)
        , benchIOSink value "postscanl'" (postscanl' 1)
        , benchIOSink value "postscanlM'" (postscanlM' 1)
        , benchIOSink value "scan" (scan 1)
        , benchIOSink value "postscan" (postscan 1)
        ]
    ]

o_1_space_mappingX4 :: Int -> [Benchmark]
o_1_space_mappingX4 value =
    [ bgroup "mappingX4"
        [ benchIOSink value "map" (mapN 4)
        , benchIOSink value "mapM" (mapM 4)
        , benchIOSink value "trace" (trace 4)

        , benchIOSink value "scanl'" (scanl' 4)
        , benchIOSink value "scanl1'" (scanl1' 4)
        , benchIOSink value "scanlM'" (scanlM' 4)
        , benchIOSink value "scanl1M'" (scanl1M' 4)
        , benchIOSink value "postscanl'" (postscanl' 4)
        , benchIOSink value "postscanlM'" (postscanlM' 4)
        , benchIOSink value "scan" (scan 4)
        , benchIOSink value "postscan" (postscan 4)
{-
        -- XXX this is horribly slow
        , let value16 = round (fromIntegral value**(1/16::Double))
           benchFold "concatMap" (concatMap 4) (sourceUnfoldrMN value16)
-}
        ]
    ]

{-# INLINE sieveScan #-}
sieveScan :: Monad m => Stream m Int -> Stream m Int
sieveScan =
      Stream.mapMaybe snd
    . Stream.scanl (Scanl.scanlM' (\(primes, _) n -> do
            return $
                let ps = takeWhile (\p -> p * p <= n) primes
                 in if all (\p -> n `mod` p /= 0) ps
                    then (primes ++ [n], Just n)
                    else (primes, Nothing)) (return ([2], Just 2)))

o_n_space_mapping :: Int -> [Benchmark]
o_n_space_mapping value =
    [ bgroup "mapping"
        [
          benchIO "naive prime sieve"
            (\n -> Stream.fold FL.sum $ sieveScan $ Stream.enumerateFromTo 2 (value + n))
        ]
    ]

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

o_1_space_functor :: Int -> [Benchmark]
o_1_space_functor value =
    [ bgroup "Functor"
        [ benchIOSink value "fmap" (mapN 1)
        , benchIOSink value "fmap x 4" (mapN 4)
        ]
    ]

-------------------------------------------------------------------------------
-- Iteration/looping utilities
-------------------------------------------------------------------------------

{-# INLINE iterateN #-}
iterateN :: (Int -> a -> a) -> a -> Int -> a
iterateN g initial count = f count initial

    where

    f (0 :: Int) x = x
    f i x = f (i - 1) (g i x)

-- Iterate a transformation over a singleton stream
{-# INLINE iterateSingleton #-}
iterateSingleton :: Applicative m =>
       (Int -> Stream m Int -> Stream m Int)
    -> Int
    -> Int
    -> Stream m Int
iterateSingleton g count n = iterateN g (Stream.fromPure n) count

{-
-- XXX need to check why this is slower than the explicit recursion above, even
-- if the above code is written in a foldr like head recursive way. We also
-- need to try this with foldlM' once #150 is fixed.
-- However, it is perhaps best to keep the iteration benchmarks independent of
-- foldrM and any related fusion issues.
{-# INLINE _iterateSingleton #-}
_iterateSingleton ::
       Monad m
    => (Int -> Stream m Int -> Stream m Int)
    -> Int
    -> Int
    -> Stream m Int
_iterateSingleton g value n = S.foldrM g (return n) $ sourceIntFromTo value n
-}

o_n_space_iterated :: Int -> [Benchmark]
o_n_space_iterated value =
    [ bgroup "iterated"
        [ benchIO "(+) (n times) (baseline)" $ \i0 ->
            iterateN (\i acc -> acc >>= \n -> return $ i + n) (return i0) value
        , benchIOSrc "(<$) (n times)" $
            iterateSingleton (<$) value
        , benchIOSrc "fmap (n times)" $
            iterateSingleton (fmap . (+)) value
        {-
        , benchIOSrc fromSerial "_(<$) (n times)" $
            _iterateSingleton (<$) value
        , benchIOSrc fromSerial "_fmap (n times)" $
            _iterateSingleton (fmap . (+)) value
        -}
        ]
    ]

-------------------------------------------------------------------------------
-- Size reducing transformations (filtering)
-------------------------------------------------------------------------------

{-# INLINE filterEven #-}
filterEven :: MonadIO m => Int -> Stream m Int -> m ()
filterEven n = composeN n $ Stream.filter even

{-# INLINE filterAllOut #-}
filterAllOut :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterAllOut value n = composeN n $ Stream.filter (> (value + 1))

{-# INLINE filterAllIn #-}
filterAllIn :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterAllIn value n = composeN n $ Stream.filter (<= (value + 1))

{-# INLINE filterMEven #-}
filterMEven :: MonadIO m => Int -> Stream m Int -> m ()
filterMEven n = composeN n $ Stream.filterM (return . even)

{-# INLINE filterMAllOut #-}
filterMAllOut :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterMAllOut value n = composeN n $ Stream.filterM (\x -> return $ x > (value + 1))

{-# INLINE filterMAllIn #-}
filterMAllIn :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterMAllIn value n = composeN n $ Stream.filterM (\x -> return $ x <= (value + 1))

{-# INLINE _takeOne #-}
_takeOne :: MonadIO m => Int -> Stream m Int -> m ()
_takeOne n = composeN n $ Stream.take 1

{-# INLINE takeAll #-}
takeAll :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeAll value n = composeN n $ Stream.take (value + 1)

{-# INLINE takeWhileTrue #-}
takeWhileTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeWhileTrue value n = composeN n $ Stream.takeWhile (<= (value + 1))

{-# INLINE takeWhileMTrue #-}
takeWhileMTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeWhileMTrue value n = composeN n $ Stream.takeWhileM (return . (<= (value + 1)))

{-# INLINE dropOne #-}
dropOne :: MonadIO m => Int -> Stream m Int -> m ()
dropOne n = composeN n $ Stream.drop 1

{-# INLINE dropAll #-}
dropAll :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropAll value n = composeN n $ Stream.drop (value + 1)

{-# INLINE dropWhileTrue #-}
dropWhileTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropWhileTrue value n = composeN n $ Stream.dropWhile (<= (value + 1))

{-# INLINE dropWhileMTrue #-}
dropWhileMTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropWhileMTrue value n = composeN n $ Stream.dropWhileM (return . (<= (value + 1)))

{-# INLINE dropWhileFalse #-}
dropWhileFalse :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropWhileFalse value n = composeN n $ Stream.dropWhile (> (value + 1))

{-# INLINE findIndices #-}
findIndices :: MonadIO m => Int -> Int -> Stream m Int -> m ()
findIndices value n = composeN n $ Stream.findIndices (== (value + 1))

{-# INLINE elemIndices #-}
elemIndices :: MonadIO m => Int -> Int -> Stream m Int -> m ()
elemIndices value n = composeN n $ Stream.elemIndices (value + 1)

{-# INLINE deleteBy #-}
deleteBy :: MonadIO m => Int -> Int -> Stream m Int -> m ()
deleteBy value n = composeN n $ Stream.deleteBy (>=) (value + 1)

-- uniq . uniq == uniq, composeN 2 ~ composeN 1
{-# INLINE uniq #-}
uniq :: MonadIO m => Int -> Stream m Int -> m ()
uniq n = composeN n Stream.uniq

{-# INLINE mapMaybe #-}
mapMaybe :: MonadIO m => Int -> Stream m Int -> m ()
mapMaybe n =
    composeN n $
    Stream.mapMaybe
        (\x ->
             if odd x
             then Nothing
             else Just x)

{-# INLINE mapMaybeM #-}
mapMaybeM :: MonadAsync m => Int -> Stream m Int -> m ()
mapMaybeM n =
    composeN n $
    Stream.mapMaybeM
        (\x ->
             if odd x
             then return Nothing
             else return $ Just x)

o_1_space_filtering :: Int -> [Benchmark]
o_1_space_filtering value =
    [ bgroup "filtering"
        [ benchIOSink value "filter-even" (filterEven 1)
        , benchIOSink value "filter-all-out" (filterAllOut value 1)
        , benchIOSink value "filter-all-in" (filterAllIn value 1)

        , benchIOSink value "filterM-even" (filterMEven 1)
        , benchIOSink value "filterM-all-out" (filterMAllOut value 1)
        , benchIOSink value "filterM-all-in" (filterMAllIn value 1)

        -- Trimming
        , benchIOSink value "take-all" (takeAll value 1)
        , benchIOSink value "takeWhile-true" (takeWhileTrue value 1)
     -- , benchIOSink value "takeWhileM-true" (_takeWhileMTrue value 1)
        , benchIOSink value "drop-one" (dropOne 1)
        , benchIOSink value "drop-all" (dropAll value 1)
        , benchIOSink value "dropWhile-true" (dropWhileTrue value 1)
     -- , benchIOSink value "dropWhileM-true" (_dropWhileMTrue value 1)
        , benchIOSink
              value
              "dropWhile-false"
              (dropWhileFalse value 1)
        , benchIOSink value "deleteBy" (deleteBy value 1)

        , benchIOSink value "uniq" (uniq 1)

        -- Map and filter
        , benchIOSink value "mapMaybe" (mapMaybe 1)
        , benchIOSink value "mapMaybeM" (mapMaybeM 1)

        -- Searching (stateful map and filter)
        , benchIOSink value "findIndices" (findIndices value 1)
        , benchIOSink value "elemIndices" (elemIndices value 1)
        ]
    ]

o_1_space_filteringX4 :: Int -> [Benchmark]
o_1_space_filteringX4 value =
    [ bgroup "filteringX4"
        [ benchIOSink value "filter-even" (filterEven 4)
        , benchIOSink value "filter-all-out" (filterAllOut value 4)
        , benchIOSink value "filter-all-in" (filterAllIn value 4)

        , benchIOSink value "filterM-even" (filterMEven 4)
        , benchIOSink value "filterM-all-out" (filterMAllOut value 4)
        , benchIOSink value "filterM-all-in" (filterMAllIn value 4)

        -- trimming
        , benchIOSink value "take-all" (takeAll value 4)
        , benchIOSink value "takeWhile-true" (takeWhileTrue value 4)
        , benchIOSink value "takeWhileM-true" (takeWhileMTrue value 4)
        , benchIOSink value "drop-one" (dropOne 4)
        , benchIOSink value "drop-all" (dropAll value 4)
        , benchIOSink value "dropWhile-true" (dropWhileTrue value 4)
        , benchIOSink value "dropWhileM-true" (dropWhileMTrue value 4)
        -- XXX requires @-fspec-constr-recursive=12@.
        , benchIOSink
              value
              "dropWhile-false"
              (dropWhileFalse value 4)
        , benchIOSink value "deleteBy" (deleteBy value 4)

        , benchIOSink value "uniq" (uniq 4)

        -- map and filter
        , benchIOSink value "mapMaybe" (mapMaybe 4)
        , benchIOSink value "mapMaybeM" (mapMaybeM 4)

        -- searching
        , benchIOSink value "findIndices" (findIndices value 4)
        , benchIOSink value "elemIndices" (elemIndices value 4)
        ]
    ]

-------------------------------------------------------------------------------
-- Size increasing transformations (insertions)
-------------------------------------------------------------------------------

{-# INLINE intersperse #-}
intersperse :: MonadAsync m => Int -> Int -> Stream m Int -> m ()
intersperse value n = composeN n $ Stream.intersperse (value + 1)

{-# INLINE intersperseM #-}
intersperseM :: MonadAsync m => Int -> Int -> Stream m Int -> m ()
intersperseM value n = composeN n $ Stream.intersperseM (return $ value + 1)

{-# INLINE insertBy #-}
insertBy :: MonadIO m => Int -> Int -> Stream m Int -> m ()
insertBy value n = composeN n $ Stream.insertBy compare (value + 1)

{-# INLINE interposeSuffix #-}
interposeSuffix :: Monad m => Int -> Int -> Stream m Int -> m ()
interposeSuffix value n =
    composeN n $ Stream.unfoldEachSepBy (value + 1) Unfold.identity

{-# INLINE intercalateSuffix #-}
intercalateSuffix :: Monad m => Int -> Int -> Stream m Int -> m ()
intercalateSuffix value n =
    composeN n $ Stream.unfoldEachSepBySeq (value + 1) Unfold.identity

o_1_space_inserting :: Int -> [Benchmark]
o_1_space_inserting value =
    [ bgroup "inserting"
        [ benchIOSink value "intersperse" (intersperse value 1)
        , benchIOSink value "intersperseM" (intersperseM value 1)
        , benchIOSink value "insertBy" (insertBy value 1)
        , benchIOSink value "interposeSuffix" (interposeSuffix value 1)
        , benchIOSink value "intercalateSuffix" (intercalateSuffix value 1)
        , benchIOSinkPureSrc value "interspersePure" (intersperse value 1)
        ]
    ]

o_1_space_insertingX4 :: Int -> [Benchmark]
o_1_space_insertingX4 value =
    [ bgroup "insertingX4"
        [
          -- XXX requires @-fspec-constr-recursive=16@.
          benchIOSink value "intersperse" (intersperse value 4)
        , benchIOSink value "insertBy" (insertBy value 4)
        ]
    ]

-------------------------------------------------------------------------------
-- Indexing
-------------------------------------------------------------------------------

{-# INLINE indexed #-}
indexed :: MonadIO m => Int -> Stream m Int -> m ()
indexed n = composeN n (fmap snd . Stream.indexed)

{-# INLINE indexedR #-}
indexedR :: MonadIO m => Int -> Int -> Stream m Int -> m ()
indexedR value n = composeN n (fmap snd . Stream.indexedR value)

o_1_space_indexing :: Int -> [Benchmark]
o_1_space_indexing value =
    [ bgroup "indexing"
        [ benchIOSink value "indexed" (indexed 1)
        , benchIOSink value "indexedR" (indexedR value 1)
        ]
    ]

o_1_space_indexingX4 :: Int -> [Benchmark]
o_1_space_indexingX4 value =
    [ bgroup "indexingx4"
        [ benchIOSink value "indexed" (indexed 4)
        , benchIOSink value "indexedR" (indexedR value 4)
        ]
    ]

-------------------------------------------------------------------------------
-- Inspection
-------------------------------------------------------------------------------

#ifdef INSPECTION
-- The transformations above take an abstract input 'Stream', so a 'Step'-free
-- core can only be checked on a complete pipeline. We bake in a concrete
-- 'sourceUnfoldrM' source (the same one 'benchIOSink' supplies) and a single
-- compose, then assert that the whole generate+transform+drain pipeline fully
-- fuses: no 'Step' constructors remain in the optimized core. Combinators that
-- buffer or recurse through opaque state keep their 'Step' constructors; those
-- checks are kept but commented out.

-- maps and scans
{-# INLINE inspMap #-}
inspMap :: Int -> Int -> IO ()
inspMap value n = mapN 1 (sourceUnfoldrM value n)

{-# INLINE inspMapM #-}
inspMapM :: Int -> Int -> IO ()
inspMapM value n = mapM 1 (sourceUnfoldrM value n)

{-# INLINE inspSequence #-}
inspSequence :: Int -> Int -> IO ()
inspSequence value n = sequence (sourceUnfoldrAction value n)

{-# INLINE inspTap #-}
inspTap :: Int -> Int -> IO ()
inspTap value n = tap 1 (sourceUnfoldrM value n)

{-# INLINE inspTrace #-}
inspTrace :: Int -> Int -> IO ()
inspTrace value n = trace 1 (sourceUnfoldrM value n)

{-# INLINE inspScanl' #-}
inspScanl' :: Int -> Int -> IO ()
inspScanl' value n = scanl' 1 (sourceUnfoldrM value n)

{-# INLINE inspScanl1' #-}
inspScanl1' :: Int -> Int -> IO ()
inspScanl1' value n = scanl1' 1 (sourceUnfoldrM value n)

{-# INLINE inspScanlM' #-}
inspScanlM' :: Int -> Int -> IO ()
inspScanlM' value n = scanlM' 1 (sourceUnfoldrM value n)

{-# INLINE inspScanl1M' #-}
inspScanl1M' :: Int -> Int -> IO ()
inspScanl1M' value n = scanl1M' 1 (sourceUnfoldrM value n)

{-# INLINE inspScan #-}
inspScan :: Int -> Int -> IO ()
inspScan value n = scan 1 (sourceUnfoldrM value n)

{-# INLINE inspPostscan #-}
inspPostscan :: Int -> Int -> IO ()
inspPostscan value n = postscan 1 (sourceUnfoldrM value n)

{-# INLINE inspPostscanl' #-}
inspPostscanl' :: Int -> Int -> IO ()
inspPostscanl' value n = postscanl' 1 (sourceUnfoldrM value n)

{-# INLINE inspPostscanlM' #-}
inspPostscanlM' :: Int -> Int -> IO ()
inspPostscanlM' value n = postscanlM' 1 (sourceUnfoldrM value n)

-- filtering
{-# INLINE inspFilterEven #-}
inspFilterEven :: Int -> Int -> IO ()
inspFilterEven value n = filterEven 1 (sourceUnfoldrM value n)

{-# INLINE inspFilterAllOut #-}
inspFilterAllOut :: Int -> Int -> IO ()
inspFilterAllOut value n = filterAllOut value 1 (sourceUnfoldrM value n)

{-# INLINE inspFilterAllIn #-}
inspFilterAllIn :: Int -> Int -> IO ()
inspFilterAllIn value n = filterAllIn value 1 (sourceUnfoldrM value n)

{-# INLINE inspFilterMEven #-}
inspFilterMEven :: Int -> Int -> IO ()
inspFilterMEven value n = filterMEven 1 (sourceUnfoldrM value n)

{-# INLINE inspFilterMAllOut #-}
inspFilterMAllOut :: Int -> Int -> IO ()
inspFilterMAllOut value n = filterMAllOut value 1 (sourceUnfoldrM value n)

{-# INLINE inspFilterMAllIn #-}
inspFilterMAllIn :: Int -> Int -> IO ()
inspFilterMAllIn value n = filterMAllIn value 1 (sourceUnfoldrM value n)

{-# INLINE inspTakeAll #-}
inspTakeAll :: Int -> Int -> IO ()
inspTakeAll value n = takeAll value 1 (sourceUnfoldrM value n)

{-# INLINE inspTakeWhileTrue #-}
inspTakeWhileTrue :: Int -> Int -> IO ()
inspTakeWhileTrue value n = takeWhileTrue value 1 (sourceUnfoldrM value n)

{-# INLINE inspTakeWhileMTrue #-}
inspTakeWhileMTrue :: Int -> Int -> IO ()
inspTakeWhileMTrue value n = takeWhileMTrue value 1 (sourceUnfoldrM value n)

{-# INLINE inspDropOne #-}
inspDropOne :: Int -> Int -> IO ()
inspDropOne value n = dropOne 1 (sourceUnfoldrM value n)

{-# INLINE inspDropAll #-}
inspDropAll :: Int -> Int -> IO ()
inspDropAll value n = dropAll value 1 (sourceUnfoldrM value n)

{-# INLINE inspDropWhileTrue #-}
inspDropWhileTrue :: Int -> Int -> IO ()
inspDropWhileTrue value n = dropWhileTrue value 1 (sourceUnfoldrM value n)

{-# INLINE inspDropWhileMTrue #-}
inspDropWhileMTrue :: Int -> Int -> IO ()
inspDropWhileMTrue value n = dropWhileMTrue value 1 (sourceUnfoldrM value n)

{-# INLINE inspDropWhileFalse #-}
inspDropWhileFalse :: Int -> Int -> IO ()
inspDropWhileFalse value n = dropWhileFalse value 1 (sourceUnfoldrM value n)

{-# INLINE inspDeleteBy #-}
inspDeleteBy :: Int -> Int -> IO ()
inspDeleteBy value n = deleteBy value 1 (sourceUnfoldrM value n)

{-# INLINE inspUniq #-}
inspUniq :: Int -> Int -> IO ()
inspUniq value n = uniq 1 (sourceUnfoldrM value n)

{-# INLINE inspMapMaybe #-}
inspMapMaybe :: Int -> Int -> IO ()
inspMapMaybe value n = mapMaybe 1 (sourceUnfoldrM value n)

{-# INLINE inspMapMaybeM #-}
inspMapMaybeM :: Int -> Int -> IO ()
inspMapMaybeM value n = mapMaybeM 1 (sourceUnfoldrM value n)

{-# INLINE inspFindIndices #-}
inspFindIndices :: Int -> Int -> IO ()
inspFindIndices value n = findIndices value 1 (sourceUnfoldrM value n)

{-# INLINE inspElemIndices #-}
inspElemIndices :: Int -> Int -> IO ()
inspElemIndices value n = elemIndices value 1 (sourceUnfoldrM value n)

-- inserting
{-# INLINE inspIntersperse #-}
inspIntersperse :: Int -> Int -> IO ()
inspIntersperse value n = intersperse value 1 (sourceUnfoldrM value n)

{-# INLINE inspIntersperseM #-}
inspIntersperseM :: Int -> Int -> IO ()
inspIntersperseM value n = intersperseM value 1 (sourceUnfoldrM value n)

{-# INLINE inspInsertBy #-}
inspInsertBy :: Int -> Int -> IO ()
inspInsertBy value n = insertBy value 1 (sourceUnfoldrM value n)

{-# INLINE inspInterposeSuffix #-}
inspInterposeSuffix :: Int -> Int -> IO ()
inspInterposeSuffix value n = interposeSuffix value 1 (sourceUnfoldrM value n)

{-# INLINE inspIntercalateSuffix #-}
inspIntercalateSuffix :: Int -> Int -> IO ()
inspIntercalateSuffix value n = intercalateSuffix value 1 (sourceUnfoldrM value n)

-- indexing
{-# INLINE inspIndexed #-}
inspIndexed :: Int -> Int -> IO ()
inspIndexed value n = indexed 1 (sourceUnfoldrM value n)

{-# INLINE inspIndexedR #-}
inspIndexedR :: Int -> Int -> IO ()
inspIndexedR value n = indexedR value 1 (sourceUnfoldrM value n)

-- The 'interspersePure' benchmark applies 'intersperse' to a pure
-- ('sourceUnfoldr') source rather than the monadic 'sourceUnfoldrM' source.
{-# INLINE inspInterspersePure #-}
inspInterspersePure :: Int -> Int -> IO ()
inspInterspersePure value n = intersperse value 1 (sourceUnfoldr value n)

-- The 'X4' benchmark groups compose each transformation four times. This
-- stresses fusion (some need a higher @-fspec-constr-recursive@) harder than
-- the single compose above, so they are checked separately.

-- mappingX4
{-# INLINE inspMapX4 #-}
inspMapX4 :: Int -> Int -> IO ()
inspMapX4 value n = mapN 4 (sourceUnfoldrM value n)

{-# INLINE inspMapMX4 #-}
inspMapMX4 :: Int -> Int -> IO ()
inspMapMX4 value n = mapM 4 (sourceUnfoldrM value n)

{-# INLINE inspTraceX4 #-}
inspTraceX4 :: Int -> Int -> IO ()
inspTraceX4 value n = trace 4 (sourceUnfoldrM value n)

{-# INLINE inspScanl'X4 #-}
inspScanl'X4 :: Int -> Int -> IO ()
inspScanl'X4 value n = scanl' 4 (sourceUnfoldrM value n)

{-# INLINE inspScanl1'X4 #-}
inspScanl1'X4 :: Int -> Int -> IO ()
inspScanl1'X4 value n = scanl1' 4 (sourceUnfoldrM value n)

{-# INLINE inspScanlM'X4 #-}
inspScanlM'X4 :: Int -> Int -> IO ()
inspScanlM'X4 value n = scanlM' 4 (sourceUnfoldrM value n)

{-# INLINE inspScanl1M'X4 #-}
inspScanl1M'X4 :: Int -> Int -> IO ()
inspScanl1M'X4 value n = scanl1M' 4 (sourceUnfoldrM value n)

{-# INLINE inspScanX4 #-}
inspScanX4 :: Int -> Int -> IO ()
inspScanX4 value n = scan 4 (sourceUnfoldrM value n)

{-# INLINE inspPostscanX4 #-}
inspPostscanX4 :: Int -> Int -> IO ()
inspPostscanX4 value n = postscan 4 (sourceUnfoldrM value n)

{-# INLINE inspPostscanl'X4 #-}
inspPostscanl'X4 :: Int -> Int -> IO ()
inspPostscanl'X4 value n = postscanl' 4 (sourceUnfoldrM value n)

{-# INLINE inspPostscanlM'X4 #-}
inspPostscanlM'X4 :: Int -> Int -> IO ()
inspPostscanlM'X4 value n = postscanlM' 4 (sourceUnfoldrM value n)

-- filteringX4
{-# INLINE inspFilterEvenX4 #-}
inspFilterEvenX4 :: Int -> Int -> IO ()
inspFilterEvenX4 value n = filterEven 4 (sourceUnfoldrM value n)

{-# INLINE inspFilterAllOutX4 #-}
inspFilterAllOutX4 :: Int -> Int -> IO ()
inspFilterAllOutX4 value n = filterAllOut value 4 (sourceUnfoldrM value n)

{-# INLINE inspFilterAllInX4 #-}
inspFilterAllInX4 :: Int -> Int -> IO ()
inspFilterAllInX4 value n = filterAllIn value 4 (sourceUnfoldrM value n)

{-# INLINE inspFilterMEvenX4 #-}
inspFilterMEvenX4 :: Int -> Int -> IO ()
inspFilterMEvenX4 value n = filterMEven 4 (sourceUnfoldrM value n)

{-# INLINE inspFilterMAllOutX4 #-}
inspFilterMAllOutX4 :: Int -> Int -> IO ()
inspFilterMAllOutX4 value n = filterMAllOut value 4 (sourceUnfoldrM value n)

{-# INLINE inspFilterMAllInX4 #-}
inspFilterMAllInX4 :: Int -> Int -> IO ()
inspFilterMAllInX4 value n = filterMAllIn value 4 (sourceUnfoldrM value n)

{-# INLINE inspTakeAllX4 #-}
inspTakeAllX4 :: Int -> Int -> IO ()
inspTakeAllX4 value n = takeAll value 4 (sourceUnfoldrM value n)

{-# INLINE inspTakeWhileTrueX4 #-}
inspTakeWhileTrueX4 :: Int -> Int -> IO ()
inspTakeWhileTrueX4 value n = takeWhileTrue value 4 (sourceUnfoldrM value n)

{-# INLINE inspTakeWhileMTrueX4 #-}
inspTakeWhileMTrueX4 :: Int -> Int -> IO ()
inspTakeWhileMTrueX4 value n = takeWhileMTrue value 4 (sourceUnfoldrM value n)

{-# INLINE inspDropOneX4 #-}
inspDropOneX4 :: Int -> Int -> IO ()
inspDropOneX4 value n = dropOne 4 (sourceUnfoldrM value n)

{-# INLINE inspDropAllX4 #-}
inspDropAllX4 :: Int -> Int -> IO ()
inspDropAllX4 value n = dropAll value 4 (sourceUnfoldrM value n)

{-# INLINE inspDropWhileTrueX4 #-}
inspDropWhileTrueX4 :: Int -> Int -> IO ()
inspDropWhileTrueX4 value n = dropWhileTrue value 4 (sourceUnfoldrM value n)

{-# INLINE inspDropWhileMTrueX4 #-}
inspDropWhileMTrueX4 :: Int -> Int -> IO ()
inspDropWhileMTrueX4 value n = dropWhileMTrue value 4 (sourceUnfoldrM value n)

{-# INLINE inspDropWhileFalseX4 #-}
inspDropWhileFalseX4 :: Int -> Int -> IO ()
inspDropWhileFalseX4 value n = dropWhileFalse value 4 (sourceUnfoldrM value n)

{-# INLINE inspDeleteByX4 #-}
inspDeleteByX4 :: Int -> Int -> IO ()
inspDeleteByX4 value n = deleteBy value 4 (sourceUnfoldrM value n)

{-# INLINE inspUniqX4 #-}
inspUniqX4 :: Int -> Int -> IO ()
inspUniqX4 value n = uniq 4 (sourceUnfoldrM value n)

{-# INLINE inspMapMaybeX4 #-}
inspMapMaybeX4 :: Int -> Int -> IO ()
inspMapMaybeX4 value n = mapMaybe 4 (sourceUnfoldrM value n)

{-# INLINE inspMapMaybeMX4 #-}
inspMapMaybeMX4 :: Int -> Int -> IO ()
inspMapMaybeMX4 value n = mapMaybeM 4 (sourceUnfoldrM value n)

{-# INLINE inspFindIndicesX4 #-}
inspFindIndicesX4 :: Int -> Int -> IO ()
inspFindIndicesX4 value n = findIndices value 4 (sourceUnfoldrM value n)

{-# INLINE inspElemIndicesX4 #-}
inspElemIndicesX4 :: Int -> Int -> IO ()
inspElemIndicesX4 value n = elemIndices value 4 (sourceUnfoldrM value n)

-- insertingX4
{-# INLINE inspIntersperseX4 #-}
inspIntersperseX4 :: Int -> Int -> IO ()
inspIntersperseX4 value n = intersperse value 4 (sourceUnfoldrM value n)

{-# INLINE inspInsertByX4 #-}
inspInsertByX4 :: Int -> Int -> IO ()
inspInsertByX4 value n = insertBy value 4 (sourceUnfoldrM value n)

-- indexingX4
{-# INLINE inspIndexedX4 #-}
inspIndexedX4 :: Int -> Int -> IO ()
inspIndexedX4 value n = indexed 4 (sourceUnfoldrM value n)

{-# INLINE inspIndexedRX4 #-}
inspIndexedRX4 :: Int -> Int -> IO ()
inspIndexedRX4 value n = indexedR value 4 (sourceUnfoldrM value n)

-- maps and scans
inspect $ hasNoTypeClasses 'inspMap
inspect $ 'inspMap `hasNoType` ''Stream.Step
inspect $ 'inspMap `hasNoType` ''FL.Step
inspect $ 'inspMap `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspMapM
inspect $ 'inspMapM `hasNoType` ''Stream.Step
inspect $ 'inspMapM `hasNoType` ''FL.Step
inspect $ 'inspMapM `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspSequence
inspect $ 'inspSequence `hasNoType` ''Stream.Step
inspect $ 'inspSequence `hasNoType` ''FL.Step
inspect $ 'inspSequence `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTap
inspect $ 'inspTap `hasNoType` ''Stream.Step
inspect $ 'inspTap `hasNoType` ''FL.Step
inspect $ 'inspTap `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTrace
inspect $ 'inspTrace `hasNoType` ''Stream.Step
inspect $ 'inspTrace `hasNoType` ''FL.Step
inspect $ 'inspTrace `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspScanl'
inspect $ 'inspScanl' `hasNoType` ''Stream.Step
inspect $ hasNoTypeClasses 'inspScanl1'
inspect $ 'inspScanl1' `hasNoType` ''Stream.Step
inspect $ hasNoTypeClasses 'inspScanlM'
inspect $ 'inspScanlM' `hasNoType` ''Stream.Step
inspect $ hasNoTypeClasses 'inspScanl1M'
inspect $ 'inspScanl1M' `hasNoType` ''Stream.Step
inspect $ hasNoTypeClasses 'inspScan
inspect $ 'inspScan `hasNoType` ''Stream.Step
inspect $ 'inspScan `hasNoType` ''FL.Step
inspect $ 'inspScan `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspPostscan
inspect $ 'inspPostscan `hasNoType` ''Stream.Step
inspect $ 'inspPostscan `hasNoType` ''FL.Step
inspect $ 'inspPostscan `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspPostscanl'
inspect $ 'inspPostscanl' `hasNoType` ''Stream.Step
inspect $ hasNoTypeClasses 'inspPostscanlM'
inspect $ 'inspPostscanlM' `hasNoType` ''Stream.Step

-- filtering
inspect $ hasNoTypeClasses 'inspFilterEven
inspect $ 'inspFilterEven `hasNoType` ''Stream.Step
inspect $ 'inspFilterEven `hasNoType` ''FL.Step
inspect $ 'inspFilterEven `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterAllOut
inspect $ 'inspFilterAllOut `hasNoType` ''Stream.Step
inspect $ 'inspFilterAllOut `hasNoType` ''FL.Step
inspect $ 'inspFilterAllOut `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterAllIn
inspect $ 'inspFilterAllIn `hasNoType` ''Stream.Step
inspect $ 'inspFilterAllIn `hasNoType` ''FL.Step
inspect $ 'inspFilterAllIn `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterMEven
inspect $ 'inspFilterMEven `hasNoType` ''Stream.Step
inspect $ 'inspFilterMEven `hasNoType` ''FL.Step
inspect $ 'inspFilterMEven `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterMAllOut
inspect $ 'inspFilterMAllOut `hasNoType` ''Stream.Step
inspect $ 'inspFilterMAllOut `hasNoType` ''FL.Step
inspect $ 'inspFilterMAllOut `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterMAllIn
inspect $ 'inspFilterMAllIn `hasNoType` ''Stream.Step
inspect $ 'inspFilterMAllIn `hasNoType` ''FL.Step
inspect $ 'inspFilterMAllIn `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTakeAll
inspect $ 'inspTakeAll `hasNoType` ''Stream.Step
inspect $ 'inspTakeAll `hasNoType` ''FL.Step
inspect $ 'inspTakeAll `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTakeWhileTrue
inspect $ 'inspTakeWhileTrue `hasNoType` ''Stream.Step
inspect $ 'inspTakeWhileTrue `hasNoType` ''FL.Step
inspect $ 'inspTakeWhileTrue `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTakeWhileMTrue
inspect $ 'inspTakeWhileMTrue `hasNoType` ''Stream.Step
inspect $ 'inspTakeWhileMTrue `hasNoType` ''FL.Step
inspect $ 'inspTakeWhileMTrue `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDropOne
inspect $ 'inspDropOne `hasNoType` ''Stream.Step
inspect $ 'inspDropOne `hasNoType` ''FL.Step
inspect $ 'inspDropOne `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDropAll
inspect $ 'inspDropAll `hasNoType` ''Stream.Step
inspect $ 'inspDropAll `hasNoType` ''FL.Step
inspect $ 'inspDropAll `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDropWhileTrue
inspect $ 'inspDropWhileTrue `hasNoType` ''Stream.Step
inspect $ 'inspDropWhileTrue `hasNoType` ''FL.Step
inspect $ 'inspDropWhileTrue `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDropWhileMTrue
inspect $ 'inspDropWhileMTrue `hasNoType` ''Stream.Step
inspect $ 'inspDropWhileMTrue `hasNoType` ''FL.Step
inspect $ 'inspDropWhileMTrue `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDropWhileFalse
inspect $ 'inspDropWhileFalse `hasNoType` ''Stream.Step
inspect $ 'inspDropWhileFalse `hasNoType` ''FL.Step
inspect $ 'inspDropWhileFalse `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDeleteBy
inspect $ 'inspDeleteBy `hasNoType` ''Stream.Step
inspect $ 'inspDeleteBy `hasNoType` ''FL.Step
inspect $ 'inspDeleteBy `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspUniq
inspect $ 'inspUniq `hasNoType` ''Stream.Step
inspect $ 'inspUniq `hasNoType` ''FL.Step
inspect $ 'inspUniq `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspMapMaybe
inspect $ 'inspMapMaybe `hasNoType` ''Stream.Step
inspect $ 'inspMapMaybe `hasNoType` ''FL.Step
inspect $ 'inspMapMaybe `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspMapMaybeM
inspect $ 'inspMapMaybeM `hasNoType` ''Stream.Step
inspect $ 'inspMapMaybeM `hasNoType` ''FL.Step
inspect $ 'inspMapMaybeM `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFindIndices
inspect $ 'inspFindIndices `hasNoType` ''Stream.Step
inspect $ 'inspFindIndices `hasNoType` ''FL.Step
inspect $ 'inspFindIndices `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspElemIndices
inspect $ 'inspElemIndices `hasNoType` ''Stream.Step
inspect $ 'inspElemIndices `hasNoType` ''FL.Step
inspect $ 'inspElemIndices `hasNoType` ''SPEC

-- inserting
inspect $ hasNoTypeClasses 'inspIntersperse
inspect $ 'inspIntersperse `hasNoType` ''Stream.Step
inspect $ 'inspIntersperse `hasNoType` ''FL.Step
inspect $ 'inspIntersperse `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspIntersperseM
inspect $ 'inspIntersperseM `hasNoType` ''Stream.Step
inspect $ 'inspIntersperseM `hasNoType` ''FL.Step
inspect $ 'inspIntersperseM `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspInsertBy
inspect $ 'inspInsertBy `hasNoType` ''Stream.Step
inspect $ 'inspInsertBy `hasNoType` ''FL.Step
inspect $ 'inspInsertBy `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspInterposeSuffix
inspect $ 'inspInterposeSuffix `hasNoType` ''Stream.Step
inspect $ 'inspInterposeSuffix `hasNoType` ''FL.Step
inspect $ 'inspInterposeSuffix `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspIntercalateSuffix
inspect $ 'inspIntercalateSuffix `hasNoType` ''Stream.Step
inspect $ 'inspIntercalateSuffix `hasNoType` ''FL.Step
inspect $ 'inspIntercalateSuffix `hasNoType` ''SPEC

-- indexing
inspect $ hasNoTypeClasses 'inspIndexed
inspect $ 'inspIndexed `hasNoType` ''Stream.Step
inspect $ 'inspIndexed `hasNoType` ''FL.Step
inspect $ 'inspIndexed `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspIndexedR
inspect $ 'inspIndexedR `hasNoType` ''Stream.Step
inspect $ 'inspIndexedR `hasNoType` ''FL.Step
inspect $ 'inspIndexedR `hasNoType` ''SPEC

-- pure-source intersperse
inspect $ hasNoTypeClasses 'inspInterspersePure
inspect $ 'inspInterspersePure `hasNoType` ''Stream.Step
inspect $ 'inspInterspersePure `hasNoType` ''FL.Step
inspect $ 'inspInterspersePure `hasNoType` ''SPEC

-- mappingX4
inspect $ hasNoTypeClasses 'inspMapX4
inspect $ 'inspMapX4 `hasNoType` ''Stream.Step
inspect $ 'inspMapX4 `hasNoType` ''FL.Step
inspect $ 'inspMapX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspMapMX4
inspect $ 'inspMapMX4 `hasNoType` ''Stream.Step
inspect $ 'inspMapMX4 `hasNoType` ''FL.Step
inspect $ 'inspMapMX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTraceX4
inspect $ 'inspTraceX4 `hasNoType` ''Stream.Step
inspect $ 'inspTraceX4 `hasNoType` ''FL.Step
inspect $ 'inspTraceX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspScanl'X4
inspect $ 'inspScanl'X4 `hasNoType` ''Stream.Step
inspect $ hasNoTypeClasses 'inspScanl1'X4
inspect $ 'inspScanl1'X4 `hasNoType` ''Stream.Step
inspect $ hasNoTypeClasses 'inspScanlM'X4
inspect $ 'inspScanlM'X4 `hasNoType` ''Stream.Step
inspect $ hasNoTypeClasses 'inspScanl1M'X4
inspect $ 'inspScanl1M'X4 `hasNoType` ''Stream.Step
inspect $ hasNoTypeClasses 'inspScanX4
inspect $ 'inspScanX4 `hasNoType` ''Stream.Step
inspect $ 'inspScanX4 `hasNoType` ''FL.Step
inspect $ 'inspScanX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspPostscanX4
inspect $ 'inspPostscanX4 `hasNoType` ''Stream.Step
inspect $ 'inspPostscanX4 `hasNoType` ''FL.Step
inspect $ 'inspPostscanX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspPostscanl'X4
-- 'postscanl'' composed 4x does not fully fuse at the build's
-- @-fspec-constr-recursive@, so a 'Step' constructor survives (the single
-- compose 'inspPostscanl'' above is 'Step'-free).
-- inspect $ 'inspPostscanl'X4 `hasNoType` ''Stream.Step
inspect $ hasNoTypeClasses 'inspPostscanlM'X4
inspect $ 'inspPostscanlM'X4 `hasNoType` ''Stream.Step

-- filteringX4
inspect $ hasNoTypeClasses 'inspFilterEvenX4
inspect $ 'inspFilterEvenX4 `hasNoType` ''Stream.Step
inspect $ 'inspFilterEvenX4 `hasNoType` ''FL.Step
inspect $ 'inspFilterEvenX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterAllOutX4
inspect $ 'inspFilterAllOutX4 `hasNoType` ''Stream.Step
inspect $ 'inspFilterAllOutX4 `hasNoType` ''FL.Step
inspect $ 'inspFilterAllOutX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterAllInX4
inspect $ 'inspFilterAllInX4 `hasNoType` ''Stream.Step
inspect $ 'inspFilterAllInX4 `hasNoType` ''FL.Step
inspect $ 'inspFilterAllInX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterMEvenX4
inspect $ 'inspFilterMEvenX4 `hasNoType` ''Stream.Step
inspect $ 'inspFilterMEvenX4 `hasNoType` ''FL.Step
inspect $ 'inspFilterMEvenX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterMAllOutX4
inspect $ 'inspFilterMAllOutX4 `hasNoType` ''Stream.Step
inspect $ 'inspFilterMAllOutX4 `hasNoType` ''FL.Step
inspect $ 'inspFilterMAllOutX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFilterMAllInX4
inspect $ 'inspFilterMAllInX4 `hasNoType` ''Stream.Step
inspect $ 'inspFilterMAllInX4 `hasNoType` ''FL.Step
inspect $ 'inspFilterMAllInX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTakeAllX4
inspect $ 'inspTakeAllX4 `hasNoType` ''Stream.Step
inspect $ 'inspTakeAllX4 `hasNoType` ''FL.Step
inspect $ 'inspTakeAllX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTakeWhileTrueX4
inspect $ 'inspTakeWhileTrueX4 `hasNoType` ''Stream.Step
inspect $ 'inspTakeWhileTrueX4 `hasNoType` ''FL.Step
inspect $ 'inspTakeWhileTrueX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspTakeWhileMTrueX4
inspect $ 'inspTakeWhileMTrueX4 `hasNoType` ''Stream.Step
inspect $ 'inspTakeWhileMTrueX4 `hasNoType` ''FL.Step
inspect $ 'inspTakeWhileMTrueX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDropOneX4
inspect $ 'inspDropOneX4 `hasNoType` ''Stream.Step
inspect $ 'inspDropOneX4 `hasNoType` ''FL.Step
inspect $ 'inspDropOneX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDropAllX4
inspect $ 'inspDropAllX4 `hasNoType` ''Stream.Step
inspect $ 'inspDropAllX4 `hasNoType` ''FL.Step
inspect $ 'inspDropAllX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDropWhileTrueX4
inspect $ 'inspDropWhileTrueX4 `hasNoType` ''Stream.Step
inspect $ 'inspDropWhileTrueX4 `hasNoType` ''FL.Step
inspect $ 'inspDropWhileTrueX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDropWhileMTrueX4
inspect $ 'inspDropWhileMTrueX4 `hasNoType` ''Stream.Step
inspect $ 'inspDropWhileMTrueX4 `hasNoType` ''FL.Step
inspect $ 'inspDropWhileMTrueX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDropWhileFalseX4
inspect $ 'inspDropWhileFalseX4 `hasNoType` ''Stream.Step
inspect $ 'inspDropWhileFalseX4 `hasNoType` ''FL.Step
inspect $ 'inspDropWhileFalseX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspDeleteByX4
inspect $ 'inspDeleteByX4 `hasNoType` ''Stream.Step
inspect $ 'inspDeleteByX4 `hasNoType` ''FL.Step
inspect $ 'inspDeleteByX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspUniqX4
inspect $ 'inspUniqX4 `hasNoType` ''Stream.Step
inspect $ 'inspUniqX4 `hasNoType` ''FL.Step
inspect $ 'inspUniqX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspMapMaybeX4
inspect $ 'inspMapMaybeX4 `hasNoType` ''Stream.Step
inspect $ 'inspMapMaybeX4 `hasNoType` ''FL.Step
inspect $ 'inspMapMaybeX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspMapMaybeMX4
inspect $ 'inspMapMaybeMX4 `hasNoType` ''Stream.Step
inspect $ 'inspMapMaybeMX4 `hasNoType` ''FL.Step
inspect $ 'inspMapMaybeMX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspFindIndicesX4
inspect $ 'inspFindIndicesX4 `hasNoType` ''Stream.Step
inspect $ 'inspFindIndicesX4 `hasNoType` ''FL.Step
inspect $ 'inspFindIndicesX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspElemIndicesX4
inspect $ 'inspElemIndicesX4 `hasNoType` ''Stream.Step
inspect $ 'inspElemIndicesX4 `hasNoType` ''FL.Step
inspect $ 'inspElemIndicesX4 `hasNoType` ''SPEC

-- insertingX4
inspect $ hasNoTypeClasses 'inspIntersperseX4
inspect $ 'inspIntersperseX4 `hasNoType` ''Stream.Step
inspect $ 'inspIntersperseX4 `hasNoType` ''FL.Step
-- inspect $ 'inspIntersperseX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspInsertByX4
inspect $ 'inspInsertByX4 `hasNoType` ''Stream.Step
inspect $ 'inspInsertByX4 `hasNoType` ''FL.Step
inspect $ 'inspInsertByX4 `hasNoType` ''SPEC

-- indexingX4
inspect $ hasNoTypeClasses 'inspIndexedX4
inspect $ 'inspIndexedX4 `hasNoType` ''Stream.Step
inspect $ 'inspIndexedX4 `hasNoType` ''FL.Step
inspect $ 'inspIndexedX4 `hasNoType` ''SPEC
inspect $ hasNoTypeClasses 'inspIndexedRX4
inspect $ 'inspIndexedRX4 `hasNoType` ''Stream.Step
inspect $ 'inspIndexedRX4 `hasNoType` ''FL.Step
inspect $ 'inspIndexedRX4 `hasNoType` ''SPEC

-- Not inspection-tested (not 'Step'-free fusion targets):
--   * 'o_n_space' "naive prime sieve" ('sieveScan') reduces with list
--     (++)/takeWhile/all, so it cannot eliminate 'Step'.
--   * 'o_n_space' "iterated" benchmarks ('iterateSingleton', and the "(+)"
--     baseline) build a stream by explicit recursion rather than a
--     generate+transform+drain pipeline, so there is nothing to fuse.
#endif

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
benchmarks :: String -> Int -> [Benchmark]
benchmarks moduleName size =
        [ bgroup (o_1_space_prefix moduleName) $ Prelude.concat
            [ o_1_space_functor size
            , o_1_space_mapping size
            , o_1_space_mappingX4 size
            , o_1_space_filtering size
            , o_1_space_filteringX4 size
            , o_1_space_inserting size
            , o_1_space_insertingX4 size
            , o_1_space_indexing size
            , o_1_space_indexingX4 size
            ]
        , bgroup (o_n_space_prefix moduleName) $ Prelude.concat
            [
              o_n_space_mapping size
            , o_n_space_iterated size
            ]
        ]
