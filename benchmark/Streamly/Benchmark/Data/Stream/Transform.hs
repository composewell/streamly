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
#ifdef USE_PRELUDE
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Stream.Transform (benchmarks) where

import Control.Monad.IO.Class (MonadIO(..))

import System.Random (randomRIO)

import qualified Streamly.Internal.Data.Fold as FL

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Unfold as Unfold

#ifdef USE_PRELUDE
import Control.DeepSeq (NFData(..))
import Data.Functor.Identity (Identity(..))
import qualified Prelude
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Internal.Data.Time.Units
#else
import Streamly.Internal.Data.Stream (Stream)
import qualified Streamly.Internal.Data.Stream as Stream
#ifndef USE_STREAMLY_CORE
import qualified Streamly.Internal.Data.Stream.Prelude as Stream
#endif
#ifdef USE_STREAMK
import Control.DeepSeq (NFData(..))
import Data.Functor.Identity (Identity(..))
import qualified Prelude
import qualified Streamly.Internal.Data.Fold as Fold
import Streamly.Internal.Data.StreamK (StreamK)
import qualified Streamly.Internal.Data.StreamK as StreamK
#endif
#endif

import Test.Tasty.Bench
import Stream.Common hiding (scanl')
import Streamly.Benchmark.Common
import Prelude hiding (sequence, mapM)

#ifdef USE_PRELUDE
type Stream = Stream.SerialT
#endif

-------------------------------------------------------------------------------
-- Pipelines (stream-to-stream transformations)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- one-to-one transformations
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Traversable Instance
-------------------------------------------------------------------------------

#ifdef USE_STREAMK
{-# INLINE traversableTraverse #-}
traversableTraverse :: StreamK Identity Int -> IO (StreamK Identity Int)
traversableTraverse = traverse return

{-# INLINE traversableSequenceA #-}
traversableSequenceA :: StreamK Identity Int -> IO (StreamK Identity Int)
traversableSequenceA = sequenceA . Prelude.fmap return

{-# INLINE traversableMapM #-}
traversableMapM :: StreamK Identity Int -> IO (StreamK Identity Int)
traversableMapM = Prelude.mapM return

{-# INLINE traversableSequence #-}
traversableSequence :: StreamK Identity Int -> IO (StreamK Identity Int)
traversableSequence = Prelude.sequence . Prelude.fmap return

{-# INLINE benchPureSinkIO #-}
benchPureSinkIO
    :: NFData b
    => Int -> String -> (StreamK Identity Int -> IO b) -> Benchmark
benchPureSinkIO value name f =
    bench name
        $ nfIO $ randomRIO (1, 1) >>= f . fromStream . sourceUnfoldr value

instance NFData a => NFData (StreamK Identity a) where
    {-# INLINE rnf #-}
    rnf xs =
        runIdentity
            $ Stream.fold (Fold.foldl' (\_ x -> rnf x) ()) (toStream xs)

o_n_space_traversable :: Int -> [Benchmark]
o_n_space_traversable value =
    -- Buffering operations using heap proportional to number of elements.
    [ bgroup "traversable"
        -- Traversable instance
        [ benchPureSinkIO value "traverse" traversableTraverse
        , benchPureSinkIO value "sequenceA" traversableSequenceA
        , benchPureSinkIO value "mapM" traversableMapM
        , benchPureSinkIO value "sequence" traversableSequence
        ]
    ]
#endif

-------------------------------------------------------------------------------
-- maps and scans
-------------------------------------------------------------------------------

#ifdef USE_PRELUDE
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
#endif

{-# INLINE scan #-}
scan :: MonadIO m => Int -> Stream m Int -> m ()
scan n = composeN n $ Stream.scan FL.sum

#ifdef USE_PRELUDE
{-# INLINE postscanl' #-}
postscanl' :: MonadIO m => Int -> Stream m Int -> m ()
postscanl' n = composeN n $ Stream.postscanl' (+) 0

{-# INLINE postscanlM' #-}
postscanlM' :: MonadIO m => Int -> Stream m Int -> m ()
postscanlM' n = composeN n $ Stream.postscanlM' (\b a -> return $ b + a) (return 0)
#endif

{-# INLINE postscan #-}
postscan :: MonadIO m => Int -> Stream m Int -> m ()
postscan n = composeN n $ Stream.postscan FL.sum

{-# INLINE sequence #-}
sequence :: MonadAsync m => Stream m (m Int) -> m ()
sequence = Common.drain . Stream.sequence

{-# INLINE tap #-}
tap :: MonadIO m => Int -> Stream m Int -> m ()
tap n = composeN n $ Stream.tap FL.sum

#ifdef USE_PRELUDE
{-# INLINE pollCounts #-}
pollCounts :: Int -> Stream IO Int -> IO ()
pollCounts n =
    composeN n (Stream.pollCounts (const True) f)

    where

    f = Stream.drain . Stream.rollingMap2 (-) . Stream.delayPost 1

{-# INLINE timestamped #-}
timestamped :: (MonadAsync m) => Stream m Int -> m ()
timestamped = Stream.drain . Stream.timestamped
#endif

#ifdef USE_STREAMK
{-# INLINE foldrS #-}
foldrS :: MonadIO m => Int -> Stream m Int -> m ()
foldrS n =
    composeN n (toStream . StreamK.foldrS StreamK.cons StreamK.nil . fromStream)

{-# INLINE foldrSMap #-}
foldrSMap :: MonadIO m => Int -> Stream m Int -> m ()
foldrSMap n =
    composeN n
        ( toStream
        . StreamK.foldrS (\x xs -> x + 1 `StreamK.cons` xs) StreamK.nil
        . fromStream
        )
#endif

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
#ifdef USE_STREAMK
        -- Right folds
          benchIOSink value "foldrS" (foldrS 1)
        , benchIOSink value "foldrSMap" (foldrSMap 1)
        ,
#endif
        -- , benchIOSink value "foldrT" (foldrT 1)
        -- , benchIOSink value "foldrTMap" (foldrTMap 1)

        -- Mapping
          benchIOSink value "map" (mapN 1)
        , bench "sequence" $ nfIO $ randomRIO (1, 1000) >>= \n ->
              sequence (sourceUnfoldrAction value n)
        , benchIOSink value "mapM" (mapM 1)
        , benchIOSink value "tap" (tap 1)
#ifdef USE_PRELUDE
        , benchIOSink value "pollCounts 1 second" (pollCounts 1)
        , benchIOSink value "timestamped" timestamped

        -- Scanning
        , benchIOSink value "scanl'" (scanl' 1)
        , benchIOSink value "scanl1'" (scanl1' 1)
        , benchIOSink value "scanlM'" (scanlM' 1)
        , benchIOSink value "scanl1M'" (scanl1M' 1)
        , benchIOSink value "postscanl'" (postscanl' 1)
        , benchIOSink value "postscanlM'" (postscanlM' 1)
#endif
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

#ifdef USE_PRELUDE
        , benchIOSink value "scanl'" (scanl' 4)
        , benchIOSink value "scanl1'" (scanl1' 4)
        , benchIOSink value "scanlM'" (scanlM' 4)
        , benchIOSink value "scanl1M'" (scanl1M' 4)
        , benchIOSink value "postscanl'" (postscanl' 4)
        , benchIOSink value "postscanlM'" (postscanlM' 4)
#endif
        ]
    ]

{-# INLINE sieveScan #-}
sieveScan :: Monad m => Stream m Int -> Stream m Int
sieveScan =
      Stream.mapMaybe snd
    . Stream.scan (FL.foldlM' (\(primes, _) n -> do
            return $
                let ps = takeWhile (\p -> p * p <= n) primes
                 in if all (\p -> n `mod` p /= 0) ps
                    then (primes ++ [n], Just n)
                    else (primes, Nothing)) (return ([2], Just 2)))

o_n_space_mapping :: Int -> [Benchmark]
o_n_space_mapping value =
    [ bgroup "mapping"
        [ benchIO "naive prime sieve"
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

#ifdef USE_STREAMK
-- Iterate a transformation over a singleton stream
{-# INLINE iterateSingleton #-}
iterateSingleton :: Applicative m =>
       (Int -> StreamK m Int -> StreamK m Int)
    -> Int
    -> Int
    -> Stream m Int
iterateSingleton g count n = toStream $ iterateN g (StreamK.fromPure n) count
#else
-- Iterate a transformation over a singleton stream
{-# INLINE iterateSingleton #-}
iterateSingleton :: Applicative m =>
       (Int -> Stream m Int -> Stream m Int)
    -> Int
    -> Int
    -> Stream m Int
iterateSingleton g count n = iterateN g (Stream.fromPure n) count
#endif

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

#if !defined(USE_STREAMLY_CORE) && !defined(USE_PRELUDE)
{-# INLINE takeInterval #-}
takeInterval :: Double -> Int -> Stream IO Int -> IO ()
takeInterval i n = composeN n (Stream.takeInterval i)

-- Inspection testing is disabled for takeInterval
-- Enable it when looking at it throughly
#ifdef INSPECTION
-- inspect $ hasNoType 'takeInterval ''SPEC
-- inspect $ hasNoTypeClasses 'takeInterval
-- inspect $ 'takeInterval `hasNoType` ''D.Step
#endif

{-# INLINE dropInterval #-}
dropInterval :: Double -> Int -> Stream IO Int -> IO ()
dropInterval i n = composeN n (Stream.dropInterval i)

-- Inspection testing is disabled for dropInterval
-- Enable it when looking at it throughly
#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'dropInterval
-- inspect $ 'dropInterval `hasNoType` ''D.Step
#endif
#endif

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

#ifdef USE_PRELUDE
-- XXX Decide on the time interval
{-# INLINE _intervalsOfSum #-}
_intervalsOfSum :: MonadAsync m => Double -> Int -> Stream m Int -> m ()
_intervalsOfSum i n = composeN n (Stream.intervalsOf i FL.sum)
#endif

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
#if !defined(USE_STREAMLY_CORE) && !defined(USE_PRELUDE)
        , benchIOSink value "takeInterval-all" (takeInterval 10000 1)
        , benchIOSink value "dropInterval-all" (dropInterval 10000 1)
#endif
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
    composeN n $ Stream.interposeSuffix (value + 1) Unfold.identity

{-# INLINE intercalateSuffix #-}
intercalateSuffix :: Monad m => Int -> Int -> Stream m Int -> m ()
intercalateSuffix value n =
    composeN n $ Stream.intercalateSuffix Unfold.identity (value + 1)

o_1_space_inserting :: Int -> [Benchmark]
o_1_space_inserting value =
    [ bgroup "inserting"
        [ benchIOSink value "intersperse" (intersperse value 1)
        , benchIOSink value "intersperseM" (intersperseM value 1)
        , benchIOSink value "insertBy" (insertBy value 1)
        , benchIOSink value "interposeSuffix" (interposeSuffix value 1)
        , benchIOSink value "intercalateSuffix" (intercalateSuffix value 1)
        ]
    ]

o_1_space_insertingX4 :: Int -> [Benchmark]
o_1_space_insertingX4 value =
    [ bgroup "insertingX4"
        [ benchIOSink value "intersperse" (intersperse value 4)
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
#ifdef USE_STREAMK
              o_n_space_traversable size
            ,
#endif
              o_n_space_mapping size
            , o_n_space_iterated size
            ]
        ]
