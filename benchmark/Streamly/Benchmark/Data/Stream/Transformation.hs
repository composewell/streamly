-- |
-- Module      : Stream.Transformation
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Stream.Transformation (benchmarks) where

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity)
import System.Random (randomRIO)
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Prelude

import Gauge
import Streamly.Internal.Data.Stream (Stream)
import Stream.Common
import Streamly.Benchmark.Common
    (o_1_space_prefix
    , o_n_space_prefix
    , o_n_heap_prefix
    , o_n_stack_prefix)
import Prelude hiding (sequence, mapM, fmap)

-------------------------------------------------------------------------------
-- Traversable Instance
-------------------------------------------------------------------------------

{-# INLINE traversableTraverse #-}
traversableTraverse :: Stream Identity Int -> IO (Stream Identity Int)
traversableTraverse = traverse return

{-# INLINE traversableSequenceA #-}
traversableSequenceA :: Stream Identity Int -> IO (Stream Identity Int)
traversableSequenceA = traverse return

{-# INLINE traversableMapM #-}
traversableMapM :: Stream Identity Int -> IO (Stream Identity Int)
traversableMapM = Prelude.mapM return

{-# INLINE traversableSequence #-}
traversableSequence :: Stream Identity Int -> IO (Stream Identity Int)
traversableSequence = Prelude.mapM return

{-# INLINE benchPureSinkIO #-}
benchPureSinkIO
    :: NFData b
    => Int -> String -> (Stream Identity Int -> IO b) -> Benchmark
benchPureSinkIO value name f =
    bench name $ nfIO $ randomRIO (1, 1) >>= f . sourceUnfoldr value

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

-------------------------------------------------------------------------------
-- maps and scans
-------------------------------------------------------------------------------
{-# INLINE scan #-}
scan :: MonadIO m => Stream m Int -> m ()
scan = Stream.fold FL.drain . Stream.scan FL.sum

{-# INLINE postscan #-}
postscan :: MonadIO m => Stream m Int -> m ()
postscan = Stream.fold FL.drain . Stream.postscan FL.sum

{-# INLINE sequence #-}
sequence ::
       (Monad m)
    => Stream m (m Int)
    -> m ()
sequence = Stream.fold FL.drain . Stream.sequence

{-# INLINE tap #-}
tap :: MonadIO m => Stream m Int -> m ()
tap = Stream.fold FL.drain . Stream.tap FL.sum

{-# INLINE foldrS #-}
foldrS :: MonadIO m => Stream m Int -> m ()
foldrS = Stream.fold FL.drain . Stream.foldrS Stream.cons Stream.nil

{-# INLINE foldrSMap #-}
foldrSMap :: MonadIO m => Stream m Int -> m ()
foldrSMap = Stream.fold FL.drain . Stream.foldrS (\x xs -> x + 1 `Stream.cons` xs) Stream.nil

{-# INLINE foldrT #-}
foldrT :: MonadIO m => Stream m Int -> m ()
foldrT = Stream.fold FL.drain . Stream.foldrT Stream.cons Stream.nil

{-# INLINE foldrTMap #-}
foldrTMap :: MonadIO m => Stream m Int -> m ()
foldrTMap = Stream.fold FL.drain . Stream.foldrT (\x xs -> x + 1 `Stream.cons` xs) Stream.nil


{-# INLINE trace #-}
trace :: Monad m =>  Stream m Int -> m ()
trace = Stream.fold FL.drain . Stream.trace return

o_1_space_mapping :: Int -> [Benchmark]
o_1_space_mapping value =
    [ bgroup
        "mapping"
        [
        -- Right folds
          benchIOSink value "foldrS" foldrS
        , benchIOSink value "foldrSMap" foldrSMap
        , benchIOSink value "foldrT" foldrT
        , benchIOSink value "foldrTMap" foldrTMap

        -- Mapping
        , benchIOSink value "tap" tap
        , benchIOSink value "trace" trace
        , bench "sequence" $ nfIO $ randomRIO (1, 1000) >>= \n ->
            sequence (sourceUnfoldrAction value n)
        , benchIOSink value "scan" scan
        , benchIOSink value "postscan" postscan
        ]
    ]


-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- Size reducing transformations (filtering)
-------------------------------------------------------------------------------

{-# INLINE filterEven #-}
filterEven :: MonadIO m => Stream m Int -> m ()
filterEven = Stream.fold FL.drain . Stream.filter even

{-# INLINE filterAllOut #-}
filterAllOut :: MonadIO m => Int -> Stream m Int -> m ()
filterAllOut value = Stream.fold FL.drain . Stream.filter (> (value + 1))

{-# INLINE filterAllIn #-}
filterAllIn :: MonadIO m => Int -> Stream m Int -> m ()
filterAllIn value = Stream.fold FL.drain . Stream.filter (<= (value + 1))

{-# INLINE filterMEven #-}
filterMEven :: MonadIO m => Stream m Int -> m ()
filterMEven = Stream.fold FL.drain . Stream.filterM (return . even)

{-# INLINE filterMAllOut #-}
filterMAllOut :: MonadIO m => Int -> Stream m Int -> m ()
filterMAllOut value = Stream.fold FL.drain . Stream.filterM (\x -> return $ x > (value + 1))

{-# INLINE filterMAllIn #-}
filterMAllIn :: MonadIO m => Int -> Stream m Int -> m ()
filterMAllIn value = Stream.fold FL.drain . Stream.filterM (\x -> return $ x <= (value + 1))

{-# INLINE foldFilterEven #-}
foldFilterEven :: MonadIO m => Stream m Int -> m ()
foldFilterEven = Stream.fold FL.drain . Stream.foldFilter (FL.satisfy even)

{-# INLINE _takeOne #-}
_takeOne :: MonadIO m => Stream m Int -> m ()
_takeOne = Stream.fold FL.drain . Stream.take 1

{-# INLINE takeAll #-}
takeAll :: MonadIO m => Int -> Stream m Int -> m ()
takeAll value = Stream.fold FL.drain . Stream.take (value + 1)

{-# INLINE takeWhileTrue #-}
takeWhileTrue :: MonadIO m => Int -> Stream m Int -> m ()
takeWhileTrue value = Stream.fold FL.drain . Stream.takeWhile (<= (value + 1))

{-# INLINE takeWhileMTrue #-}
takeWhileMTrue :: MonadIO m => Int -> Stream m Int -> m ()
takeWhileMTrue value = Stream.fold FL.drain . Stream.takeWhileM (return . (<= (value + 1)))


{-# INLINE dropOne #-}
dropOne :: MonadIO m => Stream m Int -> m ()
dropOne = Stream.fold FL.drain . Stream.drop 1

{-# INLINE dropAll #-}
dropAll :: MonadIO m => Int -> Stream m Int -> m ()
dropAll value = Stream.fold FL.drain . Stream.drop (value + 1)

{-# INLINE dropWhileTrue #-}
dropWhileTrue :: MonadIO m => Int -> Stream m Int -> m ()
dropWhileTrue value = Stream.fold FL.drain . Stream.dropWhile (<= (value + 1))

{-# INLINE dropWhileMTrue #-}
dropWhileMTrue :: MonadIO m => Int -> Stream m Int -> m ()
dropWhileMTrue value = Stream.fold FL.drain . Stream.dropWhileM (return . (<= (value + 1)))

{-# INLINE dropWhileFalse #-}
dropWhileFalse :: MonadIO m => Int -> Stream m Int -> m ()
dropWhileFalse value = Stream.fold FL.drain . Stream.dropWhile (> (value + 1))


{-# INLINE findIndices #-}
findIndices :: MonadIO m => Int -> Stream m Int -> m ()
findIndices value = Stream.fold FL.drain . Stream.findIndices (== (value + 1))

{-# INLINE elemIndices #-}
elemIndices :: MonadIO m => Int -> Stream m Int -> m ()
elemIndices value = Stream.fold FL.drain . Stream.elemIndices (value + 1)

{-# INLINE deleteBy #-}
deleteBy :: MonadIO m => Int -> Stream m Int -> m ()
deleteBy value = Stream.fold FL.drain . Stream.deleteBy (>=) (value + 1)

-- uniq . uniq == uniq, composeN 2 ~ composeN 1
{-# INLINE uniq #-}
uniq :: MonadIO m => Stream m Int -> m ()
uniq = Stream.fold FL.drain . Stream.uniq

{-# INLINE mapMaybe #-}
mapMaybe :: MonadIO m => Stream m Int -> m ()
mapMaybe =
    Stream.fold FL.drain .
    Stream.mapMaybe
        (\x ->
             if odd x
             then Nothing
             else Just x)

{-# INLINE mapMaybeM #-}
mapMaybeM :: Monad m => Stream m Int -> m ()
mapMaybeM =
    Stream.fold FL.drain .
    Stream.mapMaybeM
        (\x ->
             if odd x
             then return Nothing
             else return $ Just x)

o_1_space_filtering :: Int -> [Benchmark]
o_1_space_filtering value =
    [ bgroup "filtering"
        [ benchIOSink value "filter-even" filterEven
        , benchIOSink value "filter-all-out" (filterAllOut value)
        , benchIOSink value "filter-all-in" (filterAllIn value)

        , benchIOSink value "filterM-even" filterMEven
        , benchIOSink value "filterM-all-out" (filterMAllOut value)
        , benchIOSink value "filterM-all-in" (filterMAllIn value)

        , benchIOSink value "foldFilter-even" foldFilterEven

        -- Trimming
        , benchIOSink value "take-all" (takeAll value)

        , benchIOSink value "takeWhile-true" (takeWhileTrue value)
     -- , benchIOSink value "takeWhileM-true" (_takeWhileMTrue value 1)
        , benchIOSink value "drop-one" dropOne
        , benchIOSink value "drop-all" (dropAll value)
     -- , benchIOSink value "dropWhileM-true" (_dropWhileMTrue value 1)
        , benchIOSink
              value
              "dropWhile-false"
              (dropWhileFalse value)
        , benchIOSink value "deleteBy" (deleteBy value)

        , benchIOSink value "uniq" uniq

        -- Map and filter
        , benchIOSink value "mapMaybe" mapMaybe
        , benchIOSink value "mapMaybeM" mapMaybeM

        -- Searching (stateful map and filter)
        , benchIOSink value "findIndices" (findIndices value)
        , benchIOSink value "elemIndices" (elemIndices value)
        ]
    ]

o_1_space_filteringX4 :: Int -> [Benchmark]
o_1_space_filteringX4 value =
    [ bgroup "filteringX4"
        [ benchIOSink value "filter-even" filterEven
        , benchIOSink value "filter-all-out" (filterAllOut value)
        , benchIOSink value "filter-all-in" (filterAllIn value)

        , benchIOSink value "filterM-even" filterMEven
        , benchIOSink value "filterM-all-out" (filterMAllOut value)
        , benchIOSink value "filterM-all-in" (filterMAllIn value)

        , benchIOSink value "foldFilter-even" foldFilterEven

        -- trimming
        , benchIOSink value "take-all" (takeAll value)
        , benchIOSink value "takeWhile-true" (takeWhileTrue value)
        , benchIOSink value "takeWhileM-true" (takeWhileMTrue value)
        , benchIOSink value "drop-one" dropOne
        , benchIOSink value "drop-all" (dropAll value)
        , benchIOSink value "dropWhile-true" (dropWhileTrue value)
        , benchIOSink value "dropWhileM-true" (dropWhileMTrue value)
        , benchIOSink
              value
              "dropWhile-false"
              (dropWhileFalse value)
        , benchIOSink value "deleteBy" (deleteBy value)

        , benchIOSink value "uniq" uniq

        -- map and filter
        , benchIOSink value "mapMaybe" mapMaybe
        , benchIOSink value "mapMaybeM" mapMaybeM

        -- searching
        , benchIOSink value "findIndices" (findIndices value)
        , benchIOSink value "elemIndices" (elemIndices value)
        ]
    ]

-------------------------------------------------------------------------------
-- Size increasing transformations (insertions)
-------------------------------------------------------------------------------

{-# INLINE intersperse #-}
intersperse :: Monad m => Int -> Stream m Int -> m ()
intersperse value = Stream.fold FL.drain . Stream.intersperse (value + 1)

{-# INLINE intersperseM #-}
intersperseM :: Monad m => Int -> Stream m Int -> m ()
intersperseM value =
    Stream.fold FL.drain . Stream.intersperseM (return $ value + 1)

{-# INLINE insertBy #-}
insertBy :: MonadIO m => Int -> Stream m Int -> m ()
insertBy value = Stream.fold FL.drain . Stream.insertBy compare (value + 1)

{-# INLINE interposeSuffix #-}
interposeSuffix :: Monad m => Int -> Stream m Int -> m ()
interposeSuffix value  =
    Stream.fold FL.drain . Stream.interposeSuffix (value + 1) Unfold.identity

{-# INLINE intercalateSuffix #-}
intercalateSuffix :: Monad m =>  Int -> Stream m Int -> m ()
intercalateSuffix value =
    Stream.fold FL.drain . Stream.intercalateSuffix Unfold.identity (value + 1)

o_1_space_inserting :: Int -> [Benchmark]
o_1_space_inserting value =
    [ bgroup "inserting"
        [ benchIOSink value "intersperse" (intersperse value)
        , benchIOSink value "intersperseM" (intersperseM value)
        , benchIOSink value "insertBy" (insertBy value)
        , benchIOSink value "interposeSuffix" (interposeSuffix value)
        , benchIOSink value "intercalateSuffix" (intercalateSuffix value)
        ]
    ]

-------------------------------------------------------------------------------
-- Indexing
-------------------------------------------------------------------------------

{-# INLINE indexed #-}
indexed :: MonadIO m => Stream m Int -> m ()
indexed  = Stream.fold FL.drain . Stream.map snd . Stream.indexed

{-# INLINE indexedR #-}
indexedR :: MonadIO m => Int -> Stream m Int -> m ()
indexedR value  = Stream.fold FL.drain . (Stream.map snd . Stream.indexedR value)

o_1_space_indexing :: Int -> [Benchmark]
o_1_space_indexing value =
    [ bgroup "indexing"
        [ benchIOSink value "indexed" indexed
        , benchIOSink value "indexedR" (indexedR value)
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
        [ bgroup  (o_1_space_prefix moduleName) $ Prelude.concat
            [ o_1_space_mapping size
            , o_1_space_filtering size
            , o_1_space_filteringX4 size
            , o_1_space_inserting size
            , o_1_space_indexing size
            ]
        , bgroup  (o_n_space_prefix moduleName) $ o_n_space_traversable size
        ]
