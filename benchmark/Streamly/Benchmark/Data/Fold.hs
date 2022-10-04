-- |
-- Module      : Streamly.Benchmark.Data.Fold
-- Copyright   : (c) 2018 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Main (main) where

import Control.DeepSeq (NFData(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.IntMap.Strict (IntMap)
import Data.Monoid (Last(..), Sum(..))
import System.Random (randomRIO)

import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Fold (Fold(..))
import Streamly.Internal.Data.IsMap.HashMap ()

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Internal.Data.Stream as Stream

import Gauge
import Streamly.Benchmark.Common
import Prelude hiding (all, any, take, unzip, sequence_, filter)

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> Stream m Int
sourceUnfoldrM value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE source #-}
source :: Monad m => Int -> Int -> Stream m Int
source = sourceUnfoldrM

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: NFData b
    => Int -> String -> (Stream IO Int -> IO b) -> Benchmark
benchIOSink value name f = bench name $ nfIO $ randomRIO (1,1) >>= f . source value

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

{-# INLINE any #-}
any :: (Monad m, Ord a) => a -> Stream m a -> m Bool
any value = Stream.fold (FL.any (> value))

{-# INLINE all #-}
all :: (Monad m, Ord a) => a -> Stream m a -> m Bool
all value = Stream.fold (FL.all (<= value))

{-# INLINE take #-}
take :: Monad m => Int -> Stream m a -> m ()
take value = Stream.fold (FL.take value FL.drain)

{-# INLINE sequence_ #-}
sequence_ :: Monad m => Int -> Fold m a ()
sequence_ value =
    foldr f (FL.fromPure ()) (Prelude.replicate value (FL.take 1 FL.drain))

    where

    {-# INLINE f #-}
    f m k = FL.concatMap (const k) m

-------------------------------------------------------------------------------
-- Filter
-------------------------------------------------------------------------------

{-# INLINE filter #-}
filter :: Monad m => Int -> Stream m Int -> m ()
filter _ = Stream.fold (FL.filter even FL.drain)

{-# INLINE scanMaybe #-}
scanMaybe :: Monad m => Int -> Stream m Int -> m ()
scanMaybe _ = Stream.fold (FL.scanMaybe (FL.filtering even) FL.drain)

{-# INLINE scanMaybe2 #-}
scanMaybe2 :: Monad m => Int -> Stream m Int -> m ()
scanMaybe2 _ =
    Stream.fold
        $ FL.scanMaybe (FL.filtering even)
        $ FL.scanMaybe (FL.filtering odd) FL.drain

-------------------------------------------------------------------------------
-- Splitting by serial application
-------------------------------------------------------------------------------

{-# INLINE takeEndBy_ #-}
takeEndBy_ :: Monad m => Int -> Stream m Int -> m ()
takeEndBy_ value = Stream.fold (FL.takeEndBy_ (>= value) FL.drain)

{-# INLINE many #-}
many :: Monad m => Stream m Int -> m ()
many = Stream.fold (FL.many (FL.take 1 FL.drain) FL.drain)

{-# INLINE splitAllAny #-}
splitAllAny :: Monad m => Int -> Stream m Int -> m (Bool, Bool)
splitAllAny value =
    Stream.fold
        (FL.serialWith (,)
            (FL.all (<= (value `div` 2)))
            (FL.any (> value))
        )

{-# INLINE serial_ #-}
serial_ :: Monad m => Int -> Stream m Int -> m Bool
serial_ value =
    Stream.fold
        (FL.serial_
            (FL.all (<= (value `div` 2)))
            (FL.any (> value))
        )

{-# INLINE shortest #-}
shortest :: Monad m => Stream m Int -> m (Either Int Int)
shortest = Stream.fold (FL.shortest FL.sum FL.length)

{-# INLINE longest #-}
longest :: Monad m => Stream m Int -> m (Either Int Int)
longest = Stream.fold (FL.longest FL.sum FL.length)

{-# INLINE foldBreak #-}
foldBreak :: Monad m => Stream m Int -> m ()
foldBreak s = do
    (r, s1) <- Stream.foldBreak (FL.take 1 FL.length) s
    when (r /= 0) $ foldBreak s1

-------------------------------------------------------------------------------
-- Distributing by parallel application
-------------------------------------------------------------------------------

{-# INLINE teeSumLength #-}
teeSumLength :: Monad m => Stream m Int -> m (Int, Int)
teeSumLength = Stream.fold (FL.teeWith (,) FL.sum FL.length)

{-# INLINE teeAllAny #-}
teeAllAny :: (Monad m, Ord a) => a -> Stream m a -> m (Bool, Bool)
teeAllAny value = Stream.fold (FL.teeWith (,) all_ any_)

    where

    all_ = FL.all (<= value)
    any_ = FL.any (> value)

{-# INLINE teeWithFst #-}
teeWithFst :: Monad m => Stream m Int -> m (Int, Int)
teeWithFst = Stream.fold (FL.teeWithFst (,) FL.sum FL.length)

{-# INLINE teeWithMin #-}
teeWithMin :: Monad m => Stream m Int -> m (Int, Int)
teeWithMin = Stream.fold (FL.teeWithMin (,) FL.sum FL.length)

{-# INLINE distribute #-}
distribute :: Monad m => Stream m Int -> m [Int]
distribute = Stream.fold (FL.distribute [FL.sum, FL.length])

-------------------------------------------------------------------------------
-- Partitioning
-------------------------------------------------------------------------------

{-# INLINE oddEven #-}
oddEven :: Int -> Either Int Int
oddEven x = if odd x then Left x else Right x

{-# INLINE partition #-}
partition :: Monad m => Stream m Int -> m (Int, Int)
partition = Stream.fold $ FL.lmap oddEven (FL.partition FL.sum FL.length)

{-# INLINE partitionByFstM #-}
partitionByFstM :: Monad m => Stream m Int -> m (Int, Int)
partitionByFstM =
    Stream.fold (FL.partitionByFstM (return . oddEven) FL.sum FL.length)

{-# INLINE partitionByMinM #-}
partitionByMinM :: Monad m => Stream m Int -> m (Int, Int)
partitionByMinM =
    Stream.fold (FL.partitionByMinM (return . oddEven) FL.sum FL.length)

{-# INLINE demuxWith  #-}
demuxWith :: (Monad m, Ord k) =>
    (a -> k) -> (a -> m (Fold m a b)) -> Stream m a -> m (Map k b)
demuxWith f g = Stream.fold (FL.demuxWith f g)

{-# INLINE demuxWithInt  #-}
demuxWithInt :: Monad m =>
    (a -> Int) -> (a -> m (Fold m a b)) -> Stream m a -> m (IntMap b)
demuxWithInt f g = Stream.fold (FL.demuxWith f g)

{-# INLINE demuxWithHash  #-}
demuxWithHash :: (Monad m, Ord k, Hashable k) =>
    (a -> k) -> (a -> m (Fold m a b)) -> Stream m a -> m (HashMap k b)
demuxWithHash f g = Stream.fold (FL.demuxWith f g)

{-# INLINE demuxMutWith  #-}
demuxMutWith :: (MonadIO m, Ord k) =>
    (a -> k) -> (a -> m (Fold m a b)) -> Stream m a -> m (Map k b)
demuxMutWith f g = Stream.fold (FL.demuxMutWith f g)

{-# INLINE demuxMutWithHash  #-}
demuxMutWithHash :: (MonadIO m, Ord k, Hashable k) =>
    (a -> k) -> (a -> m (Fold m a b)) -> Stream m a -> m (HashMap k b)
demuxMutWithHash f g = Stream.fold (FL.demuxMutWith f g)

{-# INLINE classifyWith #-}
classifyWith ::
       (Monad m, Ord k, Num a) => (a -> k) -> Stream m a -> m (Map k a)
classifyWith f = Stream.fold (FL.classifyWith f FL.sum)

{-# INLINE classifyWithInt #-}
classifyWithInt ::
       (Monad m, Num a) => (a -> Int) -> Stream m a -> m (IntMap a)
classifyWithInt f = Stream.fold (FL.classifyWith f FL.sum)

{-# INLINE classifyMutWith #-}
classifyMutWith ::
       (MonadIO m, Ord k, Num a) => (a -> k) -> Stream m a -> m (Map k a)
classifyMutWith f = Stream.fold (FL.classifyMutWith f FL.sum)

{-# INLINE classifyMutWithInt #-}
classifyMutWithInt ::
       (MonadIO m, Num a) => (a -> Int) -> Stream m a -> m (IntMap a)
classifyMutWithInt f = Stream.fold (FL.classifyMutWith f FL.sum)

{-# INLINE classifyMutWithHash #-}
classifyMutWithHash :: (MonadIO m, Ord k, Num a, Hashable k) =>
    (a -> k) -> Stream m a -> m (HashMap k a)
classifyMutWithHash f = Stream.fold (FL.classifyMutWith f FL.sum)

-------------------------------------------------------------------------------
-- unzip
-------------------------------------------------------------------------------

{-# INLINE unzip #-}
unzip :: Monad m => Stream m Int -> m (Int, Int)
unzip = Stream.fold $ FL.lmap (\a -> (a, a)) (FL.unzip FL.sum FL.length)

{-# INLINE unzipWithFstM #-}
unzipWithFstM :: Monad m => Stream m Int -> m (Int, Int)
unzipWithFstM = do
    let f = \a -> return (a + 1, a)
    Stream.fold (FL.unzipWithFstM f FL.sum FL.length)

{-# INLINE unzipWithMinM #-}
unzipWithMinM :: Monad m => Stream m Int -> m (Int, Int)
unzipWithMinM = do
    let f = \a -> return (a + 1, a)
    Stream.fold (FL.unzipWithMinM f FL.sum FL.length)

-------------------------------------------------------------------------------
-- Nested
-------------------------------------------------------------------------------

{-# INLINE unfoldMany #-}
unfoldMany :: Int -> Benchmarkable
unfoldMany val =
    nfIO
        $ Stream.fold (FL.unfoldMany Unfold.replicateM FL.drain)
        $ Stream.fromPure (val, randomRIO (1, 1 :: Int))

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Fold"

o_1_space_serial_elimination :: Int -> [Benchmark]
o_1_space_serial_elimination value =
    [ bgroup "elimination"
        [ benchIOSink value "drain" (Stream.fold FL.drain)
        , benchIOSink value "drainBy" (Stream.fold (FL.drainBy return))
        , benchIOSink value "drainN" (Stream.fold (FL.drainN value))
        , benchIOSink value "last" (Stream.fold FL.last)
        , benchIOSink value "nub" (Stream.fold FL.nub)
        , benchIOSink value "length" (Stream.fold FL.length)
        , benchIOSink value "top" (Stream.fold $ FL.top 10)
        , benchIOSink value "bottom" (Stream.fold $ FL.bottom 10)
        , benchIOSink value "sum" (Stream.fold FL.sum)
        , benchIOSink value "sum (foldMap)" (Stream.fold (FL.foldMap Sum))
        , benchIOSink value "product" (Stream.fold FL.product)
        , benchIOSink value "maximumBy" (Stream.fold (FL.maximumBy compare))
        , benchIOSink value "maximum" (Stream.fold FL.maximum)
        , benchIOSink value "minimumBy" (Stream.fold (FL.minimumBy compare))
        , benchIOSink value "minimum" (Stream.fold FL.minimum)
        , benchIOSink
              value
              "mean"
              (Stream.fold FL.mean . fmap (fromIntegral :: Int -> Double))
        , benchIOSink
              value
              "variance"
              (Stream.fold FL.variance . fmap (fromIntegral :: Int -> Double))
        , benchIOSink
              value
              "stdDev"
              (Stream.fold FL.stdDev . fmap (fromIntegral :: Int -> Double))
        , benchIOSink
              value
              "mconcat"
              (Stream.fold FL.mconcat . fmap (Last . Just))
        , benchIOSink
              value
              "foldMap"
              (Stream.fold (FL.foldMap (Last . Just)))
        , benchIOSink
              value
              "foldMapM"
              (Stream.fold (FL.foldMapM (return . Last . Just)))
        , benchIOSink value "index" (Stream.fold (FL.index (value + 1)))
        , benchIOSink value "head" (Stream.fold FL.head)
        , benchIOSink value "find" (Stream.fold (FL.find (== (value + 1))))
        , benchIOSink
              value
              "lookup"
              (Stream.fold (FL.lmap (\a -> (a, a)) (FL.lookup (value + 1))))
        , benchIOSink
              value
              "findIndex"
              (Stream.fold (FL.findIndex (== (value + 1))))
        , benchIOSink
              value
              "elemIndex"
              (Stream.fold (FL.elemIndex (value + 1)))
        , benchIOSink value "null" (Stream.fold FL.null)
        , benchIOSink value "elem" (Stream.fold (FL.elem (value + 1)))
        , benchIOSink value "notElem" (Stream.fold (FL.notElem (value + 1)))
        , benchIOSink value "all" $ all value
        , benchIOSink value "any" $ any value
        , benchIOSink value "take" $ take value
        , benchIOSink value "takeEndBy_" $ takeEndBy_ value
        , benchIOSink value "and" (Stream.fold FL.and . fmap (<= (value + 1)))
        , benchIOSink value "or" (Stream.fold FL.or . fmap (> (value + 1)))
        ]
    ]

o_1_space_serial_transformation :: Int -> [Benchmark]
o_1_space_serial_transformation value =
    [ bgroup "transformation"
        [ benchIOSink value "map" (Stream.fold (FL.lmap (+ 1) FL.drain))
        , let f x = if even x then Just x else Nothing
              fld = FL.mapMaybe f FL.drain
           in benchIOSink value "mapMaybe" (Stream.fold fld)
        , benchIOSink
              value
              "rsequence"
              (Stream.fold (FL.rmapM id (return <$> FL.drain)))
        , benchIOSink value "rmapM" (Stream.fold (FL.rmapM return FL.drain))
        , benchIOSink
              value
              "pipe-mapM"
              (Stream.fold
                   (FL.transform
                        (Pipe.mapM (\x -> return $ x + 1))
                        FL.drain))
        , benchIOSink
            value
            "fold-scan"
            (Stream.fold $ FL.scan FL.sum FL.drain)
        , benchIOSink
            value
            "fold-scanMany"
            (Stream.fold $ FL.scanMany (FL.take 2 FL.drain) FL.drain)
        , benchIOSink
            value
            "fold-postscan"
            (Stream.fold $ FL.postscan FL.sum FL.drain)
        ]
    ]

o_1_space_serial_composition :: Int -> [Benchmark]
o_1_space_serial_composition value =
      [ bgroup
            "composition"
            [ benchIOSink value "filter even" $ filter value
            , benchIOSink value "scanMaybe even" $ scanMaybe value
            , benchIOSink value "scanMaybe even, odd" $ scanMaybe2 value
            , benchIOSink value "foldBreak (recursive)" foldBreak
            , benchIOSink value "serialWith (all, any)" $ splitAllAny value
            , benchIOSink value "serial_ (all, any)" $ serial_ value
            , benchIOSink value "tee (all, any)" $ teeAllAny value
            , benchIOSink value "many drain (take 1)" many
            , bench "unfoldMany" $ unfoldMany value
            , benchIOSink value "shortest (sum, length)" shortest
            , benchIOSink value "longest (sum, length)" longest
            , benchIOSink value "tee (sum, length)" teeSumLength
            , benchIOSink value "teeWithFst (sum, length)" teeWithFst
            , benchIOSink value "teeWithMin (sum, length)" teeWithMin
            , benchIOSink value "distribute [sum, length]" distribute
            , benchIOSink value "partition (sum, length)" partition
            , benchIOSink value "partitionByFstM (sum, length)" partitionByFstM
            , benchIOSink value "partitionByMinM (sum, length)" partitionByMinM
            , benchIOSink value "unzip (sum, length)" unzip
            , benchIOSink value "unzipWithFstM (sum, length)" unzipWithFstM
            , benchIOSink value "unzipWithMinM (sum, length)" unzipWithMinM
            ]
      ]

o_n_space_serial :: Int -> [Benchmark]
o_n_space_serial value =
    [ benchIOSink value "sequence_/100" $ Stream.fold (sequence_ (value `div` 100))
    ]

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [ bgroup "elimination"
      -- Left folds for building a structure are inherently non-streaming
      -- as the structure cannot be lazily consumed until fully built.
            [
              benchIOSink value "toList" (Stream.fold FL.toList)
            , benchIOSink value "toListRev" (Stream.fold FL.toListRev)
            , benchIOSink value "toStream"
                (Stream.fold FL.toStream
                    :: Stream IO a -> IO (Stream Identity a))
            , benchIOSink value "toStreamRev"
                (Stream.fold FL.toStreamRev
                    :: Stream IO a -> IO (Stream Identity a))
            ]
    , bgroup "key-value"
            [
              benchIOSink value "demuxWith (64 buckets) [sum, length]"
                $ demuxWith (getKey 64) (getFold . getKey 64)
            , benchIOSink value "demuxWithInt (64 buckets) [sum, length]"
                $ demuxWithInt (getKey 64) (getFold . getKey 64)
            , benchIOSink value "demuxWithHash (64 buckets) [sum, length]"
                $ demuxWithHash (getKey 64) (getFold . getKey 64)
            , benchIOSink value "demuxMutWith (64 buckets) [sum, length]"
                $ demuxMutWith (getKey 64) (getFold . getKey 64)
            , benchIOSink value "demuxMutWithHash (64 buckets) [sum, length]"
                $ demuxMutWithHash (getKey 64) (getFold . getKey 64)

            -- classify: immutable
            , benchIOSink value "classifyWith (64 buckets) sum"
                $ classifyWith (getKey 64)
            , benchIOSink value "classifyWithInt (64 buckets) sum"
                $ classifyWithInt (getKey 64)

            -- classify: mutable cells
            , benchIOSink value "classifyMutWith (single bucket) sum"
                $ classifyMutWith (getKey 1)
            , benchIOSink value "classifyMutWith (64 buckets) sum"
                $ classifyMutWith (getKey 64)
            , benchIOSink value "classifyMutWith (max buckets) sum"
                $ classifyMutWith (getKey value)
            , benchIOSink value "classifyMutWithInt (64 buckets) sum"
                $ classifyMutWithInt (getKey 64)
            , benchIOSink value "classifyMutWithHash (single bucket) sum"
                $ classifyMutWithHash (getKey 1)
            , benchIOSink value "classifyMutWithHash (64 buckets) sum"
                $ classifyMutWithHash (getKey 64)
            , benchIOSink value "classifyMutWithHash (max buckets) sum"
                $ classifyMutWithHash (getKey value)
            ]
    ]

    where

    getKey buckets = (`mod` buckets)

    getFold k =
        return $ case k of
            0 -> FL.sum
            1 -> FL.length
            _ -> FL.length

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks value =
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [ o_1_space_serial_elimination value
            , o_1_space_serial_transformation value
            , o_1_space_serial_composition value
            ]
        , bgroup (o_n_space_prefix moduleName) (o_n_space_serial value)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        ]
