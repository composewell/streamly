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

import Streamly.Prelude (SerialT)
import Streamly.Internal.Data.Fold (Fold(..))
import Streamly.Internal.Data.IsMap.HashMap ()

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Internal.Data.Stream.IsStream as IP
import qualified Streamly.Prelude as S

import Gauge
import Streamly.Benchmark.Common
import Prelude hiding (all, any, take, unzip, sequence_, filter)

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrM value n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE source #-}
source :: (S.MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
source = sourceUnfoldrM

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: (S.IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f = bench name $ nfIO $ randomRIO (1,1) >>= f . source value

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

{-# INLINE any #-}
any :: (Monad m, Ord a) => a -> SerialT m a -> m Bool
any value = IP.fold (FL.any (> value))

{-# INLINE all #-}
all :: (Monad m, Ord a) => a -> SerialT m a -> m Bool
all value = IP.fold (FL.all (<= value))

{-# INLINE take #-}
take :: Monad m => Int -> SerialT m a -> m ()
take value = IP.fold (FL.take value FL.drain)

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
filter :: Monad m => Int -> SerialT m Int -> m ()
filter _ = IP.fold (FL.filter even FL.drain)

{-# INLINE foldFilter #-}
foldFilter :: Monad m => Int -> SerialT m Int -> m ()
foldFilter _ = IP.fold (FL.foldFilter (FL.satisfy even) FL.drain)

{-# INLINE foldFilter2 #-}
foldFilter2 :: Monad m => Int -> SerialT m Int -> m ()
foldFilter2 _ =
    IP.fold
        $ FL.foldFilter (FL.satisfy even)
        $ FL.foldFilter (FL.satisfy odd) FL.drain

-------------------------------------------------------------------------------
-- Splitting by serial application
-------------------------------------------------------------------------------

{-# INLINE takeEndBy_ #-}
takeEndBy_ :: Monad m => Int -> SerialT m Int -> m ()
takeEndBy_ value = IP.fold (FL.takeEndBy_ (>= value) FL.drain)

{-# INLINE many #-}
many :: Monad m => SerialT m Int -> m ()
many = IP.fold (FL.many (FL.take 1 FL.drain) FL.drain)

{-# INLINE splitAllAny #-}
splitAllAny :: Monad m => Int -> SerialT m Int -> m (Bool, Bool)
splitAllAny value =
    IP.fold
        (FL.serialWith (,)
            (FL.all (<= (value `div` 2)))
            (FL.any (> value))
        )

{-# INLINE serial_ #-}
serial_ :: Monad m => Int -> SerialT m Int -> m Bool
serial_ value =
    IP.fold
        (FL.serial_
            (FL.all (<= (value `div` 2)))
            (FL.any (> value))
        )

{-# INLINE shortest #-}
shortest :: Monad m => SerialT m Int -> m (Either Int Int)
shortest = IP.fold (FL.shortest FL.sum FL.length)

{-# INLINE longest #-}
longest :: Monad m => SerialT m Int -> m (Either Int Int)
longest = IP.fold (FL.longest FL.sum FL.length)

{-# INLINE foldBreak #-}
foldBreak :: Monad m => SerialT m Int -> m ()
foldBreak s = do
    (r, s1) <- IP.foldBreak (FL.take 1 FL.length) s
    when (r /= 0) $ foldBreak s1

-------------------------------------------------------------------------------
-- Distributing by parallel application
-------------------------------------------------------------------------------

{-# INLINE teeSumLength #-}
teeSumLength :: Monad m => SerialT m Int -> m (Int, Int)
teeSumLength = IP.fold (FL.teeWith (,) FL.sum FL.length)

{-# INLINE teeAllAny #-}
teeAllAny :: (Monad m, Ord a) => a -> SerialT m a -> m (Bool, Bool)
teeAllAny value = IP.fold (FL.teeWith (,) all_ any_)

    where

    all_ = FL.all (<= value)
    any_ = FL.any (> value)

{-# INLINE teeWithFst #-}
teeWithFst :: Monad m => SerialT m Int -> m (Int, Int)
teeWithFst = IP.fold (FL.teeWithFst (,) FL.sum FL.length)

{-# INLINE teeWithMin #-}
teeWithMin :: Monad m => SerialT m Int -> m (Int, Int)
teeWithMin = IP.fold (FL.teeWithMin (,) FL.sum FL.length)

{-# INLINE distribute #-}
distribute :: Monad m => SerialT m Int -> m [Int]
distribute = IP.fold (FL.distribute [FL.sum, FL.length])

-------------------------------------------------------------------------------
-- Partitioning
-------------------------------------------------------------------------------

{-# INLINE oddEven #-}
oddEven :: Int -> Either Int Int
oddEven x = if odd x then Left x else Right x

{-# INLINE partition #-}
partition :: Monad m => SerialT m Int -> m (Int, Int)
partition = IP.fold $ FL.lmap oddEven (FL.partition FL.sum FL.length)

{-# INLINE partitionByFstM #-}
partitionByFstM :: Monad m => SerialT m Int -> m (Int, Int)
partitionByFstM =
    IP.fold (FL.partitionByFstM (return . oddEven) FL.sum FL.length)

{-# INLINE partitionByMinM #-}
partitionByMinM :: Monad m => SerialT m Int -> m (Int, Int)
partitionByMinM =
    IP.fold (FL.partitionByMinM (return . oddEven) FL.sum FL.length)

{-# INLINE demuxWith  #-}
demuxWith :: (Monad m, Ord k) =>
    (a -> k) -> (a -> m (Fold m a b)) -> SerialT m a -> m (Map k b)
demuxWith f g = S.fold (FL.demuxWith f g)

{-# INLINE demuxWithInt  #-}
demuxWithInt :: Monad m =>
    (a -> Int) -> (a -> m (Fold m a b)) -> SerialT m a -> m (IntMap b)
demuxWithInt f g = S.fold (FL.demuxWith f g)

{-# INLINE demuxWithHash  #-}
demuxWithHash :: (Monad m, Ord k, Hashable k) =>
    (a -> k) -> (a -> m (Fold m a b)) -> SerialT m a -> m (HashMap k b)
demuxWithHash f g = S.fold (FL.demuxWith f g)

{-# INLINE demuxMutWith  #-}
demuxMutWith :: (MonadIO m, Ord k) =>
    (a -> k) -> (a -> m (Fold m a b)) -> SerialT m a -> m (Map k b)
demuxMutWith f g = S.fold (FL.demuxMutWith f g)

{-# INLINE demuxMutWithHash  #-}
demuxMutWithHash :: (MonadIO m, Ord k, Hashable k) =>
    (a -> k) -> (a -> m (Fold m a b)) -> SerialT m a -> m (HashMap k b)
demuxMutWithHash f g = S.fold (FL.demuxMutWith f g)

{-# INLINE classifyWith #-}
classifyWith ::
       (Monad m, Ord k, Num a) => (a -> k) -> SerialT m a -> m (Map k a)
classifyWith f = S.fold (FL.classifyWith f FL.sum)

{-# INLINE classifyWithInt #-}
classifyWithInt ::
       (MonadIO m, Num a) => (a -> Int) -> SerialT m a -> m (IntMap a)
classifyWithInt f = S.fold (FL.classifyWith f FL.sum)

{-# INLINE classifyMutWith #-}
classifyMutWith ::
       (MonadIO m, Ord k, Num a) => (a -> k) -> SerialT m a -> m (Map k a)
classifyMutWith f = S.fold (FL.classifyMutWith f FL.sum)

{-# INLINE classifyMutWithInt #-}
classifyMutWithInt ::
       (MonadIO m, Num a) => (a -> Int) -> SerialT m a -> m (IntMap a)
classifyMutWithInt f = S.fold (FL.classifyMutWith f FL.sum)

{-# INLINE classifyMutWithHash #-}
classifyMutWithHash :: (MonadIO m, Ord k, Num a, Hashable k) =>
    (a -> k) -> SerialT m a -> m (HashMap k a)
classifyMutWithHash f = S.fold (FL.classifyMutWith f FL.sum)

-------------------------------------------------------------------------------
-- unzip
-------------------------------------------------------------------------------

{-# INLINE unzip #-}
unzip :: Monad m => SerialT m Int -> m (Int, Int)
unzip = IP.fold $ FL.lmap (\a -> (a, a)) (FL.unzip FL.sum FL.length)

{-# INLINE unzipWithFstM #-}
unzipWithFstM :: Monad m => SerialT m Int -> m (Int, Int)
unzipWithFstM = do
    let f = \a -> return (a + 1, a)
    IP.fold (FL.unzipWithFstM f FL.sum FL.length)

{-# INLINE unzipWithMinM #-}
unzipWithMinM :: Monad m => SerialT m Int -> m (Int, Int)
unzipWithMinM = do
    let f = \a -> return (a + 1, a)
    IP.fold (FL.unzipWithMinM f FL.sum FL.length)

-------------------------------------------------------------------------------
-- Nested
-------------------------------------------------------------------------------

{-# INLINE unfoldMany #-}
unfoldMany :: Int -> Benchmarkable
unfoldMany val =
    nfIO
        $ IP.fold (FL.unfoldMany (Unfold.replicateM val) FL.drain)
        $ IP.fromPure (randomRIO (1, 1 :: Int))

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Fold"

o_1_space_serial_elimination :: Int -> [Benchmark]
o_1_space_serial_elimination value =
    [ bgroup "elimination"
        [ benchIOSink value "drain" (S.fold FL.drain)
        , benchIOSink value "drainBy" (S.fold (FL.drainBy return))
        , benchIOSink value "drainN" (S.fold (FL.drainN value))
        , benchIOSink value "last" (S.fold FL.last)
        , benchIOSink value "nub" (S.fold FL.nub)
        , benchIOSink value "length" (S.fold FL.length)
        , benchIOSink value "top" (S.fold $ FL.top 10)
        , benchIOSink value "bottom" (S.fold $ FL.bottom 10)
        , benchIOSink value "sum" (S.fold FL.sum)
        , benchIOSink value "sum (foldMap)" (S.fold (FL.foldMap Sum))
        , benchIOSink value "product" (S.fold FL.product)
        , benchIOSink value "maximumBy" (S.fold (FL.maximumBy compare))
        , benchIOSink value "maximum" (S.fold FL.maximum)
        , benchIOSink value "minimumBy" (S.fold (FL.minimumBy compare))
        , benchIOSink value "minimum" (S.fold FL.minimum)
        , benchIOSink
              value
              "mean"
              (S.fold FL.mean . S.map (fromIntegral :: Int -> Double))
        , benchIOSink
              value
              "variance"
              (S.fold FL.variance . S.map (fromIntegral :: Int -> Double))
        , benchIOSink
              value
              "stdDev"
              (S.fold FL.stdDev . S.map (fromIntegral :: Int -> Double))
        , benchIOSink
              value
              "mconcat"
              (S.fold FL.mconcat . S.map (Last . Just))
        , benchIOSink
              value
              "foldMap"
              (S.fold (FL.foldMap (Last . Just)))
        , benchIOSink
              value
              "foldMapM"
              (S.fold (FL.foldMapM (return . Last . Just)))
        , benchIOSink value "index" (S.fold (FL.index (value + 1)))
        , benchIOSink value "head" (S.fold FL.head)
        , benchIOSink value "find" (S.fold (FL.find (== (value + 1))))
        , benchIOSink
              value
              "lookup"
              (S.fold (FL.lmap (\a -> (a, a)) (FL.lookup (value + 1))))
        , benchIOSink
              value
              "findIndex"
              (S.fold (FL.findIndex (== (value + 1))))
        , benchIOSink
              value
              "elemIndex"
              (S.fold (FL.elemIndex (value + 1)))
        , benchIOSink value "null" (S.fold FL.null)
        , benchIOSink value "elem" (S.fold (FL.elem (value + 1)))
        , benchIOSink value "notElem" (S.fold (FL.notElem (value + 1)))
        , benchIOSink value "all" $ all value
        , benchIOSink value "any" $ any value
        , benchIOSink value "take" $ take value
        , benchIOSink value "takeEndBy_" $ takeEndBy_ value
        , benchIOSink value "and" (S.fold FL.and . S.map (<= (value + 1)))
        , benchIOSink value "or" (S.fold FL.or . S.map (> (value + 1)))
        ]
    ]

o_1_space_serial_transformation :: Int -> [Benchmark]
o_1_space_serial_transformation value =
    [ bgroup "transformation"
        [ benchIOSink value "map" (S.fold (FL.lmap (+ 1) FL.drain))
        , let f x = if even x then Just x else Nothing
              fld = FL.mapMaybe f FL.drain
           in benchIOSink value "mapMaybe" (S.fold fld)
        , benchIOSink
              value
              "rsequence"
              (S.fold (FL.rmapM id (return <$> FL.drain)))
        , benchIOSink value "rmapM" (S.fold (FL.rmapM return FL.drain))
        , benchIOSink
              value
              "pipe-mapM"
              (S.fold
                   (FL.transform
                        (Pipe.mapM (\x -> return $ x + 1))
                        FL.drain))
        , benchIOSink
            value
            "fold-scan"
            (S.fold $ FL.scan FL.sum FL.drain)
        , benchIOSink
            value
            "fold-scanMany"
            (S.fold $ FL.scanMany (FL.take 2 FL.drain) FL.drain)
        , benchIOSink
            value
            "fold-postscan"
            (S.fold $ FL.postscan FL.sum FL.drain)
        ]
    ]

o_1_space_serial_composition :: Int -> [Benchmark]
o_1_space_serial_composition value =
      [ bgroup
            "composition"
            [ benchIOSink value "filter even" $ filter value
            , benchIOSink value "foldFilter even" $ foldFilter value
            , benchIOSink value "foldFilter even, odd" $ foldFilter2 value
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
    [ benchIOSink value "sequence_/100" $ S.fold (sequence_ (value `div` 100))
    ]

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [ bgroup "elimination"
      -- Left folds for building a structure are inherently non-streaming
      -- as the structure cannot be lazily consumed until fully built.
            [
              benchIOSink value "toList" (S.fold FL.toList)
            , benchIOSink value "toListRev" (S.fold FL.toListRev)
            , benchIOSink value "toStream"
                (S.fold FL.toStream
                    :: SerialT IO a -> IO (SerialT Identity a))
            , benchIOSink value "toStreamRev"
                (S.fold FL.toStreamRev
                    :: SerialT IO a -> IO (SerialT Identity a))
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
