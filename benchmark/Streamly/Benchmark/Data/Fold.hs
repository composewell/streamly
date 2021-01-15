-- |
-- Module      : Streamly.Benchmark.Data.Fold
-- Copyright   : (c) 2018 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.DeepSeq (NFData(..))
import Data.Map.Strict (Map)
import Data.Monoid (Last(..), Sum(..))
import System.Random (randomRIO)

import Streamly.Prelude (SerialT)
import Streamly.Internal.Data.Fold (Fold(..))

import qualified Data.Map.Strict as Map
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold.Types as FL
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Internal.Data.Sink as Sink
import qualified Streamly.Internal.Data.Stream.IsStream as IP
import qualified Streamly.Prelude as S

import Gauge
import Streamly.Benchmark.Common
import Prelude hiding (all, any, take, unzip)

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

{-# INLINE takeLE #-}
takeLE :: Monad m => Int -> SerialT m a -> m ()
takeLE value = IP.fold (FL.takeLE value FL.drain)

-------------------------------------------------------------------------------
-- Splitting by serial application
-------------------------------------------------------------------------------

{-# INLINE sliceSepBy #-}
sliceSepBy :: Monad m => Int -> SerialT m Int -> m ()
sliceSepBy value = IP.fold (FL.sliceSepBy (>= value) FL.drain)

{-# INLINE many #-}
many :: Monad m => SerialT m Int -> m ()
many = IP.fold (FL.many FL.drain (FL.takeLE 1 FL.drain))

{-# INLINE splitAllAny #-}
splitAllAny :: Monad m => Int -> SerialT m Int -> m (Bool, Bool)
splitAllAny value =
    IP.fold
        (FL.splitWith (,)
            (FL.all (<= (value `div` 2)))
            (FL.any (> value))
        )

{-
{-# INLINE split_ #-}
split_ :: Monad m
    => Int -> SerialT m Int -> m ()
split_ value =
    IP.fold
        (FL.split_
            (FL.all (<= (value `div` 2)))
            (FL.any (> value))
        )
-}

-------------------------------------------------------------------------------
-- Distributing by parallel application
-------------------------------------------------------------------------------

{-# INLINE teeSumLength #-}
teeSumLength :: Monad m => SerialT m Int -> m (Int, Int)
teeSumLength = IP.fold (FL.teeWith (,) FL.sum FL.length)

{-# INLINE teeApplicative #-}
teeApplicative :: (Monad m, Ord a) => a -> SerialT m a -> m (Bool, Bool)
teeApplicative value =
    IP.fold ((,) <$> FL.all (<= value) <*> FL.any (> value))

{-# INLINE distribute #-}
distribute :: Monad m => SerialT m Int -> m [Int]
distribute = IP.fold (FL.distribute [FL.sum, FL.length])

-------------------------------------------------------------------------------
-- Partitioning
-------------------------------------------------------------------------------

{-# INLINE partition #-}
partition :: Monad m => SerialT m Int -> m (Int, Int)
partition =
    let f a =
            if odd a
            then Left a
            else Right a
     in IP.fold $ FL.lmap f (FL.partition FL.sum FL.length)

{-# INLINE demuxWith  #-}
demuxWith ::
       (Monad m, Ord k)
    => (a -> (k, a'))
    -> Map k (Fold m a' b)
    -> SerialT m a
    -> m (Map k b)
demuxWith f mp = S.fold (FL.demuxWith f mp)

{-# INLINE demuxDefaultWith #-}
demuxDefaultWith ::
       (Monad m, Ord k, Num b)
    => (a -> (k, b))
    -> Map k (Fold m b b)
    -> SerialT m a
    -> m (Map k b, b)
demuxDefaultWith f mp = S.fold (FL.demuxDefaultWith f mp (FL.lmap snd FL.sum))

{-# INLINE classifyWith #-}
classifyWith ::
       (Monad m, Ord k, Num a) => (a -> k) -> SerialT m a -> m (Map k a)
classifyWith f = S.fold (FL.classifyWith f FL.sum)

-------------------------------------------------------------------------------
-- unzip
-------------------------------------------------------------------------------

{-# INLINE unzip #-}
unzip :: Monad m => SerialT m Int -> m (Int, Int)
unzip = IP.fold $ FL.lmap (\a -> (a, a)) (FL.unzip FL.sum FL.length)

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
        , benchIOSink value "sink" (S.fold $ Sink.toFold Sink.drain)
        , benchIOSink value "last" (S.fold FL.last)
        , benchIOSink value "length" (S.fold FL.length)
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
        , benchIOSink value "takeLE" $ takeLE value
        , benchIOSink value "sliceSepBy" $ sliceSepBy value
        , benchIOSink value "and" (S.fold FL.and . S.map (<= (value + 1)))
        , benchIOSink value "or" (S.fold FL.or . S.map (> (value + 1)))
        ]
    ]

o_1_space_serial_transformation :: Int -> [Benchmark]
o_1_space_serial_transformation value =
    [ bgroup "transformation"
        [ benchIOSink value "lmap" (S.fold (FL.lmap (+ 1) FL.drain))
        , let f x = if even x then Just x else Nothing
              fld = FL.mapMaybe f FL.drain
           in benchIOSink value "mapMaybe" (S.fold fld)
        , benchIOSink
              value
              "sequence"
              (S.fold (FL.sequence (return <$> FL.drain)))
        , benchIOSink value "mapM" (S.fold (FL.mapM return FL.drain))
        , benchIOSink
              value
              "pipe-mapM"
              (S.fold
                   (FL.transform
                        (Pipe.mapM (\x -> return $ x + 1))
                        FL.drain))
        ]
    ]

o_1_space_serial_composition :: Int -> [Benchmark]
o_1_space_serial_composition value =
      [ bgroup
            "composition"
            [ benchIOSink value "splitWith (all, any)" $ splitAllAny value
            , benchIOSink value "teeApplicative (all, any)"
                $ teeApplicative value
            , benchIOSink value "many drain (takeLE 1)" many
            , benchIOSink value "tee (sum, length)" teeSumLength
            , benchIOSink value "distribute [sum, length]" distribute
            , benchIOSink value "partition (sum, length)" partition
            , benchIOSink value "unzip (sum, length)" unzip
            , benchIOSink value "demuxDefaultWith [sum, length] sum"
                  $ demuxDefaultWith fn mp
            , benchIOSink value "demuxWith [sum, length]" $ demuxWith fn mp
            , benchIOSink value "classifyWith sum" $ classifyWith (fst . fn)
            ]
      ]

    where

    -- We use three keys 0, 1, and 2. 0 and 1 are mapped and 3 is unmapped.
    fn x = (x `mod` 3, x)

    mp = Map.fromList [(0, FL.sum), (1, FL.length)]

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [ bgroup "elimination"
      -- Left folds for building a structure are inherently non-streaming
      -- as the structure cannot be lazily consumed until fully built.
            [
              benchIOSink value "toList" (S.fold FL.toList)
            , benchIOSink value "toListRevF" (S.fold FL.toListRevF)
            ]
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    value `seq` runMode (mode cfg) cfg benches (allBenchmarks value)

    where

    allBenchmarks value =
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [ o_1_space_serial_elimination value
            , o_1_space_serial_transformation value
            , o_1_space_serial_composition value
            ]
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        ]
