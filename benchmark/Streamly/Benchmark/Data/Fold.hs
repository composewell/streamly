-- |
-- Module      : Streamly.Benchmark.Data.Fold
-- Copyright   : (c) 2018 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.DeepSeq (NFData(..))
import Data.Monoid (Last(..), Sum(..))
import System.Random (randomRIO)
import Prelude (IO, Int, Double, String, (>), (<$>), (+), ($), even,
                (<=), Monad(..), (==), Maybe(..), (.), fromIntegral,
                compare, (>=), concat, seq, mod, fst, snd, const, Bool, Ord(..),
                div, Num(..), odd, Either(..))

import Data.Map.Strict (Map)

import Streamly.Prelude (SerialT)
import Streamly.Internal.Data.Fold (Fold(..))

import qualified Data.Map.Strict as Map

import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold.Types as FL
import qualified Streamly.Internal.Data.Pipe as Pipe

import qualified Streamly.Internal.Data.Sink as Sink

import qualified Streamly.Data.Array.Storable.Foreign as A
import qualified Streamly.Internal.Data.Array.Storable.Foreign as IA
import qualified Streamly.Internal.Data.Stream.IsStream as IP

import Gauge
import Streamly.Benchmark.Common

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
take value = IP.fold (FL.ltake value FL.drain)

{-# INLINE takeSepBy #-}
takeSepBy :: Monad m => Int -> SerialT m Int -> m ()
takeSepBy value = IP.fold (FL.takeSepBy (<= value) FL.drain)

{-# INLINE many #-}
many :: Monad m => SerialT m Int -> m Int
many = IP.fold (FL.many FL.length FL.sum)

{-# INLINE splitAllAny #-}
splitAllAny :: Monad m => Int -> SerialT m Int -> m (Bool, Bool)
splitAllAny value =
    IP.fold (FL.splitWith (,) (FL.all (<= (value `div` 2))) (FL.any (> value)))

{-# INLINE teeAllAny #-}
teeAllAny :: (Monad m, Ord a) => a -> SerialT m a -> m (Bool, Bool)
teeAllAny value =
    IP.fold (FL.teeWith (,) (FL.all (<= value)) (FL.any (> value)))

{-# INLINE teeSumLength #-}
teeSumLength :: Monad m => SerialT m Int -> m (Int, Int)
teeSumLength = IP.fold (FL.teeWith (,) FL.sum FL.length)

{-
{-# INLINE applicativeAllAny #-}
applicativeAllAny :: (Monad m, Ord a) => a -> SerialT m a -> m (Bool, Bool)
applicativeAllAny value =
    IP.fold ((,) <$> FL.all (<= value) <*> (FL.any (> value)))

{-# INLINE applicativeSumLength #-}
applicativeSumLength :: Monad m => SerialT m Int -> m (Int, Int)
applicativeSumLength = IP.fold ((,) <$> FL.sum <*> FL.length)
-}

{-# INLINE distribute_ #-}
distribute_ :: Monad m => SerialT m Int -> m ()
distribute_ =
    IP.fold (FL.distribute_ [const () <$> FL.sum, const () <$> FL.length])

{-# INLINE distribute #-}
distribute :: Monad m => SerialT m Int -> m [Int]
distribute = IP.fold (FL.distribute [FL.sum, FL.length])

{-# INLINE partition #-}
partition :: Monad m => SerialT m Int -> m (Int, Int)
partition =
    let f a =
            if odd a
            then Left a
            else Right a
     in IP.fold $ FL.lmap f (FL.partition FL.sum FL.length)

{-# INLINE unzip #-}
unzip :: Monad m => SerialT m Int -> m (Int, Int)
unzip = IP.fold $ FL.lmap (\a -> (a, a)) (FL.unzip FL.sum FL.length)

{-# INLINE demuxWith  #-}
demuxWith ::
       (Monad m, Ord k)
    => (a -> (k, a'))
    -> Map k (Fold m a' b)
    -> SerialT m a
    -> m (Map k b)
demuxWith f mp = S.fold (FL.demuxWith f mp)

{-# INLINE demuxWith_ #-}
demuxWith_ ::
       (Monad m, Ord k)
    => (a -> (k, a'))
    -> Map k (Fold m a' b)
    -> SerialT m a
    -> m ()
demuxWith_ f mp = S.fold (FL.demuxWith_ f mp)

{-# INLINE demuxWithDefault_ #-}
demuxWithDefault_ ::
       (Monad m, Ord k, Num b)
    => (a -> (k, b))
    -> Map k (Fold m b b)
    -> SerialT m a
    -> m ()
demuxWithDefault_ f mp = S.fold (FL.demuxWithDefault_ f mp (FL.lmap snd FL.sum))

{-# INLINE classifyWith #-}
classifyWith ::
       (Monad m, Ord k, Num a) => (a -> k) -> SerialT m a -> m (Map k a)
classifyWith f = S.fold (FL.classifyWith f FL.sum)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Fold"

o_1_space_serial_elimination :: Int -> [Benchmark]
o_1_space_serial_elimination value =
    [ bgroup "serially"
        [ bgroup "elimination"
            [ benchIOSink value "drain" (S.fold FL.drain)
            , benchIOSink value "drainBy" (S.fold (FL.drainBy return))
            , benchIOSink value "drainN" (S.fold (FL.drainN value))
            , benchIOSink
                  value
                  "drainSepByTrue"
                  (S.fold (FL.drainSepBy $ (<=) (value + 1)))
            , benchIOSink
                  value
                  "drainSepByFalse"
                  (S.fold (FL.drainSepBy $ (>=) (value + 1)))
            , benchIOSink value "sink" (S.fold $ Sink.toFold Sink.drain)
            , benchIOSink value "last" (S.fold FL.last)
            , benchIOSink value "lastN.1" (S.fold (IA.lastN 1))
            , benchIOSink value "lastN.10" (S.fold (IA.lastN 10))
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
                  (\s ->
                       S.fold
                           FL.mean
                           (S.map (fromIntegral :: Int -> Double) s))
            , benchIOSink
                  value
                  "variance"
                  (\s ->
                       S.fold
                           FL.variance
                           (S.map (fromIntegral :: Int -> Double) s))
            , benchIOSink
                  value
                  "stdDev"
                  (\s ->
                       S.fold
                           FL.stdDev
                           (S.map (fromIntegral :: Int -> Double) s))
            , benchIOSink
                  value
                  "mconcat"
                  (S.fold FL.mconcat . (S.map (Last . Just)))
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
            , benchIOSink value "takeSepBy" $ takeSepBy value
            , benchIOSink
                  value
                  "and"
                  (\s -> S.fold FL.and (S.map (<= (value + 1)) s))
            , benchIOSink
                  value
                  "or"
                  (\s -> S.fold FL.or (S.map (> (value + 1)) s))
            ]
        ]
    ]


o_1_space_serial_transformation :: Int -> [Benchmark]
o_1_space_serial_transformation value =
    [ bgroup "serially"
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
    ]

o_1_space_serial_composition :: Int -> [Benchmark]
o_1_space_serial_composition value =
    [ bgroup
          "serially"
          [ bgroup
                "composition"
                -- Applicative
                [ benchIOSink value "tee (all, any)" $ teeAllAny value
                , benchIOSink value "tee (sum, length)" $ teeSumLength
                -- , benchIOSink value "applicative (all, any)"
                --       $ applicativeAllAny value
                -- , benchIOSink value "applicative (sum, length)"
                --       $ applicativeSumLength
                , benchIOSink value "distribute_ [sum, length]" $ distribute_
                , benchIOSink value "distribute [sum, length]" $ distribute
                , benchIOSink value "partition (sum, length)" $ partition
                , benchIOSink value "unzip (sum, length)" $ unzip
                , benchIOSink value "demuxWith [sum, length]" $ demuxWith fn mp
                , benchIOSink value "demuxWith_ [sum, length]"
                      $ demuxWith_ fn mp
                , benchIOSink value "demuxWithDefault_ [sum, length] sum"
                      $ demuxWithDefault_ fn mp
                , benchIOSink value "classifyWith sum" $ classifyWith (fst . fn)
                , benchIOSink value "many length sum" many
                , benchIOSink value "split (all, any)" $ splitAllAny value
                ]
          ]
    ]

    where

    fn x = (x `mod` 3, x)

    mp = Map.fromList [(0, FL.sum), (1, FL.length)]

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [ bgroup "serially"
          [ bgroup "elimination"
          -- Left folds for building a structure are inherently non-streaming
          -- as the structure cannot be lazily consumed until fully built.
                [ benchIOSink value "toStream" (S.fold IP.toStream)
                , benchIOSink value "toStreamRev" (S.fold IP.toStreamRev)
                , benchIOSink value "toList" (S.fold FL.toList)
                , benchIOSink value "toListRevF" (S.fold FL.toListRevF)
          -- Converting the stream to an array
                , benchIOSink value "lastN.Max" (S.fold (IA.lastN (value + 1)))
                , benchIOSink value "writeN" (S.fold (A.writeN value))
                ]
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
        , bgroup (o_n_heap_prefix moduleName) $ concat
            [ o_n_heap_serial value ]
        ]
