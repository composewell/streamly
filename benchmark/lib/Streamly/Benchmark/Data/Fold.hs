-- |
-- Module      : Streamly.Benchmark.Data.Fold
-- Copyright   : (c) 2018 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}

module Streamly.Benchmark.Data.Fold
  ( o_1_space_serial_folds
  , o_1_space_serial_foldsTransforms
  , o_1_space_serial_foldsCompositions
  , o_n_heap_serial_folds
  ) where

import Control.DeepSeq (NFData(..))
import Data.Monoid (Last(..))
import Data.Functor.Identity (Identity)

import System.Random (randomRIO)
import Prelude (IO, Int, Double, String, (>), (<*>), (<$>), (+), ($),
                (<=), Monad(..), (==), Maybe(..), (.), fromIntegral,
                compare, (>=), Bool)

import qualified Streamly as S hiding (runStream)
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Pipe as Pipe

import qualified Streamly.Internal.Data.Sink as Sink

import qualified Streamly.Memory.Array as A
import qualified Streamly.Internal.Memory.Array as IA
import qualified Streamly.Internal.Data.Fold as IFL
import qualified Streamly.Internal.Prelude as IP

import Gauge
import Streamly hiding (runStream)

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
    :: (IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f = bench name $ nfIO $ randomRIO (1,1) >>= f . source value

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

drain :: SerialT IO a -> IO ()
drain = S.fold FL.drain

drainN :: Monad m => Int -> SerialT m a -> m ()
drainN value = S.fold (IFL.drainN value)

drainWhileTrue :: Monad m => Int -> SerialT m Int -> m ()
drainWhileTrue value = S.fold (IFL.drainWhile $ (<=) (value + 1))

drainWhileFalse :: Monad m => Int -> SerialT m Int -> m ()
drainWhileFalse value = S.fold (IFL.drainWhile $ (>=) (value + 1))

sink :: SerialT IO a -> IO ()
sink = S.fold $ Sink.toFold Sink.drain

last :: SerialT IO a -> IO (Maybe a)
last = S.fold FL.last

lastN1 :: SerialT IO Int -> IO (IA.Array Int)
lastN1 = S.fold (IA.lastN 1)

lastN10 :: SerialT IO Int -> IO (IA.Array Int)
lastN10 = S.fold (IA.lastN 10)

length :: SerialT IO a -> IO Int
length = S.fold FL.length

sum :: SerialT IO Int -> IO Int
sum = S.fold FL.sum

product :: SerialT IO Int -> IO Int
product = S.fold FL.product

maximumBy :: SerialT IO Int -> IO (Maybe Int)
maximumBy = S.fold (FL.maximumBy compare)

maximum :: SerialT IO Int -> IO (Maybe Int)
maximum = S.fold FL.maximum

minimumBy :: SerialT IO Int -> IO (Maybe Int)
minimumBy = S.fold (FL.minimumBy compare)

minimum :: SerialT IO Int -> IO (Maybe Int)
minimum = S.fold FL.minimum

mean :: SerialT IO Int -> IO Double
mean = \s -> S.fold FL.mean (S.map (fromIntegral :: Int -> Double) s)

variance :: SerialT IO Int -> IO Double
variance = \s -> S.fold FL.variance (S.map (fromIntegral :: Int -> Double) s)

stdDev :: SerialT IO Int -> IO Double
stdDev = \s -> S.fold FL.stdDev (S.map (fromIntegral :: Int -> Double) s)

mconcat :: SerialT IO a -> IO (Last a)
mconcat = S.fold FL.mconcat . (S.map (Last . Just))

foldMap :: SerialT IO a -> IO (Last a)
foldMap = S.fold (FL.foldMap (Last . Just))

index :: Monad m => Int -> SerialT m a -> m (Maybe a)
index value = S.fold (FL.index (value + 1))

head :: SerialT IO a -> IO (Maybe a)
head = S.fold FL.head

find :: Monad m => Int -> SerialT m Int -> m (Maybe Int)
find value = S.fold (FL.find (== (value + 1)))

findIndex :: Monad m => Int -> SerialT m Int -> m (Maybe Int)
findIndex value = S.fold (FL.findIndex (== (value + 1)))

elemIndex :: Monad m => Int -> SerialT m Int -> m (Maybe Int)
elemIndex value = S.fold (FL.elemIndex (value + 1))

null :: SerialT IO a -> IO Bool
null = S.fold FL.null

elem :: Monad m => Int -> SerialT m Int -> m Bool
elem value = S.fold (FL.elem (value + 1))

notElem :: Monad m => Int -> SerialT m Int -> m Bool
notElem value = S.fold (FL.notElem (value + 1))

all :: Monad m => Int -> SerialT m Int -> m Bool
all value = S.fold (FL.all (<= (value + 1)))

any :: Monad m => Int -> SerialT m Int -> m Bool
any value = S.fold (FL.any (> (value + 1)))

and :: Monad m => Int -> SerialT m Int -> m Bool
and value = \s -> S.fold FL.and (S.map (<= (value + 1)) s)

or :: Monad m => Int -> SerialT m Int -> m Bool
or value = \s -> S.fold FL.or (S.map (> (value + 1)) s)

lmap :: SerialT IO Int -> IO ()
lmap = S.fold (IFL.lmap (+ 1) FL.drain)

pipeMapM :: SerialT IO Int -> IO ()
pipeMapM = S.fold (IFL.transform (Pipe.mapM (\x -> return $ x + 1)) FL.drain)

allAppAny :: Monad m => Int -> SerialT m Int -> m (Bool, Bool)
allAppAny value =
  S.fold ((,) <$> FL.all (<= (value + 1)) <*> FL.any (> (value + 1)))

sumAppLength :: SerialT IO Int -> IO (Int, Int)
sumAppLength = S.fold ((,) <$> FL.sum <*> FL.length)

toStream :: SerialT IO a -> IO (SerialT Identity a)
toStream = S.fold IP.toStream

toStreamRev :: SerialT IO a -> IO (SerialT Identity a)
toStreamRev = S.fold IP.toStreamRev

toList :: SerialT IO a -> IO [a]
toList = S.fold FL.toList

toListRevF :: SerialT IO a -> IO [a]
toListRevF = S.fold IFL.toListRevF

lastNMax :: Int -> SerialT IO Int -> IO (IA.Array Int)
lastNMax value = S.fold (IA.lastN (value + 1))

writeN :: Int -> SerialT IO Int -> IO (IA.Array Int)
writeN value = S.fold (A.writeN value)

-------------------------------------------------------------------------------
-- Benchamrks
-------------------------------------------------------------------------------

o_1_space_serial_folds :: Int -> [Benchmark]
o_1_space_serial_folds value =
  [ bgroup
      "serially"
      [ bgroup
          "folds"
          [ benchIOSink value "drain" drain
          , benchIOSink value "drainN" (drainN value)
          , benchIOSink value "drainWhileTrue" (drainWhileTrue value)
          , benchIOSink value "drainWhileFalse" (drainWhileFalse value)
          , benchIOSink value "sink" sink
          , benchIOSink value "last" last
          , benchIOSink value "lastN.1" lastN1
          , benchIOSink value "lastN.10" lastN10
          , benchIOSink value "length" length
          , benchIOSink value "sum" sum
          , benchIOSink value "product" product
          , benchIOSink value "maximumBy" maximumBy
          , benchIOSink value "maximum" maximum
          , benchIOSink value "minimumBy" minimumBy
          , benchIOSink value "minimum" minimum
          , benchIOSink value "mean" mean
          , benchIOSink value "variance" variance
          , benchIOSink value "stdDev" stdDev
          , benchIOSink value "mconcat" mconcat
          , benchIOSink value "foldMap" foldMap
          , benchIOSink value "index" (index value)
          , benchIOSink value "head" head
          , benchIOSink value "find" (find value)
          , benchIOSink value "findIndex" (findIndex value)
          , benchIOSink value "elemIndex" (elemIndex value)
          , benchIOSink value "null" null
          , benchIOSink value "elem" (elem value)
          , benchIOSink value "notElem" (notElem value)
          , benchIOSink value "all" (all value)
          , benchIOSink value "any" (any value)
          , benchIOSink value "and" (and value)
          , benchIOSink value "or" (or value)
          ]
      ]
  ]


o_1_space_serial_foldsTransforms :: Int -> [Benchmark]
o_1_space_serial_foldsTransforms value =
  [ bgroup
      "serially"
      [ bgroup
          "folds-transforms"
          [ benchIOSink value "drain" drain
          , benchIOSink value "lmap" lmap
          , benchIOSink value "pipe-mapM" pipeMapM
          ]
      ]
  ]


o_1_space_serial_foldsCompositions :: Int -> [Benchmark]
o_1_space_serial_foldsCompositions value =
  [ bgroup
      "serially"
      [ bgroup
          "folds-compositions" -- Applicative
          [ benchIOSink value "all,any" (allAppAny value)
          , benchIOSink value "sum,length" sumAppLength
          ]
      ]
  ]


o_n_heap_serial_folds :: Int -> [Benchmark]
o_n_heap_serial_folds value =
    [ bgroup
          "serially"
          [ bgroup
                "foldl"
          -- Left folds for building a structure are inherently non-streaming
          -- as the structure cannot be lazily consumed until fully built.
                [ benchIOSink value "toStream" toStream
                , benchIOSink value "toStreamRev" toStreamRev
                , benchIOSink value "toList" toList
                , benchIOSink value "toListRevF" toListRevF
          -- Converting the stream to an array
                , benchIOSink value "lastN.Max" (lastNMax value)
                , benchIOSink value "writeN" (writeN value)
                ]
          ]
    ]
