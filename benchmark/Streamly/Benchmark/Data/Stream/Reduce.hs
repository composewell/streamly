-- |
-- Module      : Stream.Reduce
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Stream.Reduce (benchmarks) where

import Control.Monad.Catch (MonadCatch)
import Data.Monoid (Sum(Sum), getSum)
import Stream.Common (benchIOSink)
import Streamly.Benchmark.Common (o_1_space_prefix)
import Streamly.Internal.Data.Stream (Stream)

import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Parser.ParserD as ParserD

import Prelude hiding (length, sum, or, and, any, all, notElem, elem, (!!),
    lookup, repeat, minimum, maximum, product, last, mapM_, init)

import Gauge

import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Refold.Type as Refold
import Control.Monad.IO.Class (MonadIO)

{-# INLINE foldMany #-}
foldMany :: Monad m => Stream m Int -> m ()
foldMany =
      Stream.fold FL.drain
    . fmap getSum
    . Stream.foldMany (FL.take 2 FL.mconcat)
    . fmap Sum

{-# INLINE foldManyPost #-}
foldManyPost :: Monad m => Stream m Int -> m ()
foldManyPost =
      Stream.fold FL.drain
    . fmap getSum
    . Stream.foldManyPost (FL.take 2 FL.mconcat)
    . fmap Sum

{-# INLINE refoldMany #-}
refoldMany :: Monad m => Stream m Int -> m ()
refoldMany =
      Stream.fold FL.drain
    . fmap getSum
    . Stream.refoldMany (Refold.take 2 Refold.sconcat) (return mempty)
    . fmap Sum

{-# INLINE foldIterateM #-}
foldIterateM :: Monad m => Stream m Int -> m ()
foldIterateM =
    Stream.fold FL.drain
        . fmap getSum
        . Stream.foldIterateM
            (return . FL.take 2 . FL.sconcat) (return (Sum 0))
        . fmap Sum

{-# INLINE refoldIterateM #-}
refoldIterateM :: Monad m => Stream m Int -> m ()
refoldIterateM =
    Stream.fold FL.drain
        . fmap getSum
        . Stream.refoldIterateM
            (Refold.take 2 Refold.sconcat) (return (Sum 0))
        . fmap Sum

{-# INLINE parseMany #-}
parseMany :: MonadCatch m => Int -> Stream m Int -> m ()
parseMany n =
      Stream.fold FL.drain
    . fmap getSum
    . Stream.parseMany (PR.fromFold $ FL.take n FL.mconcat)
    . fmap Sum

{-# INLINE parseManyD #-}
parseManyD :: MonadCatch m => Int -> Stream m Int -> m ()
parseManyD n =
      Stream.fold FL.drain
    . fmap getSum
    . Stream.parseManyD (ParserD.fromFold $ FL.take n FL.mconcat)
    . fmap Sum

{-# INLINE parseIterate #-}
parseIterate :: MonadCatch m => Int -> Stream m Int -> m ()
parseIterate n =
      Stream.fold FL.drain
    . fmap getSum
    . Stream.parseIterate (\_ -> PR.fromFold $ FL.take n FL.mconcat) 0
    . fmap Sum

{-# INLINE arraysOf #-}
arraysOf :: (MonadCatch m, MonadIO m) => Int -> Stream m Int -> m ()
arraysOf n =
      Stream.fold FL.drain
    . Stream.arraysOf n

o_1_space_grouping :: Int -> [Benchmark]
o_1_space_grouping value =
    -- Buffering operations using heap proportional to group/window sizes.
    [ bgroup "reduce"
        [ benchIOSink value "foldMany" foldMany
        , benchIOSink value "foldManyPost" foldManyPost
        , benchIOSink value "refoldMany" refoldMany
        , benchIOSink value "foldIterateM" foldIterateM
        , benchIOSink value "refoldIterateM" refoldIterateM
        , benchIOSink value "parseMany" $ parseMany value
        , benchIOSink value "parseManyD" $ parseManyD value
        , benchIOSink value "parseIterate" $ parseIterate value
        , benchIOSink value "arraysOf" $ arraysOf value
        ]
    ]

benchmarks :: String -> Int -> [Benchmark]
benchmarks moduleName size =
    [ bgroup  (o_1_space_prefix moduleName) $  o_1_space_grouping size
    ]
