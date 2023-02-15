-- |
-- Module      : Streamly.Benchmark.Data.ParserK
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  (
    main
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (asum)
import Streamly.Data.Array (Array, Unbox)
import Streamly.Data.Stream.StreamK (StreamK)
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Stream.StreamD (Stream)
import System.Random (randomRIO)
import Prelude hiding
    (any, all, take, sequence, sequence_, sequenceA, takeWhile)

import qualified Control.Applicative as AP
import qualified Data.Foldable as F
import qualified Data.Traversable as TR
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Parser as PRD
import qualified Streamly.Data.Parser.ParserK as PR
import qualified Streamly.Internal.Data.Stream.StreamK as StreamK

import Gauge
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- XXX these can be moved to the common module

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> Stream m Int
sourceUnfoldrM value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: NFData b
    => Int -> String -> (StreamK IO (Array Int) -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1)
        >>= f
            . StreamK.fromStream
            . Stream.arraysOf 4000
            . sourceUnfoldrM value

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

#define PARSE_OP StreamK.parseKChunks

{-# INLINE one #-}
one :: MonadIO m =>
    Int -> StreamK m (Array Int) -> m (Either ParseError (Maybe Int))
one value = StreamK.parseKChunks p

    where

    p = do
        m <- PR.fromFold FL.one
        case m of
          Just i -> if i >= value then pure m else p
          Nothing -> pure Nothing

{-# INLINE satisfy #-}
satisfy :: (MonadIO m, Unbox a) => (a -> Bool) -> PR.Parser a m a
satisfy = PR.fromParser . PRD.satisfy

{-# INLINE takeWhile #-}
takeWhile :: (MonadIO m, Unbox a) => (a -> Bool) -> PR.Parser a m ()
takeWhile p = PR.fromParser $ PRD.takeWhile p FL.drain

{-# INLINE takeWhileK #-}
takeWhileK :: MonadIO m =>
    Int -> StreamK m (Array Int) -> m (Either ParseError ())
takeWhileK value = PARSE_OP (takeWhile (<= value))

{-# INLINE splitApp #-}
splitApp :: MonadIO m
    => Int -> StreamK m (Array Int) -> m (Either ParseError ((), ()))
splitApp value =
    PARSE_OP ((,) <$> takeWhile (<= (value `div` 2)) <*> takeWhile (<= value))

{-# INLINE sequenceA #-}
sequenceA :: MonadIO m => Int -> StreamK m (Array Int) -> m Int
sequenceA value xs = do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    x <- PARSE_OP (TR.sequenceA list) xs
    return $ Prelude.length x

{-# INLINE sequenceA_ #-}
sequenceA_ :: MonadIO m =>
    Int -> StreamK m (Array Int) -> m (Either ParseError ())
sequenceA_ value xs = do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    PARSE_OP (F.sequenceA_ list) xs

{-# INLINE sequence #-}
sequence :: MonadIO m => Int -> StreamK m (Array Int) -> m Int
sequence value xs = do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    x <- PARSE_OP (TR.sequence list) xs
    return $ Prelude.length x

{-# INLINE sequence_ #-}
sequence_ :: MonadIO m =>
    Int -> StreamK m (Array Int) -> m (Either ParseError ())
sequence_ value =
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
     in PARSE_OP (F.sequence_ list)

{-# INLINE manyAlt #-}
manyAlt :: MonadIO m => StreamK m (Array Int) -> m Int
manyAlt xs = do
    x <- PARSE_OP (AP.many (satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE someAlt #-}
someAlt :: MonadIO m => StreamK m (Array Int) -> m Int
someAlt xs = do
    x <- PARSE_OP (AP.some (satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE choice #-}
choice :: MonadIO m =>
    Int -> StreamK m (Array Int) -> m (Either ParseError Int)
choice value =
    PARSE_OP (asum (replicate value (satisfy (< 0)))
        AP.<|> satisfy (> 0))

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Parser.ParserK"

instance NFData ParseError where
    {-# INLINE rnf #-}
    rnf (ParseError x) = rnf x

o_1_space_serial :: Int -> [Benchmark]
o_1_space_serial value =
    [ benchIOSink value "takeWhile" $ takeWhileK value
    , benchIOSink value "splitApp" $ splitApp value
    ]

{-# INLINE sepBy1 #-}
sepBy1 :: MonadIO m => StreamK m (Array Int) -> m Int
sepBy1 xs = do
    x <- PARSE_OP (parser (satisfy odd) (satisfy even)) xs
    return $ Prelude.length x

    where

    parser p sep = do
        x <- p
        fmap (x :) $ AP.many (sep >> p)

-- O(n) heap beacuse of accumulation of the list in strict IO monad?
o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [
    -- accumulates the results in a list
    -- XXX why should this take O(n) heap, it discards the results?
      benchIOSink value "sequence_" $ sequence_ value
    , benchIOSink value "sequenceA_" $ sequenceA_ value
    , benchIOSink value "sequence" $ sequence value
    , benchIOSink value "sequenceA" $ sequenceA value
    , benchIOSink value "manyAlt" manyAlt
    , benchIOSink value "sepBy1" sepBy1
    , benchIOSink value "someAlt" someAlt
    , benchIOSink value "choice" $ choice value
    ]

-- O(n) heap beacuse of accumulation of the list in strict IO monad?
o_1_space_recursive :: Int -> [Benchmark]
o_1_space_recursive value =
    [ benchIOSink value "one (recursive)" $ one value
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks value =
        [ bgroup (o_1_space_prefix moduleName) (o_1_space_serial value)
        , bgroup (o_1_space_prefix moduleName) (o_1_space_recursive value)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        ]
