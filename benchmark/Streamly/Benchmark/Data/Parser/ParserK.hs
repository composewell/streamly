-- |
-- Module      : Streamly.Benchmark.Data.ParserK
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Main
  (
    main
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.Catch (MonadThrow)
import Data.Foldable (asum)
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)
import Prelude hiding (any, all, take, sequence, sequenceA, takeWhile)

import qualified Control.Applicative as AP
import qualified Data.Foldable as F
import qualified Data.Traversable as TR
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser.ParserK.Type as PR
import qualified Streamly.Internal.Data.Parser.ParserD as PRD
import qualified Streamly.Internal.Data.Stream as Stream

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
    => Int -> String -> (Stream IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrM value

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

#ifdef FROM_PARSERK
#define PARSE_OP (Stream.parseD . PRD.fromParserK)
#else
#define PARSE_OP Stream.parseK
#endif

{-# INLINE satisfy #-}
satisfy :: Monad m => (a -> Bool) -> PR.Parser m a a
satisfy = PRD.toParserK . PRD.satisfy

{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> PR.Parser m a ()
takeWhile p = PRD.toParserK $ PRD.takeWhile p FL.drain

{-# INLINE takeWhileK #-}
takeWhileK :: MonadThrow m => Int -> Stream m Int -> m ()
takeWhileK value = PARSE_OP (takeWhile (<= value))

{-# INLINE splitApp #-}
splitApp :: MonadThrow m
    => Int -> Stream m Int -> m ((), ())
splitApp value =
    PARSE_OP ((,) <$> takeWhile (<= (value `div` 2)) <*> takeWhile (<= value))

{-# INLINE sequenceA #-}
sequenceA :: MonadThrow m => Int -> Stream m Int -> m Int
sequenceA value xs = do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    x <- PARSE_OP (TR.sequenceA list) xs
    return $ Prelude.length x

{-# INLINE sequenceA_ #-}
sequenceA_ :: MonadThrow m => Int -> Stream m Int -> m ()
sequenceA_ value xs = do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    PARSE_OP (F.sequenceA_ list) xs

{-# INLINE sequence #-}
sequence :: MonadThrow m => Int -> Stream m Int -> m Int
sequence value xs = do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    x <- PARSE_OP (TR.sequence list) xs
    return $ Prelude.length x

{-# INLINE manyAlt #-}
manyAlt :: MonadThrow m => Stream m Int -> m Int
manyAlt xs = do
    x <- PARSE_OP (AP.many (satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE someAlt #-}
someAlt :: MonadThrow m => Stream m Int -> m Int
someAlt xs = do
    x <- PARSE_OP (AP.some (satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE choice #-}
choice :: MonadThrow m => Int -> Stream m Int -> m Int
choice value =
    PARSE_OP (asum (replicate value (satisfy (< 0)))
        AP.<|> satisfy (> 0))

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Parser.ParserK"

o_1_space_serial :: Int -> [Benchmark]
o_1_space_serial value =
    [ benchIOSink value "takeWhile" $ takeWhileK value
    , benchIOSink value "splitApp" $ splitApp value
    ]

-- O(n) heap beacuse of accumulation of the list in strict IO monad?
o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [ benchIOSink value "sequenceA" $ sequenceA value
    , benchIOSink value "sequenceA_" $ sequenceA_ value
    , benchIOSink value "sequence" $ sequence value
    , benchIOSink value "manyAlt" manyAlt
    , benchIOSink value "someAlt" someAlt
    , benchIOSink value "choice" $ choice value
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks value =
        [ bgroup (o_1_space_prefix moduleName) (o_1_space_serial value)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        ]
