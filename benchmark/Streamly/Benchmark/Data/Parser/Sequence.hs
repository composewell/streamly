#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

-- Compiling parseMany with higher values of spec-constr-recursive hogs a lot
-- of memory and takes too much time. Fusion plugin alleviates the problem
-- though.
{-# OPTIONS_GHC -fspec-constr-recursive=5 #-}

-- |
-- Module      : Streamly.Benchmark.Data.Parser.Sequence
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Streamly.Benchmark.Data.Parser.Sequence
  (
    benchmarks
  , benchmarksFileIO
  ) where

import Control.DeepSeq (NFData(..))
import Data.Function ((&))
import Data.Monoid (Sum(..))
import GHC.Magic (inline)
import GHC.Magic (noinline)
import System.IO (Handle)
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Stream (Stream)

import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Data.Stream as Stream

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle
import Streamly.Benchmark.Data.Parser.Common

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

{-# INLINE parseMany #-}
parseMany :: Monad m => Int -> Stream m Int -> m ()
parseMany n =
      Stream.fold Fold.drain
    . fmap getSum
    . Stream.catRights . Stream.parseMany (PR.fromFold $ Fold.take n Fold.mconcat)
    . fmap Sum

{-# INLINE parseManyGroupBy #-}
parseManyGroupBy :: Monad m => (Int -> Int -> Bool) -> Stream m Int -> m ()
parseManyGroupBy cmp =
    Stream.fold Fold.drain . Stream.parseMany (PR.groupBy cmp Fold.drain)

{-# INLINE parseManyGroupsRolling #-}
parseManyGroupsRolling :: Monad m => Bool -> Stream m Int -> m ()
parseManyGroupsRolling b =
      Stream.fold Fold.drain
    . Stream.parseMany (PR.groupByRolling (\_ _ -> b) Fold.drain)

{-# INLINE parseManyGroupsRollingEither #-}
parseManyGroupsRollingEither :: Monad m =>
    (Int -> Int -> Bool) -> Int -> m ()
parseManyGroupsRollingEither cmp value = do
    streamUnfoldrM value 1
        & Stream.parseMany (PR.groupByRollingEither cmp Fold.drain Fold.drain)
        & Stream.fold Fold.drain

{-# INLINE parseManyGroupsRollingEitherAlt #-}
parseManyGroupsRollingEitherAlt :: Monad m =>
    (Int -> Int -> Bool) -> Int -> m ()
parseManyGroupsRollingEitherAlt cmp value = do
    streamUnfoldrM value 1
        -- Make the input unsorted.
        & fmap (\x -> if even x then x + 2 else x)
        & Stream.parseMany (PR.groupByRollingEither cmp Fold.drain Fold.drain)
        & Stream.fold Fold.drain

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

instance NFData ParseError where
    {-# INLINE rnf #-}
    rnf (ParseError x) = rnf x

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks value =
    [
    -- parseMany
      (SpaceO_1, benchIOSink value "parseMany" $ parseMany value)
    , (SpaceO_1, benchIOSink value "parseMany (take 1)" (parseMany 1))
    , (SpaceO_1, benchIOSink value "parseMany (take all)" (parseMany value))
    , (SpaceO_1, benchIOSink value "parseMany (groupBy (<))" (parseManyGroupBy (<)))
    , (SpaceO_1, benchIOSink value "parseMany (groupBy (==))" (parseManyGroupBy (==)))
    , (SpaceO_1, benchIOSink value "parseMany groupRollingBy (bound groups)"
          $ parseManyGroupsRolling False)
    , (SpaceO_1, benchIOSink value "parseMany groupRollingBy (1 group)"
          $ parseManyGroupsRolling True)
    , (SpaceO_1, bench "parseMany groupRollingByEither (Left)"
        $ nfIO $ parseManyGroupsRollingEitherLeft)
    , (SpaceO_1, bench "parseMany groupRollingByEither (Right)"
        $ nfIO $ parseManyGroupsRollingEitherRight)
    , (SpaceO_1, bench "parseMany groupRollingByEither (Alternating)"
        $ nfIO $ parseManyGroupsRollingEitherAlt1)
    ]

    where

    {-# NOINLINE parseManyGroupsRollingEitherLeft #-}
    parseManyGroupsRollingEitherLeft = parseManyGroupsRollingEither (<) value

    {-# NOINLINE parseManyGroupsRollingEitherRight #-}
    parseManyGroupsRollingEitherRight = parseManyGroupsRollingEither (>) value

    {-# NOINLINE parseManyGroupsRollingEitherAlt1 #-}
    parseManyGroupsRollingEitherAlt1 =
        parseManyGroupsRollingEitherAlt (>) value

-------------------------------------------------------------------------------
-- parseMany with FileIO
-------------------------------------------------------------------------------

parseManyChunksOfSum :: Int -> Handle -> IO Int
parseManyChunksOfSum n inh =
    Stream.fold Fold.length
        $ Stream.parseMany
              (PR.fromFold $ Fold.take n Fold.sum)
              (Stream.unfold Handle.reader inh)

benchmarksFileIO :: BenchEnv -> [(SpaceComplexity, Benchmark)]
benchmarksFileIO env =
    [
    -- parseMany with file IO
      (SpaceO_1, mkBench ("parseMany (Fold.take " ++ show (bigSize env) ++ " Fold.sum)") env
          $ \inh _ -> noinline parseManyChunksOfSum (bigSize env) inh)
    , (SpaceO_1, mkBench "parseMany (Fold.take 1 Fold.sum)" env
          $ \inh _ -> inline parseManyChunksOfSum 1 inh)
    ]
