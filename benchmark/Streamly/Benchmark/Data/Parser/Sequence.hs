#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

-- Compiling parseMany with higher values of spec-constr-recursive hogs a lot
-- of memory and takes too much time. Fusion plugin alleviates the problem
-- though.
{-# OPTIONS_GHC -fspec-constr-recursive=10 #-}

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
import Data.Monoid (Sum(..))
import GHC.Magic (inline)
import GHC.Magic (noinline)
import System.IO (Handle)
import System.Random (randomRIO)
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Stream (Stream)

import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Data.Stream as Stream

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream as S
#endif

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> IO b -> Benchmark
benchIO name = bench name . nfIO

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> IO b
withStream value f = randomRIO (1,1) >>= f . streamUnfoldrM value

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

{-# INLINE parseMany #-}
parseMany :: Int -> Int -> IO ()
parseMany n value =
    withStream value $
          Stream.fold Fold.drain
        . fmap getSum
        . Stream.catRights . Stream.parseMany (PR.fromFold $ Fold.take n Fold.mconcat)
        . fmap Sum

#ifdef INSPECTION
inspect $ 'parseMany `hasNoType` ''S.Step
inspect $ 'parseMany `hasNoType` ''PR.Step
inspect $ 'parseMany `hasNoType` ''PR.Initial
inspect $ 'parseMany `hasNoType` ''FL.Step
inspect $ 'parseMany `hasNoType` ''SPEC
inspect $ 'parseMany `hasNoType` ''S.FIterState
#endif

{-# INLINE parseManyGroupBy #-}
parseManyGroupBy :: (Int -> Int -> Bool) -> Int -> IO ()
parseManyGroupBy cmp value =
    withStream value $
        Stream.fold Fold.drain . Stream.parseMany (PR.groupBy cmp Fold.drain)

#ifdef INSPECTION
inspect $ 'parseManyGroupBy `hasNoType` ''S.Step
inspect $ 'parseManyGroupBy `hasNoType` ''PR.Step
inspect $ 'parseManyGroupBy `hasNoType` ''PR.Initial
inspect $ 'parseManyGroupBy `hasNoType` ''FL.Step
inspect $ 'parseManyGroupBy `hasNoType` ''SPEC
inspect $ 'parseManyGroupBy `hasNoType` ''S.FIterState
inspect $ 'parseManyGroupBy `hasNoType` ''PR.GroupByState
#endif

{-# INLINE parseManyGroupsRolling #-}
parseManyGroupsRolling :: Bool -> Int -> IO ()
parseManyGroupsRolling b value =
    withStream value $
          Stream.fold Fold.drain
        . Stream.parseMany (PR.groupByRolling (\_ _ -> b) Fold.drain)

#ifdef INSPECTION
inspect $ 'parseManyGroupsRolling `hasNoType` ''S.Step
inspect $ 'parseManyGroupsRolling `hasNoType` ''PR.Step
inspect $ 'parseManyGroupsRolling `hasNoType` ''PR.Initial
inspect $ 'parseManyGroupsRolling `hasNoType` ''FL.Step
inspect $ 'parseManyGroupsRolling `hasNoType` ''SPEC
inspect $ 'parseManyGroupsRolling `hasNoType` ''S.FIterState
inspect $ 'parseManyGroupsRolling `hasNoType` ''PR.GroupByState
#endif

{-# INLINE parseManyGroupsRollingEither #-}
parseManyGroupsRollingEither :: (Int -> Int -> Bool) -> Int -> IO ()
parseManyGroupsRollingEither cmp value =
    withStream value $
          Stream.fold Fold.drain
        . Stream.parseMany (PR.groupByRollingEither cmp Fold.drain Fold.drain)

#ifdef INSPECTION
inspect $ 'parseManyGroupsRollingEither `hasNoType` ''S.Step
inspect $ 'parseManyGroupsRollingEither `hasNoType` ''PR.Step
inspect $ 'parseManyGroupsRollingEither `hasNoType` ''PR.Initial
inspect $ 'parseManyGroupsRollingEither `hasNoType` ''FL.Step
inspect $ 'parseManyGroupsRollingEither `hasNoType` ''SPEC
inspect $ 'parseManyGroupsRollingEither `hasNoType` ''S.FIterState
inspect $ 'parseManyGroupsRollingEither `hasNoType` ''PR.GroupByStatePair
#endif

{-# INLINE parseManyGroupsRollingEitherAlt #-}
parseManyGroupsRollingEitherAlt :: (Int -> Int -> Bool) -> Int -> IO ()
parseManyGroupsRollingEitherAlt cmp value =
    withStream value $
          Stream.fold Fold.drain
        . Stream.parseMany (PR.groupByRollingEither cmp Fold.drain Fold.drain)
        -- Make the input unsorted.
        . fmap (\x -> if even x then x + 2 else x)

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
      (SpaceO_1, benchIO "parseMany" $ parseMany value value)
    , (SpaceO_1, benchIO "parseMany (take 1)" $ parseMany 1 value)
    , (SpaceO_1, benchIO "parseMany (take all)" $ parseMany value value)
    , (SpaceO_1, benchIO "parseMany (groupBy (<))" $ parseManyGroupBy (<) value)
    -- requires -fspec-constr-recursive=10
    , (SpaceO_1, benchIO "parseMany (groupBy (==))" $ parseManyGroupBy (==) value)
    -- requires -fspec-constr-recursive=10
    , (SpaceO_1, benchIO "parseMany groupRollingBy (bound groups)"
          $ parseManyGroupsRolling False value)
    , (SpaceO_1, benchIO "parseMany groupRollingBy (1 group)"
          $ parseManyGroupsRolling True value)
    , (SpaceO_1, benchIO "parseMany groupRollingByEither (Left)"
        parseManyGroupsRollingEitherLeft)
    , (SpaceO_1, benchIO "parseMany groupRollingByEither (Right)"
        parseManyGroupsRollingEitherRight)
    -- requires -fspec-constr-recursive=10
    , (SpaceO_1, benchIO "parseMany groupRollingByEither (Alternating)"
        parseManyGroupsRollingEitherAlt1)
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
