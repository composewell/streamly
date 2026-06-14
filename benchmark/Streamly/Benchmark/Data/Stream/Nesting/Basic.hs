-- |
-- Module      : Stream.Nesting.Basic
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Stream.Nesting.Basic (benchmarks) where

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Producer as Producer
import Test.Inspection
#endif

import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Stream.Common hiding (benchIO)
import Stream.Type (benchIO, withRandomIntIO)
import Streamly.Benchmark.Common
import Prelude hiding (concatMap, zipWith)

-------------------------------------------------------------------------------
-- Multi-Stream
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Appending
-------------------------------------------------------------------------------

interleave2 :: Int -> IO ()
interleave2 count = withRandomIntIO $ \n ->
    drain $
        S.interleave
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'interleave2
inspect $ 'interleave2 `hasNoType` ''SPEC
inspect $ 'interleave2 `hasNoType` ''Producer.InterleaveState
inspect $ 'interleave2 `hasNoType` ''S.Step
inspect $ 'interleave2 `hasNoType` ''Fold.Step
#endif

roundRobin2 :: Int -> IO ()
roundRobin2 count = withRandomIntIO $ \n ->
    S.drain $
    S.roundRobin
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'roundRobin2
inspect $ 'roundRobin2 `hasNoType` ''SPEC
inspect $ 'roundRobin2 `hasNoType` ''S.InterleaveState
inspect $ 'roundRobin2 `hasNoType` ''S.Step
inspect $ 'roundRobin2 `hasNoType` ''Fold.Step
#endif

-------------------------------------------------------------------------------
-- Merging
-------------------------------------------------------------------------------

mergeBy :: Int -> IO ()
mergeBy count = withRandomIntIO $ \n ->
    Stream.drain
        $ Stream.mergeBy
            compare
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mergeBy
inspect $ 'mergeBy `hasNoType` ''SPEC
-- inspect $ 'mergeBy `hasNoType` ''S.Step
inspect $ 'mergeBy `hasNoType` ''Fold.Step
#endif

mergeByM :: Int -> IO ()
mergeByM count = withRandomIntIO $ \n ->
    Stream.drain
        $ Stream.mergeByM
            (\a b -> return $ compare a b)
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mergeByM
inspect $ 'mergeByM `hasNoType` ''SPEC
-- inspect $ 'mergeByM `hasNoType` ''S.Step
inspect $ 'mergeByM `hasNoType` ''Fold.Step
#endif

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- joining 2 streams using n-ary ops
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldrMUF #-}
-- unfold input is (count, value)
sourceUnfoldrMUF :: Monad m => Int -> UF.Unfold m (Int, Int) Int
sourceUnfoldrMUF count = UF.unfoldrM step
    where
    step (cnt, start) =
        return $
            if cnt > start + count
            then Nothing
            else Just (cnt, (cnt + 1, start))

bfsUnfoldEach :: Int -> Int -> IO ()
bfsUnfoldEach outer inner = withRandomIntIO $ \n ->
    S.drain $ S.bfsUnfoldEach
        -- (UF.lmap return (UF.replicateM inner))
        (UF.lmap (\x -> (x,x)) (sourceUnfoldrMUF inner))
        (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'bfsUnfoldEach
-- inspect $ 'bfsUnfoldEach `hasNoType` ''S.Step
inspect $ 'bfsUnfoldEach `hasNoType` ''Fold.Step
inspect $ 'bfsUnfoldEach `hasNoType` ''SPEC
#endif

altBfsUnfoldEach :: Int -> Int -> IO ()
altBfsUnfoldEach outer inner = withRandomIntIO $ \n ->
    S.drain $ S.altBfsUnfoldEach
        -- (UF.lmap return (UF.replicateM inner))
        (UF.lmap (\x -> (x,x)) (sourceUnfoldrMUF inner))
        (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'altBfsUnfoldEach
-- inspect $ 'altBfsUnfoldEach `hasNoType` ''S.Step
inspect $ 'altBfsUnfoldEach `hasNoType` ''Fold.Step
-- inspect $ 'altBfsUnfoldEach `hasNoType` ''SPEC
#endif

unfoldSched :: Int -> Int -> IO ()
unfoldSched outer inner = withRandomIntIO $ \n ->
    S.drain $ S.unfoldSched
        -- (UF.lmap return (UF.replicateM inner))
        (UF.lmap (\x -> (x,x)) (sourceUnfoldrMUF inner))
        (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldSched
-- inspect $ 'unfoldSched `hasNoType` ''S.Step
inspect $ 'unfoldSched `hasNoType` ''Fold.Step
inspect $ 'unfoldSched `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    -- multi-stream
      [ (SpaceO_1, benchIO "interleave" $ interleave2 (size `div` 2))
      , (SpaceO_1, benchIO "roundRobin" $ roundRobin2 (size `div` 2))
      , (SpaceO_1, benchIO "mergeBy compare" $ mergeBy (size `div` 2))
      , (SpaceO_1, benchIO "mergeByM compare" $ mergeByM (size `div` 2))

      -- join 2 streams using n-ary ops
      , (SpaceO_1, benchIO "bfsUnfoldEach" $ bfsUnfoldEach 2 (size `div` 2))
      , (SpaceO_1, benchIO "altBfsUnfoldEach" $ altBfsUnfoldEach 2 (size `div` 2))
      , (SpaceO_1, benchIO "unfoldSched" $ unfoldSched 2 (size `div` 2))

      , (HeapO_n, benchIO "bfsUnfoldEach (n of 1)" $ bfsUnfoldEach size 1)
      , (HeapO_n, benchIO "bfsUnfoldEach (sqrtVal of sqrtVal)" $ bfsUnfoldEach sqrtVal sqrtVal)
      , (HeapO_n, benchIO "altBfsUnfoldEach (n of 1)" $ altBfsUnfoldEach size 1)
      , (HeapO_n, benchIO "altBfsUnfoldEach (sqrtVal of sqrtVal)" $ altBfsUnfoldEach sqrtVal sqrtVal)
      , (HeapO_n, benchIO "unfoldSched (n of 1)" $ unfoldSched size 1)
      , (HeapO_n, benchIO "unfoldSched (sqrtVal of sqrtVal)" $ unfoldSched sqrtVal sqrtVal)
      ]

    where

    sqrtVal = round $ sqrt (fromIntegral size :: Double)
