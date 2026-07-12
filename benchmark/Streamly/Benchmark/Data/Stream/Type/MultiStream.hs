-- |
-- Module      : Stream.Type.MultiStream
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

-- | Benchmarks for operations combining multiple streams: appending, zipping,
-- @concatMap@\/@unfoldEach@ style flattening and the @foldMany@ family.
module Stream.Type.MultiStream
    ( benchmarks
    ) where

#ifdef INSPECTION
import Test.Inspection
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Producer as Producer
#endif

import GHC.Types (SPEC(..))
import Data.Monoid (Sum(..))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Data.Unfold (Unfold)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Refold.Type as Refold
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as UF

import Test.Tasty.Bench
import qualified Stream.Common as Common
import Stream.Common hiding (benchIO)
import Stream.Type.Basic (benchIO, withStream)
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import qualified Streamly.Internal.Data.SVar.Type as SVar
import Prelude hiding (concatMap, zipWith)

-------------------------------------------------------------------------------
-- Multi-stream
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Appending
-------------------------------------------------------------------------------

{-# ANN serial2 (PermitPatternMatches [''Int]) #-}
{-# ANN serial2 (PermitConstructions [''()]) #-}
{-# ANN serial2 (PermitTypeClasses []) #-}
{-# NOINLINE serial2 #-}
serial2 :: Int -> Int -> IO ()
serial2 count n =
    drain $
        Common.append
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'serial2
inspect $ 'serial2 `hasNoType` ''SPEC
inspect $ 'serial2 `hasNoType` ''S.AppendState
inspect $ 'serial2 `hasNoType` ''S.Step
inspect $ 'serial2 `hasNoType` ''Fold.Step
#endif

{-# ANN serial4 (PermitPatternMatches [''Int]) #-}
{-# ANN serial4 (PermitConstructions [''()]) #-}
{-# ANN serial4 (PermitTypeClasses []) #-}
{-# NOINLINE serial4 #-}
serial4 :: Int -> Int -> IO ()
serial4 count n =
    drain $
    Common.append
        (Common.append
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1)))
        (Common.append
              (sourceUnfoldrM count (n + 2))
              (sourceUnfoldrM count (n + 3)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'serial4
inspect $ 'serial4 `hasNoType` ''SPEC
inspect $ 'serial4 `hasNoType` ''S.AppendState
inspect $ 'serial4 `hasNoType` ''S.Step
inspect $ 'serial4 `hasNoType` ''Fold.Step
#endif

-------------------------------------------------------------------------------
-- Branching
-------------------------------------------------------------------------------

ifThenElse2 :: Int -> Int -> IO ()
ifThenElse2 count n =
    drain $
        S.ifThenElse
            (return True)
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'ifThenElse2
inspect $ 'ifThenElse2 `hasNoType` ''SPEC
inspect $ 'ifThenElse2 `hasNoType` ''S.IfThenElseState
inspect $ 'ifThenElse2 `hasNoType` ''S.Step
inspect $ 'ifThenElse2 `hasNoType` ''Fold.Step
#endif

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

{-# ANN zipWith (PermitPatternMatches [''Int]) #-}
{-# ANN zipWith (PermitConstructions [''()]) #-}
{-# ANN zipWith (PermitTypeClasses []) #-}
{-# NOINLINE zipWith #-}
zipWith :: Int -> Int -> IO ()
zipWith value n =
    let src = sourceUnfoldrM value n
    in drain $ S.zipWith (,) src src

#ifdef INSPECTION
inspect $ 'zipWith `hasNoType` ''SPEC
-- inspect $ 'zipWith `hasNoType` ''S.Step
inspect $ 'zipWith `hasNoType` ''Fold.Step
#endif

{-# ANN zipWithM (PermitPatternMatches [''Int]) #-}
{-# ANN zipWithM (PermitConstructions [''()]) #-}
{-# ANN zipWithM (PermitTypeClasses []) #-}
{-# NOINLINE zipWithM #-}
zipWithM :: Int -> Int -> IO ()
zipWithM value n =
    let src = sourceUnfoldrM value n
    in drain $ S.zipWithM (curry return) src src

#ifdef INSPECTION
inspect $ 'zipWithM `hasNoType` ''SPEC
-- inspect $ 'zipWithM `hasNoType` ''S.Step
inspect $ 'zipWithM `hasNoType` ''Fold.Step
#endif

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

{-# INLINE sourceConcatMapSingletonStreams #-}
sourceConcatMapSingletonStreams :: Monad m => Int -> Int -> Stream m (Stream m Int)
sourceConcatMapSingletonStreams count start =
    fmap Stream.fromPure $ sourceUnfoldr count start

{-# INLINE sourceConcatMapStreams #-}
sourceConcatMapStreams :: Monad m => Int -> Int -> Int -> Stream m (Stream m Int)
sourceConcatMapStreams outer inner start =
    fmap (sourceUnfoldr inner) $ sourceUnfoldr outer start

{-# ANN concatMap (PermitPatternMatches [''Int,''Stream.Step]) #-}
{-# ANN concatMap (PermitConstructions [''Int,''SVar.State,''Maybe,''Stream.Step,''(),''Bool]) #-}
{-# ANN concatMap (PermitTypeClasses []) #-}
{-# NOINLINE concatMap #-}
concatMap :: Int -> Int -> Int -> IO ()
concatMap outer inner n =
    drain $ S.concatMap
        (sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMap
inspect $ 'concatMap `hasNoType` ''SPEC
-- inspect $ 'concatMap `hasNoType` ''S.Step
inspect $ 'concatMap `hasNoType` ''Fold.Step
#endif

{-# ANN concatMapM2 (PermitPatternMatches [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN concatMapM2 (PermitConstructions [''Int,''Stream.Step,''Either,''SVar.State,''Maybe,''(,),''Stream,''Bool]) #-}
{-# ANN concatMapM2 (PermitTypeClasses []) #-}
{-# NOINLINE concatMapM2 #-}
concatMapM2 :: Int -> Int -> IO ()
concatMapM2 value = withStream value $ \s ->
    drain $ do
        Stream.concatMapM (\x ->
            pure $ Stream.concatMapM (\y ->
                pure $ Stream.fromPure $ x + y) s) s

{-# ANN concatMapM3 (PermitPatternMatches [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN concatMapM3 (PermitConstructions [''Int,''Stream.Step,''Either,''Stream,''SVar.State,''Maybe,''(,),''Bool]) #-}
{-# ANN concatMapM3 (PermitTypeClasses []) #-}
{-# NOINLINE concatMapM3 #-}
concatMapM3 :: Int -> Int -> IO ()
concatMapM3 value = withStream value $ \s ->
    drain $ do
        Stream.concatMapM (\x ->
            pure $ Stream.concatMapM (\y ->
                pure $ Stream.concatMapM (\z ->
                    pure $ Stream.fromPure $ x + y + z) s) s) s

{-# ANN concatMapViaUnfoldEach (PermitPatternMatches [''Int,''Stream.Step]) #-}
{-# ANN concatMapViaUnfoldEach (PermitConstructions [''Int,''Stream.Step]) #-}
{-# ANN concatMapViaUnfoldEach (PermitTypeClasses []) #-}
{-# NOINLINE concatMapViaUnfoldEach #-}
concatMapViaUnfoldEach :: Int -> Int -> Int -> IO ()
concatMapViaUnfoldEach outer inner n =
    drain $ cmap
        (sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

    where

    cmap f = Stream.unfoldEach (UF.lmap f UF.fromStream)

{-# ANN concatMapM (PermitPatternMatches [''Int,''Stream.Step]) #-}
{-# ANN concatMapM (PermitConstructions [''Int,''SVar.State,''Maybe,''Stream.Step,''Bool]) #-}
{-# ANN concatMapM (PermitTypeClasses []) #-}
{-# NOINLINE concatMapM #-}
concatMapM :: Int -> Int -> Int -> IO ()
concatMapM outer inner n =
    drain $ S.concatMapM
        (return . sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

concatEffect :: Int -> Int -> IO ()
concatEffect count n =
    drain $ S.concatEffect $ return $ sourceUnfoldrM count n

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatEffect
inspect $ 'concatEffect `hasNoType` ''SPEC
-- inspect $ 'concatEffect `hasNoType` ''S.Step
inspect $ 'concatEffect `hasNoType` ''Fold.Step
#endif

-- concatMap Streams

{-# ANN concatMapSingletonStreams (PermitPatternMatches [''Bool,''Int,''Stream.Step]) #-}
{-# ANN concatMapSingletonStreams (PermitConstructions [''Int,''Stream.Step,''SVar.State,''Maybe,''Bool]) #-}
{-# ANN concatMapSingletonStreams (PermitTypeClasses []) #-}
{-# NOINLINE concatMapSingletonStreams #-}
concatMapSingletonStreams :: Int -> Int -> IO ()
concatMapSingletonStreams value =
    (drain . S.concatMap id . sourceConcatMapSingletonStreams value)

{-# ANN concatMapStreams (PermitPatternMatches [''Int,''Stream.Step]) #-}
{-# ANN concatMapStreams (PermitConstructions [''Int,''SVar.State,''Maybe,''Stream.Step,''Bool]) #-}
{-# ANN concatMapStreams (PermitTypeClasses []) #-}
{-# NOINLINE concatMapStreams #-}
concatMapStreams :: Int -> Int -> Int -> IO ()
concatMapStreams outer inner =
    (S.drain . S.concatMap id . sourceConcatMapStreams outer inner)

-- concatMap unfoldr/unfoldr

{-# ANN concatMapPure (PermitPatternMatches [''Int,''Stream.Step]) #-}
{-# ANN concatMapPure (PermitConstructions [''Int,''SVar.State,''Maybe,''Stream.Step,''(),''Bool]) #-}
{-# ANN concatMapPure (PermitTypeClasses []) #-}
{-# NOINLINE concatMapPure #-}
concatMapPure :: Int -> Int -> Int -> IO ()
concatMapPure outer inner n =
    drain $ S.concatMap
        (sourceUnfoldr inner)
        (sourceUnfoldr outer n)

#ifdef INSPECTION
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'concatMapPure [''Applicative]
#else
inspect $ hasNoTypeClasses 'concatMapPure
#endif
inspect $ 'concatMapPure `hasNoType` ''SPEC
-- inspect $ 'concatMapPure `hasNoType` ''S.Step
inspect $ 'concatMapPure `hasNoType` ''Fold.Step
#endif

{-# INLINE sourceUnfoldrMUnfold #-}
sourceUnfoldrMUnfold :: Monad m => Int -> Int -> Unfold m Int Int
sourceUnfoldrMUnfold size start = UF.unfoldrM step

    where

    step i =
        return
            $ if i < start + size
              then Just (i, i + 1)
              else Nothing

{-# ANN unfoldEach (PermitPatternMatches [''Int]) #-}
{-# ANN unfoldEach (PermitConstructions [''Int,''()]) #-}
{-# ANN unfoldEach (PermitTypeClasses []) #-}
{-# NOINLINE unfoldEach #-}
unfoldEach :: Int -> Int -> Int -> IO ()
unfoldEach outer inner start = drain $
     S.unfoldEach (sourceUnfoldrMUnfold inner start)
        $ sourceUnfoldrM outer start

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldEach
inspect $ 'unfoldEach `hasNoType` ''Producer.ConcatState
inspect $ 'unfoldEach `hasNoType` ''SPEC
inspect $ 'unfoldEach `hasNoType` ''S.Step
inspect $ 'unfoldEach `hasNoType` ''Fold.Step
#endif

{-# ANN unfoldEach2 (PermitPatternMatches [''Int]) #-}
{-# ANN unfoldEach2 (PermitConstructions [''Int,''()]) #-}
{-# ANN unfoldEach2 (PermitTypeClasses []) #-}
{-# NOINLINE unfoldEach2 #-}
unfoldEach2 :: Int -> Int -> Int -> IO ()
unfoldEach2 outer inner start = drain $
     S.unfoldEach (UF.carryInput (sourceUnfoldrMUnfold inner start))
        $ sourceUnfoldrM outer start

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldEach2
inspect $ 'unfoldEach2 `hasNoType` ''Producer.ConcatState
inspect $ 'unfoldEach2 `hasNoType` ''S.Step
inspect $ 'unfoldEach2 `hasNoType` ''Fold.Step
inspect $ 'unfoldEach2 `hasNoType` ''SPEC
#endif

{-# ANN unfoldEach3 (PermitPatternMatches [''Int]) #-}
{-# ANN unfoldEach3 (PermitConstructions [''Int,''()]) #-}
{-# ANN unfoldEach3 (PermitTypeClasses []) #-}
{-# NOINLINE unfoldEach3 #-}
unfoldEach3 :: Int -> Int -> IO ()
unfoldEach3 linearCount start = drain $ do
    S.unfoldEach (UF.carryInput (UF.lmap snd (sourceUnfoldrMUnfold nestedCount3 start)))
         $ S.unfoldEach (UF.carryInput (sourceUnfoldrMUnfold nestedCount3 start))
            $ sourceUnfoldrM nestedCount3 start
    where

    nestedCount3 = round (fromIntegral linearCount**(1/3::Double))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldEach3
inspect $ 'unfoldEach3 `hasNoType` ''Producer.ConcatState
inspect $ 'unfoldEach3 `hasNoType` ''S.Step
inspect $ 'unfoldEach3 `hasNoType` ''Fold.Step
inspect $ 'unfoldEach3 `hasNoType` ''SPEC
#endif

{-# ANN unfoldCross (PermitPatternMatches [''Int]) #-}
{-# ANN unfoldCross (PermitConstructions [''()]) #-}
{-# ANN unfoldCross (PermitTypeClasses []) #-}
{-# NOINLINE unfoldCross #-}
unfoldCross :: Int -> Int -> Int -> IO ()
unfoldCross outer inner start = drain $
    Stream.unfoldCross
        UF.identity
        (sourceUnfoldrM outer start)
        (sourceUnfoldrM inner start)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldCross
inspect $ 'unfoldCross `hasNoType` ''Producer.CrossState
inspect $ 'unfoldCross `hasNoType` ''Producer.ConcatState
inspect $ 'unfoldCross `hasNoType` ''S.Step
inspect $ 'unfoldCross `hasNoType` ''Fold.Step
inspect $ 'unfoldCross `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Fold Many
-------------------------------------------------------------------------------

{-# ANN foldMany (PermitPatternMatches [''Int]) #-}
{-# ANN foldMany (PermitConstructions [''()]) #-}
{-# ANN foldMany (PermitTypeClasses []) #-}
{-# NOINLINE foldMany #-}
foldMany :: Int -> Int -> IO ()
foldMany value =
    withStream value $
          Common.drain
        . fmap getSum
        . S.foldMany (FL.take 2 FL.mconcat)
        . fmap Sum

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldMany
inspect $ 'foldMany `hasNoType` ''S.Step
inspect $ 'foldMany `hasNoType` ''S.FoldMany
inspect $ 'foldMany `hasNoType` ''FL.Step
inspect $ 'foldMany `hasNoType` ''SPEC
#endif

{-# ANN foldMany1 (PermitPatternMatches [''Int]) #-}
{-# ANN foldMany1 (PermitConstructions [''()]) #-}
{-# ANN foldMany1 (PermitTypeClasses []) #-}
{-# NOINLINE foldMany1 #-}
foldMany1 :: Int -> Int -> IO ()
foldMany1 value =
    withStream value $
          Common.drain
        . fmap getSum
        . S.foldManyPost (FL.take 2 FL.mconcat)
        . fmap Sum

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldMany1
inspect $ 'foldMany1 `hasNoType` ''S.Step
inspect $ 'foldMany1 `hasNoType` ''S.FoldManyPost
inspect $ 'foldMany1 `hasNoType` ''FL.Step
inspect $ 'foldMany1 `hasNoType` ''SPEC
#endif

{-# ANN refoldMany (PermitPatternMatches [''Int]) #-}
{-# ANN refoldMany (PermitConstructions [''()]) #-}
{-# ANN refoldMany (PermitTypeClasses []) #-}
{-# NOINLINE refoldMany #-}
refoldMany :: Int -> Int -> IO ()
refoldMany value =
    withStream value $
          Common.drain
        . fmap getSum
        . S.refoldMany (Refold.take 2 Refold.sconcat) (return mempty)
        . fmap Sum

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'refoldMany
inspect $ 'refoldMany `hasNoType` ''S.Step
inspect $ 'refoldMany `hasNoType` ''S.FoldMany
inspect $ 'refoldMany `hasNoType` ''FL.Step
inspect $ 'refoldMany `hasNoType` ''SPEC
#endif

{-# ANN refoldIterateM (PermitPatternMatches [''Int]) #-}
{-# ANN refoldIterateM (PermitConstructions [''()]) #-}
{-# ANN refoldIterateM (PermitTypeClasses []) #-}
{-# NOINLINE refoldIterateM #-}
refoldIterateM :: Int -> Int -> IO ()
refoldIterateM value =
    withStream value $
        Common.drain
            . fmap getSum
            . S.refoldIterateM
                (Refold.take 2 Refold.sconcat) (return (Sum 0))
            . fmap Sum

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'refoldIterateM
inspect $ 'refoldIterateM `hasNoType` ''S.Step
inspect $ 'refoldIterateM `hasNoType` ''S.CIterState
inspect $ 'refoldIterateM `hasNoType` ''FL.Step
inspect $ 'refoldIterateM `hasNoType` ''Refold.Tuple'Fused
inspect $ 'refoldIterateM `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

{-# ANN benchmarks "HLint: ignore" #-}
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    -- Multi-stream (concatMap/foldMany)
    [ (SpaceO_1, benchIO "serial" $ serial2 (size `div` 2))
    , (SpaceO_1, benchIO "serial (2,2,x/4)" $ serial4 (size `div` 4))
    , (SpaceO_1, benchIO "ifThenElse" $ ifThenElse2 (size `div` 2))
    , (SpaceO_1, benchIO "zipWith" $ zipWith size)
    , (SpaceO_1, benchIO "zipWithM" $ zipWithM size)
    , (SpaceO_1, benchIO "concatEffect" $ concatEffect size)
    , (SpaceO_1, benchIO "concatMap" $ concatMap 2 (size `div` 2))
    , (SpaceO_1, benchIO "concatMap unfoldr outer=Max inner=1" $
          concatMapPure size 1)
    , (SpaceO_1, benchIO "concatMap unfoldr outer=inner=(sqrt Max)" $
          concatMapPure sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMap unfoldr outer=1 inner=Max" $
          concatMapPure 1 size)
    , (SpaceO_1, benchIO "concatMap unfoldrM outer=max inner=1" $
          concatMap size 1)
    , (SpaceO_1, benchIO "concatMap unfoldrM outer=inner=(sqrt Max)" $
          concatMap sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMap unfoldrM outer=1 inner=Max" $
          concatMap 1 size)
    -- Using boxed values/streams may have entirely different perf profile
    , (SpaceO_1, benchIO "concatMap Streams fromPure outer=max inner=1" $
          concatMapSingletonStreams size)
    , (SpaceO_1, benchIO "concatMap Streams unfoldr outer=max inner=1" $
          concatMapStreams size 1)
    , (SpaceO_1, benchIO "concatMap Streams unfoldr outer=inner=(sqrt Max)" $
          concatMapStreams sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMap Streams unfoldr outer=1 inner=Max" $
          concatMapStreams 1 size)
    , (SpaceO_1, benchIO "concatMapM unfoldrM outer=max inner=1" $
          concatMapM size 1)
    , (SpaceO_1, benchIO "concatMapM unfoldrM outer=inner=(sqrt Max)" $
          concatMapM sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMapM unfoldrM outer=1 inner=Max" $
          concatMapM 1 size)
    , (SpaceO_1, benchIO "concatMapM2 fromPure" $ concatMapM2 sqrtVal)
    , (SpaceO_1, benchIO "concatMapM3 fromPure" $ concatMapM3 cubertVal)
    , (SpaceO_1, benchIO "concatMapViaUnfoldEach outer=max inner=1" $
          concatMapViaUnfoldEach size 1)
    , (SpaceO_1, benchIO "concatMapViaUnfoldEach outer=inner=(sqrt Max)" $
          concatMapViaUnfoldEach sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMapViaUnfoldEach outer=1 inner=Max" $
          concatMapViaUnfoldEach 1 size)
    , (SpaceO_1, benchIO "unfoldCross outer=max inner=1" $ unfoldCross size 1)
    , (SpaceO_1, benchIO "unfoldCross outer=inner=(sqrt Max)" $
          unfoldCross sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "unfoldCross outer=1 inner=Max" $ unfoldCross 1 size)
    -- concatMap vs unfoldEach
    , (SpaceO_1, benchIO "unfoldEach outer=Max inner=1" $ unfoldEach size 1)
    , (SpaceO_1, benchIO "unfoldEach outer=inner=(sqrt Max)" $
          unfoldEach sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "unfoldEach outer=1 inner=Max" $ unfoldEach 1 size)
    , (SpaceO_1, benchIO "unfoldEach2 outer=Max inner=1" $ unfoldEach2 size 1)
    , (SpaceO_1, benchIO "unfoldEach2 outer=inner=(sqrt Max)" $
          unfoldEach2 sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "unfoldEach2 outer=1 inner=Max" $ unfoldEach2 1 size)
    , (SpaceO_1, benchIO "unfoldEach3 outer=inner=(cubert Max)" $ unfoldEach3 size)

    -- Fold Many
    , (SpaceO_1, benchIO "foldMany" $ foldMany size)
    , (SpaceO_1, benchIO "foldMany1" $ foldMany1 size)
    , (SpaceO_1, benchIO "refoldMany" $ refoldMany size)
    , (SpaceO_1, benchIO "refoldIterateM" $ refoldIterateM size)
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral size :: Double) -- double nested loop
    cubertVal = round (fromIntegral size**(1/3::Double)) -- triple nested loop
