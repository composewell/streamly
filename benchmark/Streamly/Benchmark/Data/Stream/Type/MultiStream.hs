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

{-# ANN append_x2 (PermitPatternMatches [''Int]) #-}
{-# ANN append_x2 (PermitConstructions [''()]) #-}
{-# ANN append_x2 (PermitTypeClasses []) #-}
{-# NOINLINE append_x2 #-}
append_x2 :: Int -> Int -> IO ()
append_x2 count n =
    drain $
        Common.append
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'append_x2
inspect $ 'append_x2 `hasNoType` ''SPEC
inspect $ 'append_x2 `hasNoType` ''S.AppendState
inspect $ 'append_x2 `hasNoType` ''S.Step
inspect $ 'append_x2 `hasNoType` ''Fold.Step
#endif

{-# ANN append_x4 (PermitPatternMatches [''Int]) #-}
{-# ANN append_x4 (PermitConstructions [''()]) #-}
{-# ANN append_x4 (PermitTypeClasses []) #-}
{-# NOINLINE append_x4 #-}
append_x4 :: Int -> Int -> IO ()
append_x4 count n =
    drain $
    Common.append
        (Common.append
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1)))
        (Common.append
              (sourceUnfoldrM count (n + 2))
              (sourceUnfoldrM count (n + 3)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'append_x4
inspect $ 'append_x4 `hasNoType` ''SPEC
inspect $ 'append_x4 `hasNoType` ''S.AppendState
inspect $ 'append_x4 `hasNoType` ''S.Step
inspect $ 'append_x4 `hasNoType` ''Fold.Step
#endif

-------------------------------------------------------------------------------
-- Branching
-------------------------------------------------------------------------------

{-# ANN ifThenElse (PermitPatternMatches [''Int]) #-}
{-# ANN ifThenElse (PermitConstructions [''()]) #-}
{-# ANN ifThenElse (PermitTypeClasses []) #-}
{-# NOINLINE ifThenElse #-}
ifThenElse :: Int -> Int -> IO ()
ifThenElse count n =
    drain $
        S.ifThenElse
            (return True)
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'ifThenElse
inspect $ 'ifThenElse `hasNoType` ''SPEC
inspect $ 'ifThenElse `hasNoType` ''S.IfThenElseState
inspect $ 'ifThenElse `hasNoType` ''S.Step
inspect $ 'ifThenElse `hasNoType` ''Fold.Step
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
sourceConcatMapSingletonStreams :: Monad m => Int -> Int
    -> Stream m (Stream m Int)
sourceConcatMapSingletonStreams count start =
    fmap Stream.fromPure $ sourceUnfoldr count start

{-# INLINE sourceConcatMapStreams #-}
sourceConcatMapStreams :: Monad m => Int -> Int -> Int
    -> Stream m (Stream m Int)
sourceConcatMapStreams outer inner start =
    fmap (sourceUnfoldr inner) $ sourceUnfoldr outer start

{-# ANN concatMap (PermitPatternMatches [''Int,''Stream.Step]) #-}
{-# ANN concatMap (PermitConstructions
    [''Int,''SVar.State,''Maybe,''Stream.Step,''(),''Bool]) #-}
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

{-# ANN concatMapM_x2 (PermitPatternMatches
    [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN concatMapM_x2 (PermitConstructions
    [''Int,''Stream.Step,''Either,''SVar.State,''Maybe,''(,),''Stream
    ,''Bool]) #-}
{-# ANN concatMapM_x2 (PermitTypeClasses []) #-}
{-# NOINLINE concatMapM_x2 #-}
concatMapM_x2 :: Int -> Int -> IO ()
concatMapM_x2 value = withStream value $ \s ->
    drain $ do
        Stream.concatMapM (\x ->
            pure $ Stream.concatMapM (\y ->
                pure $ Stream.fromPure $ x + y) s) s

{-# ANN concatMapM_x3 (PermitPatternMatches
    [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN concatMapM_x3 (PermitConstructions
    [''Int,''Stream.Step,''Either,''Stream,''SVar.State,''Maybe,''(,)
    ,''Bool]) #-}
{-# ANN concatMapM_x3 (PermitTypeClasses []) #-}
{-# NOINLINE concatMapM_x3 #-}
concatMapM_x3 :: Int -> Int -> IO ()
concatMapM_x3 value = withStream value $ \s ->
    drain $ do
        Stream.concatMapM (\x ->
            pure $ Stream.concatMapM (\y ->
                pure $ Stream.concatMapM (\z ->
                    pure $ Stream.fromPure $ x + y + z) s) s) s

{-# ANN unfoldEach_FromStream (PermitPatternMatches [''Int,''Stream.Step]) #-}
{-# ANN unfoldEach_FromStream (PermitConstructions [''Int,''Stream.Step]) #-}
{-# ANN unfoldEach_FromStream (PermitTypeClasses []) #-}
{-# NOINLINE unfoldEach_FromStream #-}
unfoldEach_FromStream :: Int -> Int -> Int -> IO ()
unfoldEach_FromStream outer inner n =
    drain $ cmap
        (sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

    where

    cmap f = Stream.unfoldEach (UF.lmap f UF.fromStream)

{-# ANN concatMapM (PermitPatternMatches [''Int,''Stream.Step]) #-}
{-# ANN concatMapM (PermitConstructions
    [''Int,''SVar.State,''Maybe,''Stream.Step,''Bool]) #-}
{-# ANN concatMapM (PermitTypeClasses []) #-}
{-# NOINLINE concatMapM #-}
concatMapM :: Int -> Int -> Int -> IO ()
concatMapM outer inner n =
    drain $ S.concatMapM
        (return . sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

{-# ANN concatEffect (PermitPatternMatches [''Int,''Stream.Step]) #-}
{-# ANN concatEffect (PermitConstructions
    [''SVar.State,''Maybe,''Bool,''Stream.Step,''Int,''()]) #-}
{-# ANN concatEffect (PermitTypeClasses []) #-}
{-# NOINLINE concatEffect #-}
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

{-# ANN concatMap_Streams_Singleton (PermitPatternMatches
    [''Bool,''Int,''Stream.Step]) #-}
{-# ANN concatMap_Streams_Singleton (PermitConstructions
    [''Int,''Stream.Step,''SVar.State,''Maybe,''Bool]) #-}
{-# ANN concatMap_Streams_Singleton (PermitTypeClasses []) #-}
{-# NOINLINE concatMap_Streams_Singleton #-}
concatMap_Streams_Singleton :: Int -> Int -> IO ()
concatMap_Streams_Singleton value =
    (drain . S.concatMap id . sourceConcatMapSingletonStreams value)

{-# ANN concatMap_Streams (PermitPatternMatches [''Int,''Stream.Step]) #-}
{-# ANN concatMap_Streams (PermitConstructions
    [''Int,''SVar.State,''Maybe,''Stream.Step,''Bool]) #-}
{-# ANN concatMap_Streams (PermitTypeClasses []) #-}
{-# NOINLINE concatMap_Streams #-}
concatMap_Streams :: Int -> Int -> Int -> IO ()
concatMap_Streams outer inner =
    (S.drain . S.concatMap id . sourceConcatMapStreams outer inner)

-- concatMap unfoldr/unfoldr

{-# ANN concatMap_Pure (PermitPatternMatches [''Int,''Stream.Step]) #-}
{-# ANN concatMap_Pure (PermitConstructions
    [''Int,''SVar.State,''Maybe,''Stream.Step,''(),''Bool]) #-}
{-# ANN concatMap_Pure (PermitTypeClasses []) #-}
{-# NOINLINE concatMap_Pure #-}
concatMap_Pure :: Int -> Int -> Int -> IO ()
concatMap_Pure outer inner n =
    drain $ S.concatMap
        (sourceUnfoldr inner)
        (sourceUnfoldr outer n)

#ifdef INSPECTION
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'concatMap_Pure [''Applicative]
#else
inspect $ hasNoTypeClasses 'concatMap_Pure
#endif
inspect $ 'concatMap_Pure `hasNoType` ''SPEC
-- inspect $ 'concatMap_Pure `hasNoType` ''S.Step
inspect $ 'concatMap_Pure `hasNoType` ''Fold.Step
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

{-# ANN unfoldEach_CarryInput (PermitPatternMatches [''Int]) #-}
{-# ANN unfoldEach_CarryInput (PermitConstructions [''Int,''()]) #-}
{-# ANN unfoldEach_CarryInput (PermitTypeClasses []) #-}
{-# NOINLINE unfoldEach_CarryInput #-}
unfoldEach_CarryInput :: Int -> Int -> Int -> IO ()
unfoldEach_CarryInput outer inner start = drain $
     S.unfoldEach (UF.carryInput (sourceUnfoldrMUnfold inner start))
        $ sourceUnfoldrM outer start

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldEach_CarryInput
inspect $ 'unfoldEach_CarryInput `hasNoType` ''Producer.ConcatState
inspect $ 'unfoldEach_CarryInput `hasNoType` ''S.Step
inspect $ 'unfoldEach_CarryInput `hasNoType` ''Fold.Step
inspect $ 'unfoldEach_CarryInput `hasNoType` ''SPEC
#endif

{-# ANN unfoldEach_CarryInput_x3 (PermitPatternMatches [''Int]) #-}
{-# ANN unfoldEach_CarryInput_x3 (PermitConstructions [''Int,''()]) #-}
{-# ANN unfoldEach_CarryInput_x3 (PermitTypeClasses []) #-}
{-# NOINLINE unfoldEach_CarryInput_x3 #-}
unfoldEach_CarryInput_x3 :: Int -> Int -> IO ()
unfoldEach_CarryInput_x3 linearCount start = drain $ do
    S.unfoldEach
        (UF.carryInput (UF.lmap snd (sourceUnfoldrMUnfold nestedCount3 start)))
         $ S.unfoldEach
             (UF.carryInput (sourceUnfoldrMUnfold nestedCount3 start))
            $ sourceUnfoldrM nestedCount3 start
    where

    nestedCount3 = round (fromIntegral linearCount**(1/3::Double))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldEach_CarryInput_x3
inspect $ 'unfoldEach_CarryInput_x3 `hasNoType` ''Producer.ConcatState
inspect $ 'unfoldEach_CarryInput_x3 `hasNoType` ''S.Step
inspect $ 'unfoldEach_CarryInput_x3 `hasNoType` ''Fold.Step
inspect $ 'unfoldEach_CarryInput_x3 `hasNoType` ''SPEC
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

{-# ANN foldManyPost (PermitPatternMatches [''Int]) #-}
{-# ANN foldManyPost (PermitConstructions [''()]) #-}
{-# ANN foldManyPost (PermitTypeClasses []) #-}
{-# NOINLINE foldManyPost #-}
foldManyPost :: Int -> Int -> IO ()
foldManyPost value =
    withStream value $
          Common.drain
        . fmap getSum
        . S.foldManyPost (FL.take 2 FL.mconcat)
        . fmap Sum

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldManyPost
inspect $ 'foldManyPost `hasNoType` ''S.Step
inspect $ 'foldManyPost `hasNoType` ''S.FoldManyPost
inspect $ 'foldManyPost `hasNoType` ''FL.Step
inspect $ 'foldManyPost `hasNoType` ''SPEC
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
    [ (SpaceO_1, benchIO "append_x2" $ append_x2 (size `div` 2))
    , (SpaceO_1, benchIO "append_x4 (2,2,x/4)" $ append_x4 (size `div` 4))
    , (SpaceO_1, benchIO "ifThenElse" $ ifThenElse (size `div` 2))
    , (SpaceO_1, benchIO "zipWith" $ zipWith size)
    , (SpaceO_1, benchIO "zipWithM" $ zipWithM size)
    , (SpaceO_1, benchIO "concatEffect" $ concatEffect size)

    -- concatMap
    , (SpaceO_1, benchIO "concatMap (unfoldrM outer=2 inner=Max/2)" $
          concatMap 2 (size `div` 2))
    , (SpaceO_1, benchIO "concatMap_Pure (unfoldr outer=Max inner=1)" $
          concatMap_Pure size 1)
    , (SpaceO_1, benchIO "concatMap_Pure (unfoldr outer=inner=sqrt Max)" $
          concatMap_Pure sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMap_Pure (unfoldr outer=1 inner=Max)" $
          concatMap_Pure 1 size)
    , (SpaceO_1, benchIO "concatMap (unfoldrM outer=Max inner=1)" $
          concatMap size 1)
    , (SpaceO_1, benchIO "concatMap (unfoldrM outer=inner=sqrt Max)" $
          concatMap sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMap (unfoldrM outer=1 inner=Max)" $
          concatMap 1 size)

    -- concatMap Streams
    -- Using boxed values/streams may have entirely different perf profile
    , ( SpaceO_1
      , benchIO "concatMap_Streams_Singleton (fromPure outer=Max inner=1)"
          $ concatMap_Streams_Singleton size)
    , (SpaceO_1, benchIO "concatMap_Streams (unfoldr outer=Max inner=1)" $
          concatMap_Streams size 1)
    , (SpaceO_1, benchIO "concatMap_Streams (unfoldr outer=inner=sqrt Max)" $
          concatMap_Streams sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMap_Streams (unfoldr outer=1 inner=Max)" $
          concatMap_Streams 1 size)

    -- concatMapM
    , (SpaceO_1, benchIO "concatMapM (unfoldrM outer=Max inner=1)" $
          concatMapM size 1)
    , (SpaceO_1, benchIO "concatMapM (unfoldrM outer=inner=sqrt Max)" $
          concatMapM sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMapM (unfoldrM outer=1 inner=Max)" $
          concatMapM 1 size)
    , (SpaceO_1, benchIO "concatMapM_x2 (fromPure)" $ concatMapM_x2 sqrtVal)
    , (SpaceO_1, benchIO "concatMapM_x3 (fromPure)" $ concatMapM_x3 cubertVal)

    -- concatMap via unfoldEach
    , ( SpaceO_1
      , benchIO "unfoldEach_FromStream (concatMap equiv. outer=Max inner=1)"
          $ unfoldEach_FromStream size 1)
    , ( SpaceO_1
      , benchIO "unfoldEach_FromStream (concatMap equiv. outer=inner=sqrt Max)"
          $ unfoldEach_FromStream sqrtVal sqrtVal)
    , ( SpaceO_1
      , benchIO "unfoldEach_FromStream (concatMap equiv. outer=1 inner=Max)"
          $ unfoldEach_FromStream 1 size)

    -- unfoldCross
    , (SpaceO_1, benchIO "unfoldCross (outer=Max inner=1)" $ unfoldCross size 1)
    , (SpaceO_1, benchIO "unfoldCross (outer=inner=sqrt Max)" $
          unfoldCross sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "unfoldCross (outer=1 inner=Max)" $ unfoldCross 1 size)

    -- unfoldEach
    , (SpaceO_1, benchIO "unfoldEach (outer=Max inner=1)" $ unfoldEach size 1)
    , (SpaceO_1, benchIO "unfoldEach (outer=inner=sqrt Max)" $
          unfoldEach sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "unfoldEach (outer=1 inner=Max)" $ unfoldEach 1 size)

    -- concatMap vs unfoldEach
    , (SpaceO_1, benchIO "unfoldEach_CarryInput (outer=Max inner=1)" $
          unfoldEach_CarryInput size 1)
    , (SpaceO_1, benchIO "unfoldEach_CarryInput (outer=inner=sqrt Max)" $
          unfoldEach_CarryInput sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "unfoldEach_CarryInput (outer=1 inner=Max)" $
          unfoldEach_CarryInput 1 size)
    , (SpaceO_1, benchIO "unfoldEach_CarryInput_x3 (outer=inner=cubert Max)" $
          unfoldEach_CarryInput_x3 size)

    -- Fold Many
    , (SpaceO_1, benchIO "foldMany" $ foldMany size)
    , (SpaceO_1, benchIO "foldManyPost" $ foldManyPost size)
    , (SpaceO_1, benchIO "refoldMany" $ refoldMany size)
    , (SpaceO_1, benchIO "refoldIterateM" $ refoldIterateM size)
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral size :: Double) -- double nested loop
    cubertVal = round (fromIntegral size**(1/3::Double)) -- triple nested loop
