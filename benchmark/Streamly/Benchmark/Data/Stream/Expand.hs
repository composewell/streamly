-- |
-- Module      : Stream.Expand
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

module Stream.Expand (benchmarks) where

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import qualified Streamly.Internal.Data.Producer as Producer
import Test.Inspection
#endif

import Streamly.Data.Stream (Stream)
import Streamly.Data.Unfold (Unfold)

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.StreamK as StreamK

import Control.DeepSeq (NFData(..))
import System.Random (randomRIO)

import Test.Tasty.Bench
import Stream.Common hiding (benchIO)
import Streamly.Benchmark.Common
import Prelude hiding (concatMap, zipWith)

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> IO b -> Benchmark
benchIO name = bench name . nfIO

{-# INLINE withRandomIntIO #-}
withRandomIntIO :: (Int -> IO b) -> IO b
withRandomIntIO f = randomRIO (1, 1 :: Int) >>= f

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> IO b
withStream value f = withRandomIntIO (f . sourceUnfoldrM value)

mkCross :: Stream m a -> Stream.Nested m a
mkCross = Stream.Nested

unCross :: Stream.Nested m a -> Stream m a
unCross = Stream.unNested

{-# INLINE sourceConcatMapSingletonStreams #-}
sourceConcatMapSingletonStreams :: Monad m => Int -> Int -> Stream m (Stream m Int)
sourceConcatMapSingletonStreams count start =
    fmap Stream.fromPure $ sourceUnfoldr count start

{-# INLINE sourceConcatMapStreams #-}
sourceConcatMapStreams :: Monad m => Int -> Int -> Int -> Stream m (Stream m Int)
sourceConcatMapStreams outer inner start =
    fmap (sourceUnfoldr inner) $ sourceUnfoldr outer start

{-# INLINE toNullApPure #-}
toNullApPure :: MonadAsync m => Int -> Int -> m ()
toNullApPure linearCount start = drain $ unCross $
    (+) <$> mkCross (sourceUnfoldr nestedCount2 start)
        <*> mkCross (sourceUnfoldr nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullMPure #-}
toNullMPure :: MonadAsync m => Int -> Int -> m ()
toNullMPure linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount2 start)
    y <- mkCross (sourceUnfoldr nestedCount2 start)
    return $ x + y

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullM3Pure #-}
toNullM3Pure :: MonadAsync m => Int -> Int -> m ()
toNullM3Pure linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount3 start)
    y <- mkCross (sourceUnfoldr nestedCount3 start)
    z <- mkCross (sourceUnfoldr nestedCount3 start)
    return $ x + y + z

    where

    nestedCount3 = round (fromIntegral linearCount**(1/3::Double))

{-# INLINE filterAllOutMPure #-}
filterAllOutMPure :: MonadAsync m => Int -> Int -> m ()
filterAllOutMPure linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount2 start)
    y <- mkCross (sourceUnfoldr nestedCount2 start)
    let s = x + y
    if s < 0
    then return s
    else mkCross Stream.nil

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE filterAllInMPure #-}
filterAllInMPure :: MonadAsync m => Int -> Int -> m ()
filterAllInMPure linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount2 start)
    y <- mkCross (sourceUnfoldr nestedCount2 start)
    let s = x + y
    if s > 0
    then return s
    else mkCross Stream.nil

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

-------------------------------------------------------------------------------
-- Multi-Stream
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Appending
-------------------------------------------------------------------------------

{-# INLINE serial2 #-}
serial2 :: Int -> IO ()
serial2 count = withRandomIntIO $ \n ->
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

{-# INLINE serial4 #-}
serial4 :: Int -> IO ()
serial4 count = withRandomIntIO $ \n ->
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

{-# INLINE interleave2 #-}
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

{-# INLINE roundRobin2 #-}
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

{-# INLINE mergeBy #-}
mergeBy :: (Int -> Int -> Ordering) -> Int -> IO ()
mergeBy cmp count = withRandomIntIO $ \n ->
    Stream.drain
        $ Stream.mergeBy
            cmp
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mergeBy
inspect $ 'mergeBy `hasNoType` ''SPEC
-- inspect $ 'mergeBy `hasNoType` ''S.Step
inspect $ 'mergeBy `hasNoType` ''Fold.Step
#endif

{-# INLINE mergeByM #-}
mergeByM :: (Int -> Int -> Ordering) -> Int -> IO ()
mergeByM cmp count = withRandomIntIO $ \n ->
    Stream.drain
        $ Stream.mergeByM
            (\a b -> return $ cmp a b)
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

{-# INLINE zipWith #-}
zipWith :: Int -> IO ()
zipWith value = withRandomIntIO $ \n ->
    let src = sourceUnfoldrM value n
    in drain $ S.zipWith (,) src src

#ifdef INSPECTION
inspect $ 'zipWith `hasNoType` ''SPEC
-- inspect $ 'zipWith `hasNoType` ''S.Step
inspect $ 'zipWith `hasNoType` ''Fold.Step
#endif

{-# INLINE zipWithM #-}
zipWithM :: Int -> IO ()
zipWithM value = withRandomIntIO $ \n ->
    let src = sourceUnfoldrM value n
    in drain $ S.zipWithM (curry return) src src

#ifdef INSPECTION
inspect $ 'zipWithM `hasNoType` ''SPEC
-- inspect $ 'zipWithM `hasNoType` ''S.Step
inspect $ 'zipWithM `hasNoType` ''Fold.Step
#endif

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

{-# INLINE bfsUnfoldEach #-}
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

{-# INLINE altBfsUnfoldEach #-}
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

{-# INLINE unfoldSched #-}
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

o_1_space_joining :: Int -> [Benchmark]
o_1_space_joining value =
    [ bgroup "joining (2 of n/2)"
        [ benchIO "serial" $ serial2 (value `div` 2)
        , benchIO "serial (2,2,x/4)" $ serial4 (value `div` 4)
        , benchIO "interleave" $ interleave2 (value `div` 2)
        , benchIO "roundRobin" $ roundRobin2 (value `div` 2)
        , benchIO "mergeBy compare" $ mergeBy compare (value `div` 2)
        , benchIO "mergeByM compare" $ mergeByM compare (value `div` 2)
        , benchIO "mergeBy (flip compare)" $ mergeBy (flip compare) (value `div` 2)
        , benchIO "mergeByM (flip compare)" $ mergeByM (flip compare) (value `div` 2)
        , benchIO "zipWith" $ zipWith value
        , benchIO "zipWithM" $ zipWithM value

        -- join 2 streams using n-ary ops
        , benchIO "bfsUnfoldEach" $ bfsUnfoldEach 2 (value `div` 2)
        , benchIO "altBfsUnfoldEach" $ altBfsUnfoldEach 2 (value `div` 2)
        , benchIO "unfoldSched" $ unfoldSched 2 (value `div` 2)
        , benchIO "concatMap" $ concatMap 2 (value `div` 2)
        ]
    ]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

-- concatMap unfoldrM/unfoldrM

{-# INLINE concatMap #-}
concatMap :: Int -> Int -> IO ()
concatMap outer inner = withRandomIntIO $ \n ->
    drain $ S.concatMap
        (sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMap
inspect $ 'concatMap `hasNoType` ''SPEC
-- inspect $ 'concatMap `hasNoType` ''S.Step
inspect $ 'concatMap `hasNoType` ''Fold.Step
#endif

{-# INLINE concatMapM2 #-}
concatMapM2 :: Int -> IO ()
concatMapM2 value = withStream value $ \s ->
    drain $ do
        Stream.concatMapM (\x ->
            pure $ Stream.concatMapM (\y ->
                pure $ Stream.fromPure $ x + y) s) s

{-# INLINE concatMapM3 #-}
concatMapM3 :: Int -> IO ()
concatMapM3 value = withStream value $ \s ->
    drain $ do
        Stream.concatMapM (\x ->
            pure $ Stream.concatMapM (\y ->
                pure $ Stream.concatMapM (\z ->
                    pure $ Stream.fromPure $ x + y + z) s) s) s

{-# INLINE concatMapViaUnfoldEach #-}
concatMapViaUnfoldEach :: Int -> Int -> IO ()
concatMapViaUnfoldEach outer inner = withRandomIntIO $ \n ->
    drain $ cmap
        (sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

    where

    cmap f = Stream.unfoldEach (UF.lmap f UF.fromStream)

{-# INLINE concatMapM #-}
concatMapM :: Int -> Int -> IO ()
concatMapM outer inner = withRandomIntIO $ \n ->
    drain $ S.concatMapM
        (return . sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

-- concatMap Streams

{-# INLINE concatMapSingletonStreams #-}
concatMapSingletonStreams :: Int -> IO ()
concatMapSingletonStreams value =
    withRandomIntIO (drain . S.concatMap id . sourceConcatMapSingletonStreams value)

{-# INLINE concatMapStreams #-}
concatMapStreams :: Int -> Int -> IO ()
concatMapStreams outer inner =
    withRandomIntIO (S.drain . S.concatMap id . sourceConcatMapStreams outer inner)

-- concatMap unfoldr/unfoldr

{-# INLINE concatMapPure #-}
concatMapPure :: Int -> Int -> IO ()
concatMapPure outer inner = withRandomIntIO $ \n ->
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

{-# INLINE unfoldEach #-}
unfoldEach :: Int -> Int -> IO ()
unfoldEach outer inner = withRandomIntIO $ \start -> drain $
     -- XXX the replicateM takes much more time compared to unfoldrM, is there
     -- a perf issue or this is just because of accessing outer loop variables?
     -- S.unfoldEach (UF.lmap ((inner,) . return) UF.replicateM)
     S.unfoldEach (sourceUnfoldrMUnfold inner start)
        $ sourceUnfoldrM outer start

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldEach
inspect $ 'unfoldEach `hasNoType` ''Producer.ConcatState
inspect $ 'unfoldEach `hasNoType` ''SPEC
inspect $ 'unfoldEach `hasNoType` ''S.Step
inspect $ 'unfoldEach `hasNoType` ''Fold.Step
#endif

{-# INLINE unfoldEach2 #-}
unfoldEach2 :: Int -> Int -> IO ()
unfoldEach2 outer inner = withRandomIntIO $ \start -> drain $
     S.unfoldEach (UF.carryInput (sourceUnfoldrMUnfold inner start))
        $ sourceUnfoldrM outer start

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldEach2
inspect $ 'unfoldEach2 `hasNoType` ''Producer.ConcatState
inspect $ 'unfoldEach2 `hasNoType` ''S.Step
inspect $ 'unfoldEach2 `hasNoType` ''Fold.Step
inspect $ 'unfoldEach2 `hasNoType` ''SPEC
#endif

{-# INLINE unfoldEach3 #-}
unfoldEach3 :: Int -> IO ()
unfoldEach3 linearCount = withRandomIntIO $ \start -> drain $ do
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

{-# INLINE unfoldCross #-}
unfoldCross :: Int -> Int -> IO ()
unfoldCross outer inner = withRandomIntIO $ \start -> drain $
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

o_1_space_concat :: Int -> [Benchmark]
o_1_space_concat value = sqrtVal `seq`
    [ bgroup "concat"
        [ benchIO "concatMap unfoldr outer=Max inner=1" $ concatMapPure value 1
        , benchIO "concatMap unfoldr outer=inner=(sqrt Max)" $ concatMapPure sqrtVal sqrtVal
        , benchIO "concatMap unfoldr outer=1 inner=Max" $ concatMapPure 1 value

        , benchIO "concatMap unfoldrM outer=max inner=1" $ concatMap value 1
        , benchIO "concatMap unfoldrM outer=inner=(sqrt Max)" $ concatMap sqrtVal sqrtVal
        , benchIO "concatMap unfoldrM outer=1 inner=Max" $ concatMap 1 value

        -- Using boxed values/streams may have entirely different perf profile
        , benchIO "concatMap Streams fromPure outer=max inner=1" $
            concatMapSingletonStreams value
        , benchIO "concatMap Streams unfoldr outer=max inner=1" $
            concatMapStreams value 1
        , benchIO "concatMap Streams unfoldr outer=inner=(sqrt Max)" $
            concatMapStreams sqrtVal sqrtVal
        , benchIO "concatMap Streams unfoldr outer=1 inner=Max" $
            concatMapStreams 1 value

        , benchIO "concatMapM unfoldrM outer=max inner=1" $ concatMapM value 1
        , benchIO "concatMapM unfoldrM outer=inner=(sqrt Max)" $ concatMapM sqrtVal sqrtVal
        , benchIO "concatMapM unfoldrM outer=1 inner=Max" $ concatMapM 1 value

        , benchIO "concatMapM2 fromPure" $ concatMapM2 sqrtVal
        , benchIO "concatMapM3 fromPure" $ concatMapM3 cubertVal

        , benchIO "concatMapViaUnfoldEach outer=max inner=1" $ concatMapViaUnfoldEach value 1
        , benchIO "concatMapViaUnfoldEach outer=inner=(sqrt Max)" $ concatMapViaUnfoldEach sqrtVal sqrtVal
        , benchIO "concatMapViaUnfoldEach outer=1 inner=Max" $ concatMapViaUnfoldEach 1 value

        , benchIO "unfoldCross outer=max inner=1" $ unfoldCross value 1
        , benchIO "unfoldCross outer=inner=(sqrt Max)" $ unfoldCross sqrtVal sqrtVal
        , benchIO "unfoldCross outer=1 inner=Max" $ unfoldCross 1 value

        -- concatMap vs unfoldEach
        , benchIO "unfoldEach outer=Max inner=1" $ unfoldEach value 1
        , benchIO "unfoldEach outer=inner=(sqrt Max)" $ unfoldEach sqrtVal sqrtVal
        , benchIO "unfoldEach outer=1 inner=Max" $ unfoldEach 1 value

        , benchIO "unfoldEach2 outer=Max inner=1" $ unfoldEach2 value 1
        , benchIO "unfoldEach2 outer=inner=(sqrt Max)" $ unfoldEach2 sqrtVal sqrtVal
        , benchIO "unfoldEach2 outer=1 inner=Max" $ unfoldEach2 1 value

        , benchIO "unfoldEach3 outer=inner=(cubert Max)" $ unfoldEach3 value
        ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)
    cubertVal = round (fromIntegral value**(1/3::Double)) -- triple nested loop

o_n_heap_concat :: Int -> [Benchmark]
o_n_heap_concat value = sqrtVal `seq`
    [ bgroup "concat"
        [ benchIO "bfsUnfoldEach (n of 1)" $ bfsUnfoldEach value 1
        , benchIO "bfsUnfoldEach (sqrtVal of sqrtVal)" $ bfsUnfoldEach sqrtVal sqrtVal
        , benchIO "altBfsUnfoldEach (n of 1)" $ altBfsUnfoldEach value 1
        , benchIO "altBfsUnfoldEach (sqrtVal of sqrtVal)" $ altBfsUnfoldEach sqrtVal sqrtVal
        , benchIO "unfoldSched (n of 1)" $ unfoldSched value 1
        , benchIO "unfoldSched (sqrtVal of sqrtVal)" $ unfoldSched sqrtVal sqrtVal
        ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)

-------------------------------------------------------------------------------
-- Applicative
-------------------------------------------------------------------------------

{-# INLINE cross2 #-}
cross2 :: Int -> IO ()
cross2 linearCount = withRandomIntIO $ \start -> drain $
    Stream.crossWith (+)
        (sourceUnfoldr nestedCount2 start)
        (sourceUnfoldr nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE crossApply #-}
crossApply :: Int -> IO ()
crossApply linearCount = withRandomIntIO $ \start -> drain $
    Stream.crossApply
        ((+) <$> sourceUnfoldrM nestedCount2 start)
        (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE crossApplyFst #-}
crossApplyFst :: Int -> IO ()
crossApplyFst linearCount = withRandomIntIO $ \start -> drain $
    Stream.crossApplyFst
        (sourceUnfoldrM nestedCount2 start)
        (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE crossApplySnd #-}
crossApplySnd :: Int -> IO ()
crossApplySnd linearCount = withRandomIntIO $ \start -> drain $
    Stream.crossApplySnd
        (sourceUnfoldrM nestedCount2 start)
        (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

o_1_space_applicative :: Int -> [Benchmark]
o_1_space_applicative value =
    [ bgroup "Applicative"
        [ benchIO "(*>)" $ withRandomIntIO (apDiscardFst value)
        , benchIO "(<*)" $ withRandomIntIO (apDiscardSnd value)
        , benchIO "(<*>)" $ withRandomIntIO (toNullAp value)
        , benchIO "liftA2" $ withRandomIntIO (apLiftA2 value)
        , benchIO "crossApply" $ crossApply value
        , benchIO "crossApplyFst" $ crossApplyFst value
        , benchIO "crossApplySnd" $ crossApplySnd value
        , benchIO "pureDrain2" $ withRandomIntIO (toNullApPure value)
        , benchIO "pureCross2" $ cross2 value
        ]
    ]

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

o_1_space_monad :: Int -> [Benchmark]
o_1_space_monad value =
    [ bgroup "Monad"
        [ benchIO "then2" $ withRandomIntIO (monadThen value)
        , benchIO "drain2" $ withRandomIntIO (toNullM value)
        , benchIO "drain3" $ withRandomIntIO (toNullM3 value)
        , benchIO "filterAllOut2" $ withRandomIntIO (filterAllOutM value)
        , benchIO "filterAllIn2" $ withRandomIntIO (filterAllInM value)
        , benchIO "filterSome2" $ withRandomIntIO (filterSome value)
        , benchIO "breakAfterSome2" $ withRandomIntIO (breakAfterSome value)
        , benchIO "pureDrain2" $ withRandomIntIO (toNullMPure value)
        , benchIO "pureDrain3" $ withRandomIntIO (toNullM3Pure value)
        , benchIO "pureFilterAllIn2" $ withRandomIntIO (filterAllInMPure value)
        , benchIO "pureFilterAllOut2" $ withRandomIntIO (filterAllOutMPure value)
        ]
    ]

o_n_space_monad :: Int -> [Benchmark]
o_n_space_monad value =
    [ bgroup "Monad"
        [ benchIO "toList2" $ withRandomIntIO (toListM value)
        , benchIO "toListSome2" $ withRandomIntIO (toListSome value)
        ]
    ]

{-# INLINE drainConcatFor1 #-}
drainConcatFor1 :: Int -> IO ()
drainConcatFor1 count = withStream count $ \s ->
    drain $ Stream.concatFor s $ \x ->
        Stream.fromPure $ x + 1

{-# INLINE drainConcatFor #-}
drainConcatFor :: Int -> IO ()
drainConcatFor count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.fromPure $ x + y

{-# INLINE drainConcatForM #-}
drainConcatForM :: Int -> IO ()
drainConcatForM count = withStream count $ \s ->
    drain $ do
        Stream.concatForM s $ \x ->
            pure $ Stream.concatForM s $ \y ->
                pure $ Stream.fromPure $ x + y

{-# INLINE drainConcatFor3 #-}
drainConcatFor3 :: Int -> IO ()
drainConcatFor3 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.fromPure $ x + y + z

{-# INLINE drainConcatFor4 #-}
drainConcatFor4 :: Int -> IO ()
drainConcatFor4 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.concatFor s $ \w ->
                        Stream.fromPure $ x + y + z + w

{-# INLINE drainConcatFor5 #-}
drainConcatFor5 :: Int -> IO ()
drainConcatFor5 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.concatFor s $ \w ->
                        Stream.concatFor s $ \u ->
                            Stream.fromPure $ x + y + z + w + u

{-# INLINE drainConcatFor3M #-}
drainConcatFor3M :: Int -> IO ()
drainConcatFor3M count = withStream count $ \s ->
    drain $ do
        Stream.concatForM s $ \x ->
            pure $ Stream.concatForM s $ \y ->
                pure $ Stream.concatForM s $ \z ->
                    pure $ Stream.fromPure $ x + y + z

{-# INLINE filterAllInConcatFor #-}
filterAllInConcatFor :: Int -> IO ()
filterAllInConcatFor count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                let s1 = x + y
                 in if s1 > 0
                    then Stream.fromPure s1
                    else Stream.nil

{-# INLINE filterAllOutConcatFor #-}
filterAllOutConcatFor :: Int -> IO ()
filterAllOutConcatFor count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                let s1 = x + y
                 in if s1 < 0
                    then Stream.fromPure s1
                    else Stream.nil

o_1_space_bind :: Int -> [Benchmark]
o_1_space_bind streamLen =
    [ bgroup "concatFor"
        [ benchIO "drain1" $ drainConcatFor1 streamLen
        , benchIO "drain2" $ drainConcatFor streamLen2
        , benchIO "drain3" $ drainConcatFor3 streamLen3
        , benchIO "drain4" $ drainConcatFor4 streamLen4
        , benchIO "drain5" $ drainConcatFor5 streamLen5
        , benchIO "drainM2" $ drainConcatForM streamLen2
        , benchIO "drainM3" $ drainConcatFor3M streamLen3
        , benchIO "filterAllIn2" $ filterAllInConcatFor streamLen2
        , benchIO "filterAllOut2" $ filterAllOutConcatFor streamLen2
        ]
    ]

    where

    streamLen2 = round (fromIntegral streamLen**(1/2::Double)) -- double nested loop
    streamLen3 = round (fromIntegral streamLen**(1/3::Double)) -- triple nested loop
    streamLen4 = round (fromIntegral streamLen**(1/4::Double)) -- 4 times nested loop
    streamLen5 = round (fromIntegral streamLen**(1/5::Double)) -- 5 times nested loop

-- search space |x| = 1000, |y| = 1000
{-# INLINE boundedInts #-}
boundedInts :: Monad m => Int -> Int -> Stream m Int
boundedInts n _ =
    Stream.interleave
        (Stream.enumerateFromTo (0 :: Int) n)
        (Stream.enumerateFromThenTo (-1) (-2) (-n))

{-# INLINE infiniteInts #-}
infiniteInts :: Monad m => Int -> Int -> Stream m Int
infiniteInts _ _ =
    Stream.interleave
        (Stream.enumerateFrom (0 :: Int))
        (Stream.enumerateFromThen (-1) (-2))

{-# INLINE boundedIntsUnfold #-}
boundedIntsUnfold :: Monad m => Int -> Int -> Unfold m ((), ()) Int
boundedIntsUnfold n _ =
    Unfold.interleave
        (Unfold.supply (0 :: Int, n) Unfold.enumerateFromTo)
        (Unfold.supply (-1, -2, -n) Unfold.enumerateFromThenTo)

{-# INLINE infiniteIntsUnfold #-}
infiniteIntsUnfold :: Monad m => Int -> Int -> Unfold m ((), ()) Int
infiniteIntsUnfold _ _ =
    Unfold.interleave
        (Unfold.supply (0 :: Int) Unfold.enumerateFrom)
        (Unfold.supply (-1, -2) Unfold.enumerateFromThen)

-- In bounded case, the x stream is 0 to maxVal and y stream is -1 to -maxVal.
-- The solution of the equation is x = maxVal y = -maxVal, so in the worst case
-- we get to the solution only after exhausting both the streams.
--
-- In the infinite stream case we terminate after we get to the solution or
-- both streams go beyond maxVal, in this case if one stream is explored more
-- then we might go through more than maxVal x maxVal cases.
--
{-# INLINE checkStream #-}
checkStream :: Applicative m =>
    Int -> Int -> Int -> Stream m (Maybe (Maybe (Int, Int)))
checkStream maxVal x y =
    let eq1 = x + y == 0
        eq2 = x - y == 2 * maxVal
     in if eq1 && eq2
        then Stream.fromPure (Just (Just (x,y)))
        else if abs x > maxVal && abs y > maxVal
        then Stream.fromPure (Just Nothing)
        else Stream.fromPure Nothing

{-# INLINE checkStreamK #-}
checkStreamK :: Int -> Int -> Int -> StreamK.StreamK m (Maybe (Maybe (Int, Int)))
checkStreamK maxVal x y =
    let eq1 = x + y == 0
        eq2 = x - y == 2 * maxVal
     in if eq1 && eq2
        then StreamK.fromPure (Just (Just (x,y)))
        else if abs x > maxVal && abs y > maxVal
        then StreamK.fromPure (Just Nothing)
        else StreamK.fromPure Nothing

{-# INLINE checkPair #-}
checkPair :: Monad m => Int -> (Int, Int) -> m (Maybe (Maybe (Int, Int)))
checkPair maxVal (x, y) =
    let eq1 = x + y == 0
        eq2 = x - y == 2 * maxVal
     in if eq1 && eq2
        then pure (Just (Just (x,y)))
        else if abs x > maxVal && abs y > maxVal
        then pure (Just Nothing)
        else pure Nothing

-- Terminate the stream as soon as we get a Just value
{-# INLINE result #-}
result :: Monad m => Stream m (Maybe a) -> m ()
result = Stream.fold (Fold.take 1 Fold.drain) . Stream.catMaybes

{-# INLINE fairConcatForEqn #-}
fairConcatForEqn :: Monad m => Int -> Stream m Int -> m ()
fairConcatForEqn maxVal input =
    result
        $ Stream.fairConcatFor input $ \x ->
              Stream.fairConcatForM input $ \y -> do
                return $ checkStream maxVal x y

{-# INLINE fairConcatForEqnK #-}
fairConcatForEqnK :: Monad m => Int -> Stream m Int -> m ()
fairConcatForEqnK maxVal input =
    let inputK = StreamK.fromStream input
    in result
        $ StreamK.toStream
        $ StreamK.fairConcatFor inputK $ \x ->
              StreamK.fairConcatForM inputK $ \y -> do
                return $ checkStreamK maxVal x y

{-# INLINE concatForEqn #-}
concatForEqn :: Monad m => Int -> Stream m Int -> m ()
concatForEqn maxVal input =
    result
        $ Stream.concatFor input $ \x ->
              Stream.concatForM input $ \y -> do
                return $ checkStream maxVal x y

{-# INLINE fairSchedForEqn #-}
fairSchedForEqn :: Monad m => Int -> Stream m Int -> m ()
fairSchedForEqn maxVal input =
    result
        $ Stream.fairSchedFor input $ \x ->
              Stream.fairSchedForM input $ \y -> do
                return $ checkStream maxVal x y

_schedForEqn :: Monad m => Int -> Stream m Int -> m ()
_schedForEqn maxVal input =
    result
        $ Stream.schedFor input $ \x ->
              Stream.schedForM input $ \y -> do
                return $ checkStream maxVal x y

{-# INLINE streamCrossEqn #-}
streamCrossEqn :: Monad m => Int -> Stream m Int -> m ()
streamCrossEqn maxVal input =
    result
        $ Stream.mapM (checkPair maxVal)
        $ Stream.cross input input

{-# INLINE fairStreamCrossEqn #-}
fairStreamCrossEqn :: Monad m => Int -> Stream m Int -> m ()
fairStreamCrossEqn maxVal input =
    result
        $ Stream.mapM (checkPair maxVal)
        $ Stream.fairCross input input

{-# INLINE unfoldCrossEqn #-}
unfoldCrossEqn :: Monad m => Int -> Unfold m ((), ()) Int -> m ()
unfoldCrossEqn maxVal input =
    result
        $ Stream.mapM (checkPair maxVal)
        $ Stream.unfold (Unfold.cross input input) (undefined, undefined)

{-# INLINE fairUnfoldCrossEqn #-}
fairUnfoldCrossEqn :: Monad m => Int -> Unfold m ((), ()) Int -> m ()
fairUnfoldCrossEqn maxVal input =
    result
        $ Stream.mapM (checkPair maxVal)
        $ Stream.unfold (Unfold.fairCross input input) (undefined, undefined)

{-# INLINE unfoldEachEqn #-}
unfoldEachEqn :: Monad m => Int -> Unfold m ((), ()) Int -> Stream m Int -> m ()
unfoldEachEqn maxVal input ints =
    let intu = Unfold.carryInput $ Unfold.lmap (const (undefined, undefined)) input
     in result
        $ Stream.mapM (checkPair maxVal)
        $ Stream.unfoldEach intu ints

{-# INLINE fairUnfoldEachEqn #-}
fairUnfoldEachEqn :: Monad m => Int -> Unfold m ((), ()) Int -> Stream m Int -> m ()
fairUnfoldEachEqn maxVal input ints =
    let intu = Unfold.carryInput $ Unfold.lmap (const (undefined, undefined)) input
     in result
        $ Stream.mapM (checkPair maxVal)
        $ Stream.fairUnfoldEach intu ints

{-# INLINE unfoldSchedEqn #-}
unfoldSchedEqn :: Monad m => Int -> Unfold m ((), ()) Int -> Stream m Int -> m ()
unfoldSchedEqn maxVal input ints =
    let intu = Unfold.carryInput $ Unfold.lmap (const (undefined, undefined)) input
     in result
        $ Stream.mapM (checkPair maxVal)
        $ Stream.unfoldSched intu ints

{-# INLINE fairUnfoldSchedEqn #-}
fairUnfoldSchedEqn :: Monad m => Int -> Unfold m ((), ()) Int -> Stream m Int -> m ()
fairUnfoldSchedEqn maxVal input ints =
    let intu = Unfold.carryInput $ Unfold.lmap (const (undefined, undefined)) input
     in result
        $ Stream.mapM (checkPair maxVal)
        $ Stream.fairUnfoldSched intu ints

concatForBounded :: Int -> IO ()
concatForBounded maxVal = withRandomIntIO $ \n ->
    concatForEqn maxVal (boundedInts maxVal n)

fairConcatForBounded :: Int -> IO ()
fairConcatForBounded maxVal = withRandomIntIO $ \n ->
    fairConcatForEqn maxVal (boundedInts maxVal n)

fairConcatForKBounded :: Int -> IO ()
fairConcatForKBounded maxVal = withRandomIntIO $ \n ->
    fairConcatForEqnK maxVal (boundedInts maxVal n)

fairConcatForInfinite :: Int -> IO ()
fairConcatForInfinite maxVal = withRandomIntIO $ \n ->
    fairConcatForEqn maxVal (infiniteInts maxVal n)

fairSchedForBounded :: Int -> IO ()
fairSchedForBounded maxVal = withRandomIntIO $ \n ->
    fairSchedForEqn maxVal (boundedInts maxVal n)

fairSchedForInfinite :: Int -> IO ()
fairSchedForInfinite maxVal = withRandomIntIO $ \n ->
    fairSchedForEqn maxVal (infiniteInts maxVal n)

streamCrossBounded :: Int -> IO ()
streamCrossBounded maxVal = withRandomIntIO $ \n ->
    streamCrossEqn maxVal (boundedInts maxVal n)

fairStreamCrossBounded :: Int -> IO ()
fairStreamCrossBounded maxVal = withRandomIntIO $ \n ->
    fairStreamCrossEqn maxVal (boundedInts maxVal n)

fairStreamCrossInfinite :: Int -> IO ()
fairStreamCrossInfinite maxVal = withRandomIntIO $ \n ->
    fairStreamCrossEqn maxVal (infiniteInts maxVal n)

unfoldCrossBounded :: Int -> IO ()
unfoldCrossBounded maxVal = unfoldCrossEqn maxVal (boundedIntsUnfold maxVal 0)

fairUnfoldCrossBounded :: Int -> IO ()
fairUnfoldCrossBounded maxVal = fairUnfoldCrossEqn maxVal (boundedIntsUnfold maxVal 0)

fairUnfoldCrossInfinite :: Int -> IO ()
fairUnfoldCrossInfinite maxVal = fairUnfoldCrossEqn maxVal (infiniteIntsUnfold maxVal 0)

unfoldEachBounded :: Int -> IO ()
unfoldEachBounded maxVal = withRandomIntIO $ \n ->
    unfoldEachEqn maxVal (boundedIntsUnfold maxVal 0) (boundedInts maxVal n)

fairUnfoldEachBounded :: Int -> IO ()
fairUnfoldEachBounded maxVal = withRandomIntIO $ \n ->
    fairUnfoldEachEqn maxVal (boundedIntsUnfold maxVal 0) (boundedInts maxVal n)

fairUnfoldEachInfinite :: Int -> IO ()
fairUnfoldEachInfinite maxVal = withRandomIntIO $ \n ->
    fairUnfoldEachEqn maxVal (infiniteIntsUnfold maxVal 0) (infiniteInts maxVal n)

unfoldSchedBounded :: Int -> IO ()
unfoldSchedBounded maxVal = withRandomIntIO $ \n ->
    unfoldSchedEqn maxVal (boundedIntsUnfold maxVal 0) (boundedInts maxVal n)

fairUnfoldSchedBounded :: Int -> IO ()
fairUnfoldSchedBounded maxVal = withRandomIntIO $ \n ->
    fairUnfoldSchedEqn maxVal (boundedIntsUnfold maxVal 0) (boundedInts maxVal n)

fairUnfoldSchedInfinite :: Int -> IO ()
fairUnfoldSchedInfinite maxVal = withRandomIntIO $ \n ->
    fairUnfoldSchedEqn maxVal (infiniteIntsUnfold maxVal 0) (infiniteInts maxVal n)

-- Solve simultaneous equations by exploring all possibilities
o_1_space_equations :: Int -> [Benchmark]
o_1_space_equations value =
    [ bgroup "equations"
        [ benchIO "concatFor (bounded)" $ concatForBounded sqrtVal
        , benchIO "fairConcatFor (bounded)" $ fairConcatForBounded sqrtVal
        , benchIO "fairConcatForK (bounded)" $ fairConcatForKBounded sqrtVal
        , benchIO "fairConcatFor (infinite)" $ fairConcatForInfinite sqrtVal
        , benchIO "fairSchedFor (bounded)" $ fairSchedForBounded sqrtVal
        , benchIO "fairSchedFor (infinite)" $ fairSchedForInfinite sqrtVal
        , benchIO "streamCross (bounded)" $ streamCrossBounded sqrtVal
        , benchIO "fairStreamCross (bounded)" $ fairStreamCrossBounded sqrtVal
        , benchIO "fairStreamCross (infinite)" $ fairStreamCrossInfinite sqrtVal
        , benchIO "unfoldCross (bounded)" $ unfoldCrossBounded sqrtVal
        , benchIO "fairUnfoldCross (bounded)" $ fairUnfoldCrossBounded sqrtVal
        , benchIO "fairUnfoldCross (infinite)" $ fairUnfoldCrossInfinite sqrtVal
        , benchIO "unfoldEach (bounded)" $ unfoldEachBounded sqrtVal
        , benchIO "fairUnfoldEach (bounded)" $ fairUnfoldEachBounded sqrtVal
        , benchIO "fairUnfoldEach (infinite)" $ fairUnfoldEachInfinite sqrtVal
        , benchIO "unfoldSched (bounded)" $ unfoldSchedBounded sqrtVal
        , benchIO "fairUnfoldSched (bounded)" $ fairUnfoldSchedBounded sqrtVal
        , benchIO "fairUnfoldSched (infinite)" $ fairUnfoldSchedInfinite sqrtVal
        ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)

-------------------------------------------------------------------------------
-- Joining
-------------------------------------------------------------------------------

{-
toKv :: Int -> (Int, Int)
toKv p = (p, p)

{-# INLINE joinWith #-}
joinWith :: Common.MonadAsync m =>
       ((Int -> Int -> Bool) -> Stream m Int -> Stream m Int -> Stream m b)
    -> Int
    -> Int
    -> m ()
joinWith j val i =
    drain $ j (==) (sourceUnfoldrM val i) (sourceUnfoldrM val (val `div` 2))

{-# INLINE joinMapWith #-}
joinMapWith :: Common.MonadAsync m =>
       (Stream m (Int, Int) -> Stream m (Int, Int) -> Stream m b)
    -> Int
    -> Int
    -> m ()
joinMapWith j val i =
    drain
        $ j
            (fmap toKv (sourceUnfoldrM val i))
            (fmap toKv (sourceUnfoldrM val (val `div` 2)))

o_n_heap_buffering :: Int -> [Benchmark]
o_n_heap_buffering value =
    [ bgroup "buffered"
        [
          benchIOSrc1 "joinInnerGeneric (sqrtVal)"
            $ joinWith S.joinInnerGeneric sqrtVal
        , benchIOSrc1 "joinInner"
            $ joinMapWith S.joinInner halfVal
        , benchIOSrc1 "joinLeftGeneric (sqrtVal)"
            $ joinWith S.joinLeftGeneric sqrtVal
        , benchIOSrc1 "joinLeft "
            $ joinMapWith S.joinLeft halfVal
        , benchIOSrc1 "joinOuterGeneric (sqrtVal)"
            $ joinWith S.joinOuterGeneric sqrtVal
        , benchIOSrc1 "joinOuter"
            $ joinMapWith S.joinOuter halfVal
        , benchIOSrc1 "filterInStreamGenericBy (sqrtVal)"
            $ joinWith S.filterInStreamGenericBy sqrtVal
        , benchIOSrc1 "filterInStreamAscBy"
            $ joinMapWith (S.filterInStreamAscBy compare) halfVal
        -- Note: schedFor does a bfs scheduling, therefore, can take a lot of
        -- memory.
        , benchFold "schedFor (bounded)" schedForEqn (boundedInts 1000)
        ]
    ]

    where

    halfVal = value `div` 2
    sqrtVal = round $ sqrt (fromIntegral value :: Double)
-}

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
{-# ANN benchmarks "HLint: ignore" #-}
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    map (SpaceO_1,) (Prelude.concat
        [
        -- multi-stream
          o_1_space_joining size
        , o_1_space_concat size
        , o_1_space_applicative size
        , o_1_space_monad size
        , o_1_space_bind size
        , o_1_space_equations size
        ])
    ++ map (SpaceO_n,) (Prelude.concat
        [
        -- multi-stream
          o_n_space_monad size
        ])
    ++ map (HeapO_n,)
    {-
        -- multi-stream
        (o_n_heap_buffering size)
    -}
        (o_n_heap_concat size)
