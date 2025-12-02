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

import Test.Tasty.Bench
import Stream.Common
import Streamly.Benchmark.Common
import Prelude hiding (concatMap, zipWith)

-------------------------------------------------------------------------------
-- Multi-Stream
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Appending
-------------------------------------------------------------------------------

{-# INLINE serial2 #-}
serial2 :: Int -> Int -> IO ()
serial2 count n =
    drain $
        Common.append
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1))

{-# INLINE serial4 #-}
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

{-# INLINE interleave2 #-}
interleave2 :: Int -> Int -> IO ()
interleave2 count n =
    drain $
        S.interleave
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'interleave2
inspect $ 'interleave2 `hasNoType` ''SPEC
inspect $ 'interleave2 `hasNoType` ''S.InterleaveState
#endif

{-# INLINE roundRobin2 #-}
roundRobin2 :: Int -> Int -> IO ()
roundRobin2 value n =
    S.drain $
    S.roundRobin
        (sourceUnfoldrM (value `div` 2) n)
        (sourceUnfoldrM (value `div` 2) (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'roundRobin2
inspect $ 'roundRobin2 `hasNoType` ''SPEC
inspect $ 'roundRobin2 `hasNoType` ''S.InterleaveState
#endif

-------------------------------------------------------------------------------
-- Merging
-------------------------------------------------------------------------------

{-# INLINE mergeWith #-}
mergeWith ::
    (  (Int -> Int -> Ordering)
    -> Stream IO Int
    -> Stream IO Int
    -> Stream IO Int
    )
    -> (Int -> Int -> Ordering)
    -> Int -> Int -> IO ()
mergeWith g cmp count n =
    Stream.drain
        $ g
            cmp
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1))

{-# INLINE mergeWithM #-}
mergeWithM ::
    (  (Int -> Int -> IO Ordering)
    -> Stream IO Int
    -> Stream IO Int
    -> Stream IO Int
    )
    -> (Int -> Int -> Ordering)
    -> Int -> Int -> IO ()
mergeWithM g cmp count n =
    Stream.drain
        $ g
            (\a b -> return $ cmp a b)
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1))

{-# INLINE mergeBy #-}
mergeBy :: (Int -> Int -> Ordering) -> Int -> Int -> IO ()
mergeBy = mergeWith Stream.mergeBy

{-# INLINE mergeByM #-}
mergeByM :: (Int -> Int -> Ordering) -> Int -> Int -> IO ()
mergeByM = mergeWithM Stream.mergeByM

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mergeBy
inspect $ 'mergeBy `hasNoType` ''SPEC
-- inspect $ 'mergeBy `hasNoType` ''S.Step

inspect $ hasNoTypeClasses 'mergeByM
inspect $ 'mergeByM `hasNoType` ''SPEC
-- inspect $ 'mergeByM `hasNoType` ''S.Step
#endif

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

{-# INLINE zipWith #-}
zipWith :: Monad m => Stream m Int -> m ()
zipWith src = drain $ S.zipWith (,) src src

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'zipWith
inspect $ 'zipWith `hasNoType` ''SPEC
-- inspect $ 'zipWith `hasNoType` ''S.Step
#endif

{-# INLINE zipWithM #-}
zipWithM :: Monad m => Stream m Int -> m ()
zipWithM src = drain $ S.zipWithM (curry return) src src

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'zipWithM
inspect $ 'zipWithM `hasNoType` ''SPEC
-- inspect $ 'zipWithM `hasNoType` ''S.Step
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
bfsUnfoldEach :: Int -> Int -> Int -> IO ()
bfsUnfoldEach outer inner n =
    S.drain $ S.bfsUnfoldEach
        -- (UF.lmap return (UF.replicateM inner))
        (UF.lmap (\x -> (x,x)) (sourceUnfoldrMUF inner))
        (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'bfsUnfoldEach
-- inspect $ 'bfsUnfoldEach `hasNoType` ''SPEC
-- inspect $ 'bfsUnfoldEach `hasNoType`
--      ''S.ConcatUnfoldInterleaveState
#endif

{-# INLINE unfoldSched #-}
unfoldSched :: Int -> Int -> Int -> IO ()
unfoldSched outer inner n =
    S.drain $ S.unfoldSched
        -- (UF.lmap return (UF.replicateM inner))
        (UF.lmap (\x -> (x,x)) (sourceUnfoldrMUF inner))
        (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldSched
-- inspect $ 'unfoldSched `hasNoType` ''SPEC
-- inspect $ 'unfoldSched `hasNoType`
--      ''Stream.ConcatUnfoldInterleaveState
#endif

o_1_space_joining :: Int -> [Benchmark]
o_1_space_joining value =
    [ bgroup "joining (2 of n/2)"
        [ benchIOSrc1 "serial" (serial2 (value `div` 2))
        , benchIOSrc1 "serial (2,2,x/4)" (serial4 (value `div` 4))
        , benchIOSrc1 "interleave" (interleave2 (value `div` 2))
        , benchIOSrc1 "roundRobin" (roundRobin2 (value `div` 2))
        , benchIOSrc1
            "mergeBy compare"
            (mergeBy compare (value `div` 2))
        , benchIOSrc1
            "mergeByM compare"
            (mergeByM compare (value `div` 2))
        , benchIOSrc1
            "mergeBy (flip compare)"
            (mergeBy (flip compare) (value `div` 2))
        , benchIOSrc1
            "mergeByM (flip compare)"
            (mergeByM (flip compare) (value `div` 2))

        , benchFold "zipWith" zipWith (sourceUnfoldrM value)
        , benchFold "zipWithM" zipWithM (sourceUnfoldrM value)

        -- join 2 streams using n-ary ops
        , benchIOSrc1 "bfsUnfoldEach" (bfsUnfoldEach 2 (value `div` 2))
        , benchIOSrc1 "unfoldSched" (unfoldSched 2 (value `div` 2))
        , benchIOSrc1 "concatMap" (concatMap 2 (value `div` 2))
        ]
    ]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

-- concatMap unfoldrM/unfoldrM

{-# INLINE concatMap #-}
concatMap :: Int -> Int -> Int -> IO ()
concatMap outer inner n =
    drain $ S.concatMap
        (\_ -> sourceUnfoldrM inner n)
        (sourceUnfoldrM outer n)

{-# INLINE concatMapViaUnfoldEach #-}
concatMapViaUnfoldEach :: Int -> Int -> Int -> IO ()
concatMapViaUnfoldEach outer inner n =
    drain $ cmap
        (\_ -> sourceUnfoldrM inner n)
        (sourceUnfoldrM outer n)

    where

    cmap f = Stream.unfoldEach (UF.lmap f UF.fromStream)

{-# INLINE concatMapM #-}
concatMapM :: Int -> Int -> Int -> IO ()
concatMapM outer inner n =
    drain $ S.concatMapM
        (\_ -> return $ sourceUnfoldrM inner n)
        (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMap
inspect $ 'concatMap `hasNoType` ''SPEC
#endif

-- concatMap unfoldr/unfoldr

{-# INLINE concatMapPure #-}
concatMapPure :: Int -> Int -> Int -> IO ()
concatMapPure outer inner n =
    drain $ S.concatMap
        (\_ -> sourceUnfoldr inner n)
        (sourceUnfoldr outer n)

#ifdef INSPECTION
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'concatMapPure [''Applicative]
#else
inspect $ hasNoTypeClasses 'concatMapPure
#endif
inspect $ 'concatMapPure `hasNoType` ''SPEC
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
unfoldEach :: Int -> Int -> Int -> IO ()
unfoldEach outer inner start = drain $
     -- XXX the replicateM takes much more time compared to unfoldrM, is there
     -- a perf issue or this is just because of accessing outer loop variables?
     -- S.unfoldEach (UF.lmap ((inner,) . return) UF.replicateM)
     S.unfoldEach (sourceUnfoldrMUnfold inner start)
        $ sourceUnfoldrM outer start

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldEach
inspect $ 'unfoldEach `hasNoType` ''Stream.UnfoldEachState
inspect $ 'unfoldEach `hasNoType` ''SPEC
#endif

{-# INLINE unfoldEach2 #-}
unfoldEach2 :: Int -> Int -> Int -> IO ()
unfoldEach2 outer inner start = drain $
     S.unfoldEach (UF.carry (sourceUnfoldrMUnfold inner start))
        $ sourceUnfoldrM outer start

{-# INLINE unfoldEach3 #-}
unfoldEach3 :: Int -> Int -> IO ()
unfoldEach3 linearCount start = drain $ do
    S.unfoldEach (UF.carry (UF.lmap snd (sourceUnfoldrMUnfold nestedCount3 start)))
         $ S.unfoldEach (UF.carry (sourceUnfoldrMUnfold nestedCount3 start))
            $ sourceUnfoldrM nestedCount3 start
    where

    nestedCount3 = round (fromIntegral linearCount**(1/3::Double))

{-# INLINE unfoldCross #-}
unfoldCross :: Int -> Int -> Int -> IO ()
unfoldCross outer inner start = drain $
    Stream.unfoldCross
        UF.identity
        (sourceUnfoldrM outer start)
        (sourceUnfoldrM inner start)

o_1_space_concat :: Int -> [Benchmark]
o_1_space_concat value = sqrtVal `seq`
    [ bgroup "concat"
        [ benchIOSrc1 "concatMapPure outer=Max inner=1"
            (concatMapPure value 1)
        , benchIOSrc1 "concatMapPure outer=inner=(sqrt Max)"
            (concatMapPure sqrtVal sqrtVal)
        , benchIOSrc1 "concatMapPure outer=1 inner=Max"
            (concatMapPure 1 value)

        , benchIOSrc1 "concatMap outer=max inner=1"
            (concatMap value 1)
        , benchIOSrc1 "concatMap outer=inner=(sqrt Max)"
            (concatMap sqrtVal sqrtVal)
        , benchIOSrc1 "concatMap outer=1 inner=Max"
            (concatMap 1 value)

        -- This is for comparison with foldMapWith
        , benchIOSrc "concatMapId outer=max inner=1 (fromFoldable)"
            (S.concatMap id . sourceConcatMapId value)

        , benchIOSrc1 "concatMapM outer=max inner=1"
            (concatMapM value 1)
        , benchIOSrc1 "concatMapM outer=inner=(sqrt Max)"
            (concatMapM sqrtVal sqrtVal)
        , benchIOSrc1 "concatMapM outer=1 inner=Max"
            (concatMapM 1 value)

        , benchIOSrc1 "concatMapViaUnfoldEach outer=max inner=1"
            (concatMapViaUnfoldEach value 1)
        , benchIOSrc1 "concatMapViaUnfoldEach outer=inner=(sqrt Max)"
            (concatMapViaUnfoldEach sqrtVal sqrtVal)
        , benchIOSrc1 "concatMapViaUnfoldEach outer=1 inner=Max"
            (concatMapViaUnfoldEach 1 value)

        , benchIOSrc1 "unfoldCross outer=max inner=1"
            (unfoldCross value 1)
        , benchIOSrc1 "unfoldCross outer=inner=(sqrt Max)"
            (unfoldCross sqrtVal sqrtVal)
        , benchIOSrc1 "unfoldCross outer=1 inner=Max"
            (unfoldCross 1 value)

        -- concatMap vs unfoldEach
        , benchIOSrc1 "unfoldEach outer=Max inner=1"
            (unfoldEach value 1)
        , benchIOSrc1 "unfoldEach outer=inner=(sqrt Max)"
            (unfoldEach sqrtVal sqrtVal)
        , benchIOSrc1 "unfoldEach outer=1 inner=Max"
            (unfoldEach 1 value)

        , benchIOSrc1 "unfoldEach2 outer=Max inner=1"
            (unfoldEach2 value 1)
        , benchIOSrc1 "unfoldEach2 outer=inner=(sqrt Max)"
            (unfoldEach2 sqrtVal sqrtVal)
        , benchIOSrc1 "unfoldEach2 outer=1 inner=Max"
            (unfoldEach2 1 value)

        , benchIOSrc1 "unfoldEach3 outer=inner=(cubert Max)"
            (unfoldEach3 value)
        ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)

o_n_heap_concat :: Int -> [Benchmark]
o_n_heap_concat value = sqrtVal `seq`
    [ bgroup "concat"
        [
          benchIOSrc1
              "bfsUnfoldEach (n of 1)"
              (bfsUnfoldEach value 1)
        , benchIOSrc1
              "bfsUnfoldEach (sqrtVal of sqrtVal)"
              (bfsUnfoldEach sqrtVal sqrtVal)

        , benchIOSrc1
              "unfoldSched (n of 1)"
              (unfoldSched value 1)
        , benchIOSrc1
              "unfoldSched (sqrtVal of sqrtVal)"
              (unfoldSched sqrtVal sqrtVal)

        ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)

-------------------------------------------------------------------------------
-- Applicative
-------------------------------------------------------------------------------

{-# INLINE cross2 #-}
cross2 :: MonadAsync m => Int -> Int -> m ()
cross2 linearCount start = drain $
    Stream.crossWith (+)
        (sourceUnfoldr nestedCount2 start)
        (sourceUnfoldr nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

o_1_space_applicative :: Int -> [Benchmark]
o_1_space_applicative value =
    [ bgroup "Applicative"
        [ benchIO "(*>)" $ apDiscardFst value
        , benchIO "(<*)" $ apDiscardSnd value
        , benchIO "(<*>)" $ toNullAp value
        , benchIO "liftA2" $ apLiftA2 value
        , benchIO "pureDrain2" $ toNullApPure value
        , benchIO "pureCross2" $ cross2 value
        ]
    ]

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

o_1_space_monad :: Int -> [Benchmark]
o_1_space_monad value =
    [ bgroup "Monad"
        [ benchIO "then2" $ monadThen value
        , benchIO "drain2" $ toNullM value
        , benchIO "drain3" $ toNullM3 value
        , benchIO "filterAllOut2" $ filterAllOutM value
        , benchIO "filterAllIn2" $ filterAllInM value
        , benchIO "filterSome2" $ filterSome value
        , benchIO "breakAfterSome2" $ breakAfterSome value
        , benchIO "pureDrain2" $ toNullMPure value
        , benchIO "pureDrain3" $ toNullM3Pure value
        , benchIO "pureFilterAllIn2" $ filterAllInMPure value
        , benchIO "pureFilterAllOut2" $ filterAllOutMPure value
        ]
    ]

o_n_space_monad :: Int -> [Benchmark]
o_n_space_monad value =
    [ bgroup "Monad"
        [ benchIO "toList2" $ toListM value
        , benchIO "toListSome2" $ toListSome value
        ]
    ]

{-# INLINE drainConcatFor1 #-}
drainConcatFor1 :: Monad m => Stream m Int -> m ()
drainConcatFor1 s = drain $ do
    Stream.concatFor s $ \x ->
            Stream.fromPure $ x + 1

{-# INLINE drainConcatFor #-}
drainConcatFor :: Monad m => Stream m Int -> m ()
drainConcatFor s = drain $ do
    Stream.concatFor s $ \x ->
        Stream.concatFor s $ \y ->
            Stream.fromPure $ x + y

{-# INLINE drainConcatForM #-}
drainConcatForM :: Monad m => Stream m Int -> m ()
drainConcatForM s = drain $ do
    Stream.concatForM s $ \x ->
        pure $ Stream.concatForM s $ \y ->
            pure $ Stream.fromPure $ x + y

{-# INLINE drainConcatFor3 #-}
drainConcatFor3 :: Monad m => Stream m Int -> m ()
drainConcatFor3 s = drain $ do
    Stream.concatFor s $ \x ->
        Stream.concatFor s $ \y ->
            Stream.concatFor s $ \z ->
                Stream.fromPure $ x + y + z

{-# INLINE drainConcatFor4 #-}
drainConcatFor4 :: Monad m => Stream m Int -> m ()
drainConcatFor4 s = drain $ do
    Stream.concatFor s $ \x ->
        Stream.concatFor s $ \y ->
            Stream.concatFor s $ \z ->
                Stream.concatFor s $ \w ->
                    Stream.fromPure $ x + y + z + w

{-# INLINE drainConcatFor5 #-}
drainConcatFor5 :: Monad m => Stream m Int -> m ()
drainConcatFor5 s = drain $ do
    Stream.concatFor s $ \x ->
        Stream.concatFor s $ \y ->
            Stream.concatFor s $ \z ->
                Stream.concatFor s $ \w ->
                    Stream.concatFor s $ \u ->
                        Stream.fromPure $ x + y + z + w + u

{-# INLINE drainConcatFor3M #-}
drainConcatFor3M :: Monad m => Stream m Int -> m ()
drainConcatFor3M s = drain $ do
    Stream.concatForM s $ \x ->
        pure $ Stream.concatForM s $ \y ->
            pure $ Stream.concatForM s $ \z ->
                pure $ Stream.fromPure $ x + y + z

{-# INLINE filterAllInConcatFor #-}
filterAllInConcatFor
    :: Monad m
    => Stream m Int -> m ()
filterAllInConcatFor s = drain $ do
    Stream.concatFor s $ \x ->
        Stream.concatFor s $ \y ->
            let s1 = x + y
             in if s1 > 0
                then Stream.fromPure s1
                else Stream.nil

{-# INLINE filterAllOutConcatFor #-}
filterAllOutConcatFor
    :: Monad m
    => Stream m Int -> m ()
filterAllOutConcatFor s = drain $ do
    Stream.concatFor s $ \x ->
        Stream.concatFor s $ \y ->
            let s1 = x + y
             in if s1 < 0
                then Stream.fromPure s1
                else Stream.nil

o_1_space_bind :: Int -> [Benchmark]
o_1_space_bind streamLen =
    [ bgroup "concatFor"
        [ benchFold "drain1" drainConcatFor1   (sourceUnfoldrM streamLen)
        , benchFold "drain2" drainConcatFor   (sourceUnfoldrM streamLen2)
        , benchFold "drain3" drainConcatFor3   (sourceUnfoldrM streamLen3)
        , benchFold "drain4" drainConcatFor4   (sourceUnfoldrM streamLen4)
        , benchFold "drain5" drainConcatFor5   (sourceUnfoldrM streamLen5)
        , benchFold "drainM2" drainConcatForM   (sourceUnfoldrM streamLen2)
        , benchFold "drainM3" drainConcatFor3M   (sourceUnfoldrM streamLen3)
        , benchFold "filterAllIn2"  filterAllInConcatFor  (sourceUnfoldrM streamLen2)
        , benchFold "filterAllOut2" filterAllOutConcatFor (sourceUnfoldrM streamLen2)
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

{-# INLINE checkStream #-}
checkStream :: Applicative m =>
    Int -> Int -> Stream m (Maybe (Maybe (Int, Int)))
checkStream x y =
    let eq1 = x + y == 0
        eq2 = x - y == 1994
     in if eq1 && eq2
        then Stream.fromPure (Just (Just (x,y)))
        else if abs x > 1000 && abs y > 1000
        then Stream.fromPure (Just Nothing)
        else Stream.fromPure Nothing

{-# INLINE checkStreamK #-}
checkStreamK :: Int -> Int -> StreamK.StreamK m (Maybe (Maybe (Int, Int)))
checkStreamK x y =
    let eq1 = x + y == 0
        eq2 = x - y == 1994
     in if eq1 && eq2
        then StreamK.fromPure (Just (Just (x,y)))
        else if abs x > 1000 && abs y > 1000
        then StreamK.fromPure (Just Nothing)
        else StreamK.fromPure Nothing

{-# INLINE checkPair #-}
checkPair :: Monad m => (Int, Int) -> m (Maybe (Maybe (Int, Int)))
checkPair (x, y) =
    let eq1 = x + y == 0
        eq2 = x - y == 1994
     in if eq1 && eq2
        then pure (Just (Just (x,y)))
        else if abs x > 1000 && abs y > 1000
        then pure (Just Nothing)
        else pure Nothing

result :: Monad m => Stream m (Maybe a) -> m ()
result = Stream.fold (Fold.take 1 Fold.drain) . Stream.catMaybes

fairConcatForEqn :: Monad m => Stream m Int -> m ()
fairConcatForEqn input =
    result
        $ Stream.fairConcatFor input $ \x ->
              Stream.fairConcatForM input $ \y -> do
                return $ checkStream x y

fairConcatForEqnK :: Monad m => Stream m Int -> m ()
fairConcatForEqnK input =
    let inputK = StreamK.fromStream input
    in result
        $ StreamK.toStream
        $ StreamK.fairConcatFor inputK $ \x ->
              StreamK.fairConcatForM inputK $ \y -> do
                return $ checkStreamK x y

concatForEqn :: Monad m => Stream m Int -> m ()
concatForEqn input =
    result
        $ Stream.concatFor input $ \x ->
              Stream.concatForM input $ \y -> do
                return $ checkStream x y

fairSchedForEqn :: Monad m => Stream m Int -> m ()
fairSchedForEqn input =
    result
        $ Stream.fairSchedFor input $ \x ->
              Stream.fairSchedForM input $ \y -> do
                return $ checkStream x y

_schedForEqn :: Monad m => Stream m Int -> m ()
_schedForEqn input =
    result
        $ Stream.schedFor input $ \x ->
              Stream.schedForM input $ \y -> do
                return $ checkStream x y

streamCrossEqn :: Monad m => Stream m Int -> m ()
streamCrossEqn input =
    result
        $ Stream.mapM checkPair
        $ Stream.cross input input

fairStreamCrossEqn :: Monad m => Stream m Int -> m ()
fairStreamCrossEqn input =
    result
        $ Stream.mapM checkPair
        $ Stream.fairCross input input

unfoldCrossEqn :: Monad m => Unfold m ((), ()) Int -> m ()
unfoldCrossEqn input =
    result
        $ Stream.mapM checkPair
        $ Stream.unfold (Unfold.cross input input) (undefined, undefined)

fairUnfoldCrossEqn :: Monad m => Unfold m ((), ()) Int -> m ()
fairUnfoldCrossEqn input =
    result
        $ Stream.mapM checkPair
        $ Stream.unfold (Unfold.fairCross input input) (undefined, undefined)

unfoldEachEqn :: Monad m => Unfold m ((), ()) Int -> Stream m Int -> m ()
unfoldEachEqn input ints =
    let intu = Unfold.carry $ Unfold.lmap (const (undefined, undefined)) input
     in result
        $ Stream.mapM checkPair
        $ Stream.unfoldEach intu ints

fairUnfoldEachEqn :: Monad m => Unfold m ((), ()) Int -> Stream m Int -> m ()
fairUnfoldEachEqn input ints =
    let intu = Unfold.carry $ Unfold.lmap (const (undefined, undefined)) input
     in result
        $ Stream.mapM checkPair
        $ Stream.fairUnfoldEach intu ints

unfoldSchedEqn :: Monad m => Unfold m ((), ()) Int -> Stream m Int -> m ()
unfoldSchedEqn input ints =
    let intu = Unfold.carry $ Unfold.lmap (const (undefined, undefined)) input
     in result
        $ Stream.mapM checkPair
        $ Stream.unfoldSched intu ints

fairUnfoldSchedEqn :: Monad m => Unfold m ((), ()) Int -> Stream m Int -> m ()
fairUnfoldSchedEqn input ints =
    let intu = Unfold.carry $ Unfold.lmap (const (undefined, undefined)) input
     in result
        $ Stream.mapM checkPair
        $ Stream.fairUnfoldSched intu ints

-- Solve simultaneous equations by exploring all possibilities
o_1_space_equations :: Int -> [Benchmark]
o_1_space_equations _ =
    [ bgroup "equations"
        [ benchFold "concatFor (bounded)" concatForEqn (boundedInts 1000)
        , benchFold "fairConcatFor (bounded)"
            fairConcatForEqn (boundedInts 1000)
        , benchFold "fairConcatForK (bounded)"
            fairConcatForEqnK (boundedInts 1000)
        , benchFold "fairConcatFor (infinite)"
            fairConcatForEqn (infiniteInts 1000)
        , benchFold "fairSchedFor (bounded)"
            fairSchedForEqn (boundedInts 1000)
        , benchFold "fairSchedFor (infinite)"
            fairSchedForEqn (infiniteInts 1000)
        , benchFold "streamCross (bounded)"
            streamCrossEqn (boundedInts 1000)
        , benchFold "fairStreamCross (bounded)"
            fairStreamCrossEqn (boundedInts 1000)
        , benchFold "fairStreamCross (infinite)"
            fairStreamCrossEqn (infiniteInts 1000)
        , bench "unfoldCross (bounded)"
            $ nfIO $ unfoldCrossEqn (boundedIntsUnfold 1000 0)
        , bench "fairUnfoldCross (bounded)"
            $ nfIO $ fairUnfoldCrossEqn (boundedIntsUnfold 1000 0)
        , bench "fairUnfoldCross (infinite)"
            $ nfIO $ fairUnfoldCrossEqn (infiniteIntsUnfold 1000 0)
        , benchFold "unfoldEach (bounded)"
            (unfoldEachEqn (boundedIntsUnfold 1000 0)) (boundedInts 1000)
        , benchFold "fairUnfoldEach (bounded)"
            (fairUnfoldEachEqn (boundedIntsUnfold 1000 0)) (boundedInts 1000)
        , benchFold "fairUnfoldEach (infinite)"
            (fairUnfoldEachEqn (infiniteIntsUnfold 1000 0)) (infiniteInts 1000)
        , benchFold "unfoldSched (bounded)"
            (unfoldSchedEqn (boundedIntsUnfold 1000 0)) (boundedInts 1000)
        , benchFold "fairUnfoldSched (bounded)"
            (fairUnfoldSchedEqn (boundedIntsUnfold 1000 0)) (boundedInts 1000)
        , benchFold "fairUnfoldSched (infinite)"
            (fairUnfoldSchedEqn (infiniteIntsUnfold 1000 0)) (infiniteInts 1000)
        ]
    ]

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
benchmarks :: String -> Int -> [Benchmark]
benchmarks moduleName size =
        [ bgroup (o_1_space_prefix moduleName) $ Prelude.concat
            [
            -- multi-stream
              o_1_space_joining size
            , o_1_space_concat size
            , o_1_space_applicative size
            , o_1_space_monad size
            , o_1_space_bind size
            , o_1_space_equations size
            ]
        , bgroup (o_n_space_prefix moduleName) $ Prelude.concat
            [
            -- multi-stream
              o_n_space_monad size
            ]
       , bgroup (o_n_heap_prefix moduleName) $
       {-
            -- multi-stream
            o_n_heap_buffering size
       -}
            (o_n_heap_concat size)
        ]
