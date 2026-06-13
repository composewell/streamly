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

module Stream.Nesting (benchmarks) where

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Producer as Producer
import Test.Inspection
#endif

import Streamly.Data.Stream (Stream)
import Streamly.Data.Unfold (Unfold)

import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.StreamK as StreamK

import Test.Tasty.Bench
import Stream.Common hiding (benchIO)
import Stream.Type (benchIO, withRandomIntIO)
import Streamly.Benchmark.Common
import qualified Stream.Type as Type
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
-- Monad
-------------------------------------------------------------------------------

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

{-# INLINE fairConcatForEqn #-}
fairConcatForEqn :: Monad m => Int -> Stream m Int -> m ()
fairConcatForEqn maxVal input =
    Type.result
        $ Stream.fairConcatFor input $ \x ->
              Stream.fairConcatForM input $ \y -> do
                return $ Type.checkStream maxVal x y

{-# INLINE fairConcatForEqnK #-}
fairConcatForEqnK :: Monad m => Int -> Stream m Int -> m ()
fairConcatForEqnK maxVal input =
    let inputK = StreamK.fromStream input
    in Type.result
        $ StreamK.toStream
        $ StreamK.fairConcatFor inputK $ \x ->
              StreamK.fairConcatForM inputK $ \y -> do
                return $ checkStreamK maxVal x y

{-# INLINE fairSchedForEqn #-}
fairSchedForEqn :: Monad m => Int -> Stream m Int -> m ()
fairSchedForEqn maxVal input =
    Type.result
        $ Stream.fairSchedFor input $ \x ->
              Stream.fairSchedForM input $ \y -> do
                return $ Type.checkStream maxVal x y

_schedForEqn :: Monad m => Int -> Stream m Int -> m ()
_schedForEqn maxVal input =
    Type.result
        $ Stream.schedFor input $ \x ->
              Stream.schedForM input $ \y -> do
                return $ Type.checkStream maxVal x y

{-# INLINE unfoldCrossEqn #-}
unfoldCrossEqn :: Monad m => Int -> Unfold m ((), ()) Int -> m ()
unfoldCrossEqn maxVal input =
    Type.result
        $ Stream.mapM (Type.checkPair maxVal)
        $ Stream.unfold (Unfold.cross input input) (undefined, undefined)

{-# INLINE fairUnfoldCrossEqn #-}
fairUnfoldCrossEqn :: Monad m => Int -> Unfold m ((), ()) Int -> m ()
fairUnfoldCrossEqn maxVal input =
    Type.result
        $ Stream.mapM (Type.checkPair maxVal)
        $ Stream.unfold (Unfold.fairCross input input) (undefined, undefined)

{-# INLINE fairUnfoldEachEqn #-}
fairUnfoldEachEqn :: Monad m => Int -> Unfold m ((), ()) Int -> Stream m Int -> m ()
fairUnfoldEachEqn maxVal input ints =
    let intu = Unfold.carryInput $ Unfold.lmap (const (undefined, undefined)) input
     in Type.result
        $ Stream.mapM (Type.checkPair maxVal)
        $ Stream.fairUnfoldEach intu ints

{-# INLINE unfoldSchedEqn #-}
unfoldSchedEqn :: Monad m => Int -> Unfold m ((), ()) Int -> Stream m Int -> m ()
unfoldSchedEqn maxVal input ints =
    let intu = Unfold.carryInput $ Unfold.lmap (const (undefined, undefined)) input
     in Type.result
        $ Stream.mapM (Type.checkPair maxVal)
        $ Stream.unfoldSched intu ints

{-# INLINE fairUnfoldSchedEqn #-}
fairUnfoldSchedEqn :: Monad m => Int -> Unfold m ((), ()) Int -> Stream m Int -> m ()
fairUnfoldSchedEqn maxVal input ints =
    let intu = Unfold.carryInput $ Unfold.lmap (const (undefined, undefined)) input
     in Type.result
        $ Stream.mapM (Type.checkPair maxVal)
        $ Stream.fairUnfoldSched intu ints

fairConcatForBounded :: Int -> IO ()
fairConcatForBounded maxVal = withRandomIntIO $ \n ->
    fairConcatForEqn maxVal (Type.boundedInts maxVal n)

fairConcatForKBounded :: Int -> IO ()
fairConcatForKBounded maxVal = withRandomIntIO $ \n ->
    fairConcatForEqnK maxVal (Type.boundedInts maxVal n)

fairConcatForInfinite :: Int -> IO ()
fairConcatForInfinite maxVal = withRandomIntIO $ \n ->
    fairConcatForEqn maxVal (Type.infiniteInts maxVal n)

fairSchedForBounded :: Int -> IO ()
fairSchedForBounded maxVal = withRandomIntIO $ \n ->
    fairSchedForEqn maxVal (Type.boundedInts maxVal n)

fairSchedForInfinite :: Int -> IO ()
fairSchedForInfinite maxVal = withRandomIntIO $ \n ->
    fairSchedForEqn maxVal (Type.infiniteInts maxVal n)

unfoldCrossBounded :: Int -> IO ()
unfoldCrossBounded maxVal = unfoldCrossEqn maxVal (Type.boundedIntsUnfold maxVal 0)

fairUnfoldCrossBounded :: Int -> IO ()
fairUnfoldCrossBounded maxVal = fairUnfoldCrossEqn maxVal (Type.boundedIntsUnfold maxVal 0)

fairUnfoldCrossInfinite :: Int -> IO ()
fairUnfoldCrossInfinite maxVal = fairUnfoldCrossEqn maxVal (infiniteIntsUnfold maxVal 0)

fairUnfoldEachBounded :: Int -> IO ()
fairUnfoldEachBounded maxVal = withRandomIntIO $ \n ->
    fairUnfoldEachEqn maxVal (Type.boundedIntsUnfold maxVal 0) (Type.boundedInts maxVal n)

fairUnfoldEachInfinite :: Int -> IO ()
fairUnfoldEachInfinite maxVal = withRandomIntIO $ \n ->
    fairUnfoldEachEqn maxVal (infiniteIntsUnfold maxVal 0) (Type.infiniteInts maxVal n)

unfoldSchedBounded :: Int -> IO ()
unfoldSchedBounded maxVal = withRandomIntIO $ \n ->
    unfoldSchedEqn maxVal (Type.boundedIntsUnfold maxVal 0) (Type.boundedInts maxVal n)

fairUnfoldSchedBounded :: Int -> IO ()
fairUnfoldSchedBounded maxVal = withRandomIntIO $ \n ->
    fairUnfoldSchedEqn maxVal (Type.boundedIntsUnfold maxVal 0) (Type.boundedInts maxVal n)

fairUnfoldSchedInfinite :: Int -> IO ()
fairUnfoldSchedInfinite maxVal = withRandomIntIO $ \n ->
    fairUnfoldSchedEqn maxVal (infiniteIntsUnfold maxVal 0) (Type.infiniteInts maxVal n)

-------------------------------------------------------------------------------
-- Joining
-------------------------------------------------------------------------------

-- XXX this should be moved to the Top module
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

      -- Solve simultaneous equations by exploring all possibilities
      , (SpaceO_1, benchIO "equations/fairConcatFor (bounded)" $ fairConcatForBounded sqrtVal)
      , (SpaceO_1, benchIO "equations/fairConcatForK (bounded)" $ fairConcatForKBounded sqrtVal)
      , (SpaceO_1, benchIO "equations/fairConcatFor (infinite)" $ fairConcatForInfinite sqrtVal)
      , (SpaceO_1, benchIO "equations/fairSchedFor (bounded)" $ fairSchedForBounded sqrtVal)
      , (SpaceO_1, benchIO "equations/fairSchedFor (infinite)" $ fairSchedForInfinite sqrtVal)
      , (SpaceO_1, benchIO "equations/unfoldCross (bounded)" $ unfoldCrossBounded sqrtVal)
      , (SpaceO_1, benchIO "equations/fairUnfoldCross (bounded)" $ fairUnfoldCrossBounded sqrtVal)
      , (SpaceO_1, benchIO "equations/fairUnfoldCross (infinite)" $ fairUnfoldCrossInfinite sqrtVal)
      , (SpaceO_1, benchIO "equations/fairUnfoldEach (bounded)" $ fairUnfoldEachBounded sqrtVal)
      , (SpaceO_1, benchIO "equations/fairUnfoldEach (infinite)" $ fairUnfoldEachInfinite sqrtVal)
      , (SpaceO_1, benchIO "equations/unfoldSched (bounded)" $ unfoldSchedBounded sqrtVal)
      , (SpaceO_1, benchIO "equations/fairUnfoldSched (bounded)" $ fairUnfoldSchedBounded sqrtVal)
      , (SpaceO_1, benchIO "equations/fairUnfoldSched (infinite)" $ fairUnfoldSchedInfinite sqrtVal)
      , (HeapO_n, benchIO "bfsUnfoldEach (n of 1)" $ bfsUnfoldEach size 1)
      , (HeapO_n, benchIO "bfsUnfoldEach (sqrtVal of sqrtVal)" $ bfsUnfoldEach sqrtVal sqrtVal)
      , (HeapO_n, benchIO "altBfsUnfoldEach (n of 1)" $ altBfsUnfoldEach size 1)
      , (HeapO_n, benchIO "altBfsUnfoldEach (sqrtVal of sqrtVal)" $ altBfsUnfoldEach sqrtVal sqrtVal)
      , (HeapO_n, benchIO "unfoldSched (n of 1)" $ unfoldSched size 1)
      , (HeapO_n, benchIO "unfoldSched (sqrtVal of sqrtVal)" $ unfoldSched sqrtVal sqrtVal)
      ]

    where

    sqrtVal = round $ sqrt (fromIntegral size :: Double)
