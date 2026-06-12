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
        [ benchIO "interleave" $ interleave2 (value `div` 2)
        , benchIO "roundRobin" $ roundRobin2 (value `div` 2)
        , benchIO "mergeBy compare" $ mergeBy compare (value `div` 2)
        , benchIO "mergeByM compare" $ mergeByM compare (value `div` 2)
        , benchIO "mergeBy (flip compare)" $ mergeBy (flip compare) (value `div` 2)
        , benchIO "mergeByM (flip compare)" $ mergeByM (flip compare) (value `div` 2)

        -- join 2 streams using n-ary ops
        , benchIO "bfsUnfoldEach" $ bfsUnfoldEach 2 (value `div` 2)
        , benchIO "altBfsUnfoldEach" $ altBfsUnfoldEach 2 (value `div` 2)
        , benchIO "unfoldSched" $ unfoldSched 2 (value `div` 2)
        ]
    ]

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

-- Solve simultaneous equations by exploring all possibilities
o_1_space_equations :: Int -> [Benchmark]
o_1_space_equations value =
    [ bgroup "equations"
        [ benchIO "fairConcatFor (bounded)" $ fairConcatForBounded sqrtVal
        , benchIO "fairConcatForK (bounded)" $ fairConcatForKBounded sqrtVal
        , benchIO "fairConcatFor (infinite)" $ fairConcatForInfinite sqrtVal
        , benchIO "fairSchedFor (bounded)" $ fairSchedForBounded sqrtVal
        , benchIO "fairSchedFor (infinite)" $ fairSchedForInfinite sqrtVal
        , benchIO "unfoldCross (bounded)" $ unfoldCrossBounded sqrtVal
        , benchIO "fairUnfoldCross (bounded)" $ fairUnfoldCrossBounded sqrtVal
        , benchIO "fairUnfoldCross (infinite)" $ fairUnfoldCrossInfinite sqrtVal
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
        , o_1_space_equations size
        ])
    ++ map (HeapO_n,)
    {-
        -- multi-stream
        (o_n_heap_buffering size)
    -}
        (o_n_heap_concat size)
