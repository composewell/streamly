-- |
-- Module      : Streamly.Benchmark.Data.StreamK
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Main (main) where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Data.Maybe (isJust)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.StreamK (StreamK)
import System.Random (randomRIO)
import Test.Tasty.Bench (bench, nf, nfIO, bgroup, Benchmark)

import qualified Data.List as List
import qualified Prelude as P
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.StreamK as StreamK

import Prelude hiding
    ( Foldable(..), tail, mapM_, last, map, mapM, concatMap, zipWith, init
    , iterate, repeat, replicate
    )
import Streamly.Benchmark.Common
#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

{-# INLINE withRandomIntIO #-}
withRandomIntIO :: (Int -> IO b) -> IO b
withRandomIntIO f = randomRIO (1, 1 :: Int) >>= f

{-# INLINE withDrain #-}
withDrain :: (Int -> StreamK IO a) -> IO ()
withDrain f = withRandomIntIO $ \n -> StreamK.drain (f n)

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: Int -> Int -> StreamK m Int
sourceUnfoldr streamLen n = StreamK.unfoldr step n
    where
    step cnt =
        if cnt > n + streamLen
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE unfoldr #-}
unfoldr :: Int -> IO ()
unfoldr streamLen = withDrain (sourceUnfoldr streamLen)

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> StreamK m Int
sourceUnfoldrM streamLen n = StreamK.unfoldrMWith StreamK.consM step n
    where
    step cnt =
        if cnt > n + streamLen
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE unfoldrM #-}
unfoldrM :: Int -> IO ()
unfoldrM streamLen = withDrain (sourceUnfoldrM streamLen)

{-# INLINE withStream #-}
withStream :: Int -> (StreamK IO Int -> IO b) -> IO b
withStream value f = randomRIO (1,1) >>= f . sourceUnfoldrM value

{-# INLINE repeat #-}
repeat :: Int -> IO ()
repeat streamLen = withDrain $ StreamK.take streamLen . StreamK.repeat

{-# INLINE repeatM #-}
repeatM :: Int -> IO ()
repeatM streamLen = withDrain $ StreamK.take streamLen . StreamK.repeatM . return

{-# INLINE replicate #-}
replicate :: Int -> IO ()
replicate streamLen = withDrain (StreamK.replicate streamLen)

{-# INLINE replicateM #-}
replicateM :: Int -> IO ()
replicateM streamLen =
    withDrain $ StreamK.replicateMWith StreamK.consM streamLen . return

{-# INLINE iterate #-}
iterate :: Int -> IO ()
iterate streamLen = withDrain $ StreamK.take streamLen . StreamK.iterate (+1)

{-# INLINE iterateM #-}
iterateM :: Int -> IO ()
iterateM streamLen =
    withDrain $ StreamK.take streamLen . StreamK.iterateM (return . (+1)) . return

{-# INLINE fromFoldable #-}
fromFoldable :: Int -> IO ()
fromFoldable streamLen =
    withDrain $ \n -> StreamK.fromFoldable [n..n+streamLen]

{- HLINT ignore "Fuse foldr/fmap" -}
{-# INLINE fromFoldableM #-}
fromFoldableM :: Int -> IO ()
fromFoldableM streamLen =
    withDrain $ \n ->
    List.foldr StreamK.consM StreamK.nil (Prelude.fmap return [n..n+streamLen])

{-# INLINE concatMapFoldableSerial #-}
concatMapFoldableSerial :: Int -> Int -> StreamK m Int
concatMapFoldableSerial streamLen n =
    P.foldr (StreamK.append . StreamK.fromPure) StreamK.nil [n..n+streamLen]

{-# INLINE concatMapFoldableSerialM #-}
concatMapFoldableSerialM :: Monad m => Int -> Int -> StreamK m Int
concatMapFoldableSerialM streamLen n =
    P.foldr (StreamK.append . StreamK.fromEffect . return) StreamK.nil [n..n+streamLen]

{-# INLINE concatMapFoldableWith #-}
concatMapFoldableWith :: Int -> IO ()
concatMapFoldableWith streamLen = withDrain (concatMapFoldableSerial streamLen)

{-# INLINE concatMapFoldableWithM #-}
concatMapFoldableWithM :: Int -> IO ()
concatMapFoldableWithM streamLen = withDrain (concatMapFoldableSerialM streamLen)

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE toNull #-}
toNull :: Int -> IO ()
toNull streamLen = withDrain (sourceUnfoldrM streamLen)

{-# INLINE mapM_ #-}
mapM_ :: Int -> IO ()
mapM_ streamLen = withStream streamLen (StreamK.mapM_ (\_ -> return ()))

{-# INLINE uncons #-}
uncons :: Int -> IO ()
uncons streamLen = withStream streamLen go
    where
    go s = do
        r <- StreamK.uncons s
        case r of
            Nothing -> return ()
            Just (_, t) -> go t

{-# INLINE init #-}
init :: Int -> IO ()
init streamLen = withStream streamLen go
    where
    go s = do
        t <- StreamK.init s
        P.mapM_ StreamK.drain t

{-# INLINE tail #-}
tail :: Int -> IO ()
tail streamLen = withStream streamLen go
    where go s = StreamK.tail s >>= P.mapM_ go

{-# INLINE nullTail #-}
nullTail :: Int -> IO ()
nullTail streamLen = withStream streamLen go
    where
    go s = do
        r <- StreamK.null s
        when (not r) $ StreamK.tail s >>= P.mapM_ go

{-# INLINE headTail #-}
headTail :: Int -> IO ()
headTail streamLen = withStream streamLen go
    where
    go s = do
        h <- StreamK.head s
        when (isJust h) $ StreamK.tail s >>= P.mapM_ go

{-# INLINE toList #-}
toList :: Int -> IO [Int]
toList streamLen = withStream streamLen StreamK.toList

{-# INLINE foldl' #-}
foldl' :: Int -> IO Int
foldl' streamLen = withStream streamLen (StreamK.foldl' (+) 0)

{-# INLINE foldlM' #-}
foldlM' :: Int -> IO Int
foldlM' streamLen =
    withStream streamLen (StreamK.foldlM' (\b a -> return (b + a)) (return 0))

{-# INLINE last #-}
last :: Int -> IO (Maybe Int)
last streamLen = withStream streamLen StreamK.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE composeN #-}
composeN
    :: Monad m
    => Int -> (StreamK m Int -> StreamK m Int) -> StreamK m Int -> m ()
composeN n f =
    case n of
        1 -> StreamK.drain . f
        2 -> StreamK.drain . f . f
        3 -> StreamK.drain . f . f . f
        4 -> StreamK.drain . f . f . f . f
        _ -> undefined

{-# INLINE scanl' #-}
scanl' :: Int -> Int -> IO ()
scanl' n streamLen = withStream streamLen (composeN n (StreamK.scanl' (+) 0))

{-# INLINE map #-}
map :: Int -> Int -> IO ()
map n streamLen = withStream streamLen (composeN n (StreamK.map (+ 1)))

{-# INLINE fmapK #-}
fmapK :: Int -> Int -> IO ()
fmapK n streamLen = withStream streamLen (composeN n (P.fmap (+ 1)))

{-# INLINE mapM #-}
mapM :: Int -> Int -> IO ()
mapM n streamLen = withStream streamLen (composeN n (StreamK.mapMWith StreamK.consM return))

{-# INLINE mapMSerial #-}
mapMSerial :: Int -> Int -> IO ()
mapMSerial n streamLen = withStream streamLen (composeN n (StreamK.mapMSerial return))

{-# INLINE filterEven #-}
filterEven :: Int -> Int -> IO ()
filterEven n streamLen = withStream streamLen (composeN n (StreamK.filter even))

{-# INLINE filterAllOut #-}
filterAllOut :: Int -> Int -> IO ()
filterAllOut n streamLen = withStream streamLen (composeN n (StreamK.filter (> streamLen)))

{-# INLINE filterAllIn #-}
filterAllIn :: Int -> Int -> IO ()
filterAllIn n streamLen = withStream streamLen (composeN n (StreamK.filter (<= streamLen)))

{-# INLINE _takeOne #-}
_takeOne :: Monad m => Int -> StreamK m Int -> m ()
_takeOne n = composeN n $ StreamK.take 1

{-# INLINE takeAll #-}
takeAll :: Int -> Int -> IO ()
takeAll n streamLen = withStream streamLen (composeN n (StreamK.take streamLen))

{-# INLINE takeWhileTrue #-}
takeWhileTrue :: Int -> Int -> IO ()
takeWhileTrue n streamLen = withStream streamLen (composeN n (StreamK.takeWhile (<= streamLen)))

{-# INLINE dropOne #-}
dropOne :: Int -> Int -> IO ()
dropOne n streamLen = withStream streamLen (composeN n (StreamK.drop 1))

{-# INLINE dropAll #-}
dropAll :: Int -> Int -> IO ()
dropAll n streamLen = withStream streamLen (composeN n (StreamK.drop streamLen))

{-# INLINE dropWhileTrue #-}
dropWhileTrue :: Int -> Int -> IO ()
dropWhileTrue n streamLen = withStream streamLen (composeN n (StreamK.dropWhile (<= streamLen)))

{-# INLINE dropWhileFalse #-}
dropWhileFalse :: Int -> Int -> IO ()
dropWhileFalse n streamLen = withStream streamLen (composeN n (StreamK.dropWhile (<= 1)))

{-# INLINE foldrS #-}
foldrS :: Int -> Int -> IO ()
foldrS n streamLen = withStream streamLen (composeN n (StreamK.foldrS StreamK.cons StreamK.nil))

{-# INLINE foldlS #-}
foldlS :: Int -> Int -> IO ()
foldlS n streamLen = withStream streamLen (composeN n (StreamK.foldlS (flip StreamK.cons) StreamK.nil))

{-# INLINE intersperse #-}
intersperse :: Int -> Int -> Int -> IO ()
intersperse bound n streamLen = withStream streamLen (composeN n (StreamK.intersperse bound))

{-# INLINE interspersePure #-}
interspersePure :: Int -> Int -> Int -> IO ()
interspersePure bound n streamLen = withRandomIntIO $ composeN n (StreamK.intersperse bound) . sourceUnfoldr streamLen

-------------------------------------------------------------------------------
-- Iteration
-------------------------------------------------------------------------------

{-# INLINE iterateSource #-}
iterateSource
    :: Monad m => Int -> (StreamK m Int -> StreamK m Int) -> Int -> Int -> StreamK m Int
iterateSource iterStreamLen g i n = f i (sourceUnfoldrM iterStreamLen n)
    where
        f (0 :: Int) m = g m
        f x m = g (f (x P.- 1) m)

-- this is quadratic
{-# INLINE iterateScan #-}
iterateScan :: Int -> Int -> IO ()
iterateScan iterStreamLen maxIters =
    withDrain $ iterateSource iterStreamLen (StreamK.scanl' (+) 0) (maxIters `div` 10)

-- this is quadratic
{-# INLINE iterateDropWhileFalse #-}
iterateDropWhileFalse :: Int -> Int -> Int -> IO ()
iterateDropWhileFalse streamLen iterStreamLen maxIters =
    withDrain $ iterateSource iterStreamLen (StreamK.dropWhile (> streamLen)) (maxIters `div` 10)

{-# INLINE iterateMapM #-}
iterateMapM :: Int -> Int -> IO ()
iterateMapM iterStreamLen maxIters =
    withDrain $ iterateSource iterStreamLen (StreamK.mapMWith StreamK.consM return) maxIters

{-# INLINE iterateFilterEven #-}
iterateFilterEven :: Int -> Int -> IO ()
iterateFilterEven iterStreamLen maxIters =
    withDrain $ iterateSource iterStreamLen (StreamK.filter even) maxIters

{-# INLINE iterateTakeAll #-}
iterateTakeAll :: Int -> Int -> Int -> IO ()
iterateTakeAll streamLen iterStreamLen maxIters =
    withDrain $ iterateSource iterStreamLen (StreamK.take streamLen) maxIters

{-# INLINE iterateDropOne #-}
iterateDropOne :: Int -> Int -> IO ()
iterateDropOne iterStreamLen maxIters =
    withDrain $ iterateSource iterStreamLen (StreamK.drop 1) maxIters

{-# INLINE iterateDropWhileTrue #-}
iterateDropWhileTrue :: Int -> Int -> Int -> IO ()
iterateDropWhileTrue streamLen iterStreamLen maxIters =
    withDrain $ iterateSource iterStreamLen (StreamK.dropWhile (<= streamLen)) maxIters

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

{-# INLINE zipWith #-}
zipWith :: Int -> IO ()
zipWith streamLen = withDrain $ \n ->
    let src = sourceUnfoldrM streamLen n
    in StreamK.zipWith (,) src src

{-# INLINE zipWithM #-}
zipWithM :: Int -> IO ()
zipWithM streamLen = withDrain $ \n ->
    let src = sourceUnfoldrM streamLen n
    in StreamK.zipWithM (curry return) src src

-------------------------------------------------------------------------------
-- Sorting
-------------------------------------------------------------------------------

{-# INLINE sortByK #-}
sortByK :: (Int -> Int -> Ordering) -> StreamK m Int -> StreamK m Int
sortByK f = StreamK.mergeMapWith (StreamK.mergeBy f) StreamK.fromPure

{-# INLINE sortBy #-}
sortBy :: (Int -> Int -> Ordering) -> Int -> IO ()
sortBy cmp streamLen = withDrain $ sortByK cmp . sourceUnfoldrM streamLen

{-# INLINE sortByCompareRandomized #-}
sortByCompareRandomized :: Int -> IO ()
sortByCompareRandomized streamLen =
    withDrain $ sortByK compare . StreamK.map (\x -> if even x then x + 2 else x) . sourceUnfoldrM streamLen

-------------------------------------------------------------------------------
-- Joining
-------------------------------------------------------------------------------

{-# INLINE interleave2 #-}
interleave2 :: Int -> IO ()
interleave2 value =
    withDrain $ \n ->
    StreamK.interleave
        (sourceUnfoldrM (value `div` 2) n)
        (sourceUnfoldrM (value `div` 2) (n + 1))

{-# INLINE concatMapWith #-}
concatMapWith
    :: (StreamK IO Int -> StreamK IO Int -> StreamK IO Int)
    -> Int
    -> Int
    -> IO ()
concatMapWith op outer inner =
    withDrain $ \n ->
    StreamK.concatMapWith op
        (sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

{-# INLINE concatMapWithD #-}
concatMapWithD
    :: (Stream IO Int -> Stream IO Int -> Stream IO Int)
    -> Int
    -> Int
    -> IO ()
concatMapWithD op outer inner =
    withDrain $ \n ->
    StreamK.concatMapWith op1
        (sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

    where

    op1 s1 s2 = StreamK.fromStream $ op (StreamK.toStream s1) (StreamK.toStream s2)

{-# INLINE mergeMapWith #-}
mergeMapWith
    :: (StreamK IO Int -> StreamK IO Int -> StreamK IO Int)
    -> Int
    -> Int
    -> IO ()
mergeMapWith op outer inner =
    withDrain $ \n ->
    StreamK.mergeMapWith op
        (sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

{-# INLINE mergeMapWithD #-}
mergeMapWithD
    :: (Stream IO Int -> Stream IO Int -> Stream IO Int)
    -> Int
    -> Int
    -> IO ()
mergeMapWithD op outer inner =
    withDrain $ \n ->
    StreamK.mergeMapWith op1
        (sourceUnfoldrM inner)
        (sourceUnfoldrM outer n)

    where

    op1 s1 s2 = StreamK.fromStream $ op (StreamK.toStream s1) (StreamK.toStream s2)

-------------------------------------------------------------------------------
-- Merging
-------------------------------------------------------------------------------

{-# INLINE mergeWith #-}
mergeWith ::
    (  (Int -> Int -> Ordering)
    -> StreamK IO Int
    -> StreamK IO Int
    -> StreamK IO Int
    )
    -> (Int -> Int -> Ordering)
    -> Int -> IO ()
mergeWith g cmp count =
    withDrain $ \n ->
    g cmp
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

{-# INLINE mergeWithM #-}
mergeWithM ::
    (  (Int -> Int -> IO Ordering)
    -> StreamK IO Int
    -> StreamK IO Int
    -> StreamK IO Int
    )
    -> (Int -> Int -> Ordering)
    -> Int -> IO ()
mergeWithM g cmp count =
    withDrain $ \n ->
    g (\a b -> return $ cmp a b)
        (sourceUnfoldrM count n)
        (sourceUnfoldrM count (n + 1))

{-# INLINE mergeBy #-}
mergeBy :: (Int -> Int -> Ordering) -> Int -> IO ()
mergeBy = mergeWith StreamK.mergeBy

{-# INLINE mergeByM #-}
mergeByM :: (Int -> Int -> Ordering) -> Int -> IO ()
mergeByM = mergeWithM StreamK.mergeByM

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mergeBy
inspect $ 'mergeBy `hasNoType` ''SPEC

inspect $ hasNoTypeClasses 'mergeByM
inspect $ 'mergeByM `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Mixed Composition
-------------------------------------------------------------------------------

{-# INLINE scanMap #-}
scanMap :: Int -> Int -> IO ()
scanMap n streamLen = withStream streamLen (composeN n (StreamK.map (subtract 1) . StreamK.scanl' (+) 0))

{-# INLINE dropMap #-}
dropMap :: Int -> Int -> IO ()
dropMap n streamLen = withStream streamLen (composeN n (StreamK.map (subtract 1) . StreamK.drop 1))

{-# INLINE dropScan #-}
dropScan :: Int -> Int -> IO ()
dropScan n streamLen = withStream streamLen (composeN n (StreamK.scanl' (+) 0 . StreamK.drop 1))

{-# INLINE takeDrop #-}
takeDrop :: Int -> Int -> IO ()
takeDrop n streamLen = withStream streamLen (composeN n (StreamK.drop 1 . StreamK.take streamLen))

{-# INLINE takeScan #-}
takeScan :: Int -> Int -> IO ()
takeScan n streamLen = withStream streamLen (composeN n (StreamK.scanl' (+) 0 . StreamK.take streamLen))

{-# INLINE takeMap #-}
takeMap :: Int -> Int -> IO ()
takeMap n streamLen = withStream streamLen (composeN n (StreamK.map (subtract 1) . StreamK.take streamLen))

{-# INLINE filterDrop #-}
filterDrop :: Int -> Int -> IO ()
filterDrop n streamLen = withStream streamLen (composeN n (StreamK.drop 1 . StreamK.filter (<= streamLen)))

{-# INLINE filterTake #-}
filterTake :: Int -> Int -> IO ()
filterTake n streamLen = withStream streamLen (composeN n (StreamK.take streamLen . StreamK.filter (<= streamLen)))

{-# INLINE filterScan #-}
filterScan :: Int -> Int -> IO ()
filterScan n streamLen = withStream streamLen (composeN n (StreamK.scanl' (+) 0 . StreamK.filter (<= maxBound)))

{-# INLINE filterMap #-}
filterMap :: Int -> Int -> IO ()
filterMap n streamLen = withStream streamLen (composeN n (StreamK.map (subtract 1) . StreamK.filter (<= streamLen)))

-------------------------------------------------------------------------------
-- ConcatMap
-------------------------------------------------------------------------------

-- concatMap unfoldrM/unfoldrM

{-# INLINE concatMap #-}
concatMap :: Int -> Int -> IO ()
concatMap outer inner =
    withDrain $ \n ->
    StreamK.concatMap
        (\_ -> sourceUnfoldrM inner n)
        (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMap
#endif

-- concatMap unfoldr/unfoldr

{-# INLINE concatMapUnfoldr #-}
concatMapUnfoldr :: Int -> Int -> IO ()
concatMapUnfoldr outer inner =
    withDrain $ \n ->
    StreamK.concatMap
        (\_ -> sourceUnfoldr inner n)
        (sourceUnfoldr outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapUnfoldr
#endif

-- concatMap replicate/unfoldrM

{-# INLINE concatMapRepl #-}
concatMapRepl :: Int -> Int -> IO ()
concatMapRepl outer inner =
    withDrain $ \n ->
    StreamK.concatMap (StreamK.replicate inner) (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapRepl
#endif

-- concatMapWith

{-# INLINE sourceConcatMapId #-}
sourceConcatMapId :: Monad m
    => Int -> Int -> StreamK m (StreamK m Int)
sourceConcatMapId val n =
    StreamK.fromFoldable $ fmap (StreamK.fromEffect . return) [n..n+val]

{-# INLINE concatMapWithId #-}
concatMapWithId :: Int -> IO ()
concatMapWithId streamLen =
    withDrain $ StreamK.concatMapWith StreamK.append id . sourceConcatMapId streamLen

-------------------------------------------------------------------------------
-- Nested Composition
-------------------------------------------------------------------------------

instance Monad m => Applicative (StreamK.StreamK m) where
    {-# INLINE pure #-}
    pure = StreamK.fromPure

    {-# INLINE (<*>) #-}
    (<*>) = StreamK.crossApply

    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)

    {-# INLINE (*>) #-}
    (*>) = StreamK.crossApplySnd

    {-# INLINE (<*) #-}
    (<*) = StreamK.crossApplyFst

-- NOTE: even though concatMap for StreamD is 3x faster compared to StreamK,
-- the monad instance of StreamD is slower than StreamK after foldr/build
-- fusion.
instance Monad m => Monad (StreamK.StreamK m) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = flip StreamK.concatMap

{-# INLINE drainApplicative #-}
drainApplicative :: Int -> IO ()
drainApplicative streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in (+) <$> s <*> s

{-# INLINE drainApplicativeUnfoldr #-}
drainApplicativeUnfoldr :: Int -> IO ()
drainApplicativeUnfoldr streamLen = withDrain $ \n ->
    let s = sourceUnfoldr streamLen n
    in (+) <$> s <*> s

{-# INLINE drainMonad #-}
drainMonad :: Int -> IO ()
drainMonad streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in do { x <- s; y <- s; return $ x + y }

{-# INLINE drainMonadUnfoldr #-}
drainMonadUnfoldr :: Int -> IO ()
drainMonadUnfoldr streamLen = withDrain $ \n ->
    let s = sourceUnfoldr streamLen n
    in do { x <- s; y <- s; return $ x + y }

{-# INLINE drainConcatFor1 #-}
drainConcatFor1 :: Int -> IO ()
drainConcatFor1 streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatFor s $ \x -> StreamK.fromPure $ x + 1

{-# INLINE drainConcatFor #-}
drainConcatFor :: Int -> IO ()
drainConcatFor streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            StreamK.fromPure $ x + y

{-# INLINE drainConcatForM #-}
drainConcatForM :: Int -> IO ()
drainConcatForM streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatForM s $ \x ->
        pure $ StreamK.concatForM s $ \y ->
            pure $ StreamK.fromPure $ x + y

{-# INLINE drainMonad3 #-}
drainMonad3 :: Int -> IO ()
drainMonad3 streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in do { x <- s; y <- s; z <- s; return $ x + y + z }

{-# INLINE drainMonad3Unfoldr #-}
drainMonad3Unfoldr :: Int -> IO ()
drainMonad3Unfoldr streamLen = withDrain $ \n ->
    let s = sourceUnfoldr streamLen n
    in do { x <- s; y <- s; z <- s; return $ x + y + z }

{-# INLINE drainConcatFor3 #-}
drainConcatFor3 :: Int -> IO ()
drainConcatFor3 streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            StreamK.concatFor s $ \z ->
                StreamK.fromPure $ x + y + z

{-# INLINE drainConcatFor3M #-}
drainConcatFor3M :: Int -> IO ()
drainConcatFor3M streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatForM s $ \x ->
        pure $ StreamK.concatForM s $ \y ->
            pure $ StreamK.concatForM s $ \z ->
                pure $ StreamK.fromPure $ x + y + z

{-# INLINE drainConcatFor4 #-}
drainConcatFor4 :: Int -> IO ()
drainConcatFor4 streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            StreamK.concatFor s $ \z ->
                StreamK.concatFor s $ \w ->
                    StreamK.fromPure $ x + y + z + w

{-# INLINE drainConcatFor5 #-}
drainConcatFor5 :: Int -> IO ()
drainConcatFor5 streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            StreamK.concatFor s $ \z ->
                StreamK.concatFor s $ \w ->
                    StreamK.concatFor s $ \u ->
                        StreamK.fromPure $ x + y + z + w + u

{-# INLINE filterAllOutMonad #-}
filterAllOutMonad :: Int -> IO ()
filterAllOutMonad streamLen = withDrain $ \n ->
    let str = sourceUnfoldrM streamLen n
    in do
        x <- str
        y <- str
        let s = x + y
        if s < 0 then return s else StreamK.nil

{-# INLINE filterAllOutMonadUnfoldr #-}
filterAllOutMonadUnfoldr :: Int -> IO ()
filterAllOutMonadUnfoldr streamLen = withDrain $ \n ->
    let str = sourceUnfoldr streamLen n
    in do
        x <- str
        y <- str
        let s = x + y
        if s < 0 then return s else StreamK.nil

{-# INLINE filterAllOutConcatFor #-}
filterAllOutConcatFor :: Int -> IO ()
filterAllOutConcatFor streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            let s1 = x + y
             in if s1 < 0 then StreamK.fromPure s1 else StreamK.nil

{-# INLINE filterAllInMonad #-}
filterAllInMonad :: Int -> IO ()
filterAllInMonad streamLen = withDrain $ \n ->
    let str = sourceUnfoldrM streamLen n
    in do
        x <- str
        y <- str
        let s = x + y
        if s > 0 then return s else StreamK.nil

{-# INLINE filterAllInMonadUnfoldr #-}
filterAllInMonadUnfoldr :: Int -> IO ()
filterAllInMonadUnfoldr streamLen = withDrain $ \n ->
    let str = sourceUnfoldr streamLen n
    in do
        x <- str
        y <- str
        let s = x + y
        if s > 0 then return s else StreamK.nil

{-# INLINE filterAllInConcatFor #-}
filterAllInConcatFor :: Int -> IO ()
filterAllInConcatFor streamLen = withDrain $ \n ->
    let s = sourceUnfoldrM streamLen n
    in StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            let s1 = x + y
             in if s1 > 0 then StreamK.fromPure s1 else StreamK.nil

-------------------------------------------------------------------------------
-- Nested Composition Pure lists
-------------------------------------------------------------------------------

-- There are several list benchmarks here for comparison with lists. It is easy
-- and convenient to see the comparisons when they are here, otherwise we'll
-- have to add a separate module for list benchmarks with the same names and
-- then add a comparison in bench.sh.

{-# INLINE unfoldrList #-}
unfoldrList :: Int -> Int -> [Int]
unfoldrList maxval n = List.unfoldr step n
    where
    step cnt =
        if cnt > n + maxval
        then Nothing
        else Just (cnt, cnt + 1)


{-# INLINE withList #-}
withList :: Int -> ([Int] -> IO b) -> IO b
withList value f = randomRIO (1,1) >>= f . unfoldrList value

{-# INLINE lastList #-}
lastList :: Int -> IO [Int]
lastList streamLen = withList streamLen (return . (\xs -> [List.last xs]))

{-# INLINE listApDrain2 #-}
listApDrain2 :: Int -> IO [Int]
listApDrain2 streamLen = withList streamLen $ \s -> return $ (+) <$> s <*> s

{-# INLINE listMonadDrain2 #-}
listMonadDrain2 :: Int -> IO [Int]
listMonadDrain2 streamLen = withList streamLen $ \s -> return $ do
    x <- s
    y <- s
    return $ x + y

{-# INLINE listMonadDrain3 #-}
listMonadDrain3 :: Int -> IO [Int]
listMonadDrain3 streamLen = withList streamLen $ \s -> return $ do
    x <- s
    y <- s
    z <- s
    return $ x + y + z

{-# INLINE listMonadFilterAllIn2 #-}
listMonadFilterAllIn2 :: Int -> IO [Int]
listMonadFilterAllIn2 streamLen = withList streamLen $ \s -> return $ do
    x <- s
    y <- s
    let t = x + y
    if t > 0 then return t else []

{-# INLINE listMonadFilterAllOut2 #-}
listMonadFilterAllOut2 :: Int -> IO [Int]
listMonadFilterAllOut2 streamLen = withList streamLen $ \s -> return $ do
    x <- s
    y <- s
    let t = x + y
    if t < 0 then return t else []

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.StreamK"

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> IO b -> Benchmark
benchIO name = bench name . nfIO

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation streamLen =
    [ benchIO "unfoldr"               $ unfoldr streamLen
    , benchIO "unfoldrM"              $ unfoldrM streamLen
    , benchIO "repeat"                $ repeat streamLen
    , benchIO "repeatM"               $ repeatM streamLen
    , benchIO "replicate"             $ replicate streamLen
    , benchIO "replicateM"            $ replicateM streamLen
    , benchIO "iterate"               $ iterate streamLen
    , benchIO "iterateM"              $ iterateM streamLen

    , benchIO "fromFoldable"          $ fromFoldable streamLen
    , benchIO "fromFoldableM"         $ fromFoldableM streamLen

    -- appends
    , benchIO "concatMapFoldableWith"  $ concatMapFoldableWith streamLen
    , benchIO "concatMapFoldableWithM" $ concatMapFoldableWithM streamLen
    ]

o_1_space_elimination :: Int -> [Benchmark]
o_1_space_elimination streamLen =
    [ benchIO "toNull"   $ toNull streamLen
    , benchIO "mapM_"    $ mapM_ streamLen
    , benchIO "uncons"   $ uncons streamLen
    , benchIO "init"     $ init streamLen
    , benchIO "foldl'"   $ foldl' streamLen
    , benchIO "foldlM'"  $ foldlM' streamLen
    , benchIO "last"     $ last streamLen
    ]

o_1_space_ap :: Int -> [Benchmark]
o_1_space_ap streamLen =
    [ benchIO "ap drain2"     $ drainApplicative streamLen2
    , benchIO "ap pureDrain2" $ drainApplicativeUnfoldr streamLen2
    ]
    where
    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop

o_1_space_monad :: Int -> [Benchmark]
o_1_space_monad streamLen =
    [ benchIO "monad drain2"            $ drainMonad streamLen2
    , benchIO "monad drain3"            $ drainMonad3 streamLen3
    , benchIO "monad filterAllIn2"      $ filterAllInMonad streamLen2
    , benchIO "monad filterAllOut2"     $ filterAllOutMonad streamLen2
    , benchIO "monad pureDrain2"        $ drainMonadUnfoldr streamLen2
    , benchIO "monad pureDrain3"        $ drainMonad3Unfoldr streamLen3
    , benchIO "monad pureFilterAllIn2"  $ filterAllInMonadUnfoldr streamLen2
    , benchIO "monad pureFilterAllOut2" $ filterAllOutMonadUnfoldr streamLen2
    ]
    where
    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop
    streamLen3 = round (P.fromIntegral streamLen**(1/3::P.Double)) -- triple nested loop

o_1_space_bind :: Int -> [Benchmark]
o_1_space_bind streamLen =
    [ benchIO "concatFor drain1"        $ drainConcatFor1 streamLen
    , benchIO "concatFor drain2"        $ drainConcatFor streamLen2
    , benchIO "concatFor drainM2"       $ drainConcatForM streamLen2
    , benchIO "concatFor drain3"        $ drainConcatFor3 streamLen3
    , benchIO "concatFor drain4"        $ drainConcatFor4 streamLen4
    , benchIO "concatFor drain5"        $ drainConcatFor5 streamLen5
    , benchIO "concatFor drainM3"       $ drainConcatFor3M streamLen3
    , benchIO "concatFor filterAllIn2"  $ filterAllInConcatFor streamLen2
    , benchIO "concatFor filterAllOut2" $ filterAllOutConcatFor streamLen2
    ]
    where
    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop
    streamLen3 = round (P.fromIntegral streamLen**(1/3::P.Double)) -- triple nested loop
    streamLen4 = round (P.fromIntegral streamLen**(1/4::P.Double)) -- 4 times nested loop
    streamLen5 = round (P.fromIntegral streamLen**(1/5::P.Double)) -- 5 times nested loop

o_1_space_transformation :: Int -> [Benchmark]
o_1_space_transformation streamLen =
    [ benchIO "foldrS"     $ foldrS 1 streamLen
    , benchIO "scanl'"     $ scanl' 1 streamLen
    , benchIO "map"        $ map 1 streamLen
    , benchIO "fmap"       $ fmapK 1 streamLen
    , benchIO "mapM"       $ mapM 1 streamLen
    , benchIO "mapMSerial" $ mapMSerial 1 streamLen
    ]

o_1_space_transformationX4 :: Int -> [Benchmark]
o_1_space_transformationX4 streamLen =
    [ benchIO "scanl'X4"     $ scanl' 4 streamLen
    , benchIO "mapX4"        $ map 4 streamLen
    , benchIO "fmapX4"       $ fmapK 4 streamLen
    , benchIO "mapMX4"       $ mapM 4 streamLen
    , benchIO "mapMSerialX4" $ mapMSerial 4 streamLen
    -- XXX this is horribly slow
    -- , benchIO "concatMap" $ concatMap 4 streamLen16
    ]

o_1_space_joining :: Int -> [Benchmark]
o_1_space_joining streamLen =
    [ benchIO "interleave" $ interleave2 streamLen

    , benchIO "mergeBy compare"
        $ mergeBy compare (streamLen `div` 2)
    , benchIO "mergeByM compare"
        $ mergeByM compare (streamLen `div` 2)
    , benchIO "mergeBy (flip compare)"
        $ mergeBy (flip compare) (streamLen `div` 2)
    , benchIO "mergeByM (flip compare)"
        $ mergeByM (flip compare) (streamLen `div` 2)

    , benchIO "zipWith"  $ zipWith streamLen
    , benchIO "zipWithM" $ zipWithM streamLen

    -- join 2 streams using concatMapWith
    , benchIO "concatMapWith interleave"
          $ concatMapWith StreamK.interleave 2 (streamLen `div` 2)
    , benchIO "concatMapWith D.interleave"
          $ concatMapWithD Stream.interleave 2 (streamLen `div` 2)
    , benchIO "concatMapWith D.roundRobin"
          $ concatMapWithD Stream.roundRobin 2 (streamLen `div` 2)

    -- join 2 streams using mergeMapWith
    , benchIO "mergeMapWith interleave"
        $ mergeMapWith StreamK.interleave 2 (streamLen `div` 2)
    , benchIO "mergeMapWith D.interleave"
        $ mergeMapWithD Stream.interleave 2 (streamLen `div` 2)
    , benchIO "mergeMapWith D.roundRobin"
        $ mergeMapWithD Stream.roundRobin 2 (streamLen `div` 2)

    , benchIO "mergeMapWith (mergeBy compare)"
        $ mergeMapWith (StreamK.mergeBy compare) 2 (streamLen `div` 2)
    , benchIO "mergeMapWith (mergeBy (flip compare))"
        $ mergeMapWith (StreamK.mergeBy (flip compare)) 2 (streamLen `div` 2)
    , benchIO "mergeMapWithD (D.mergeBy compare)"
        $ mergeMapWithD (Stream.mergeBy compare) 2 (streamLen `div` 2)
    , benchIO "mergeMapWithD (D.mergeBy (flip compare))"
        $ mergeMapWithD (Stream.mergeBy (flip compare)) 2 (streamLen `div` 2)

    , benchIO "mergeMapWith (zipWith (+))"
        $ mergeMapWith (StreamK.zipWith (+)) 2 (streamLen `div` 2)
    ]

o_1_space_concat :: Int -> [Benchmark]
o_1_space_concat streamLen =
    [ benchIO "concatMapUnfoldr outer=Max inner=1"
        $ concatMapUnfoldr streamLen 1
    , benchIO "concatMapUnfoldr outer=inner=(sqrt Max)"
        $ concatMapUnfoldr streamLen2 streamLen2
    , benchIO "concatMapUnfoldr outer=1 inner=Max"
        $ concatMapUnfoldr 1 streamLen

    , benchIO "concatMap outer=Max inner=1"
        $ concatMap streamLen 1
    , benchIO "concatMap outer=inner=(sqrt Max)"
        $ concatMap streamLen2 streamLen2
    , benchIO "concatMap outer=1 inner=Max"
        $ concatMap 1 streamLen

    , benchIO "concatMapRepl outer=inner=(sqrt Max)"
        $ concatMapRepl streamLen2 streamLen2

    -- This is for comparison with concatMapFoldableWith
    , benchIO "concatMapWithId outer=Max inner=1 (fromFoldable)"
        $ concatMapWithId streamLen

    , benchIO "concatMapWith append outer=Max inner=1"
        $ concatMapWith StreamK.append streamLen 1
    , benchIO "concatMapWith append outer=inner=(sqrt Max)"
        $ concatMapWith StreamK.append streamLen2 streamLen2
    , benchIO "concatMapWith append outer=1 inner=Max"
        $ concatMapWith StreamK.append 1 streamLen

    -- interleave with concatMapWith is O(1)
    , benchIO "concatMapWith interleave outer=Max inner=1"
        $ concatMapWith StreamK.interleave streamLen 1
    , benchIO "concatMapWith interleave outer=inner=(sqrt Max)"
        $ concatMapWith StreamK.interleave streamLen2 streamLen2
    , benchIO "concatMapWith interleave outer=1 inner=Max"
        $ concatMapWith StreamK.interleave 1 streamLen
    ]

    where

    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop

o_n_space_concat :: Int -> [Benchmark]
o_n_space_concat streamLen =
    -- concatMapWith using StreamD versions of interleave operations are
    -- all quadratic, we just measure the sqrtVal benchmark for comparison.
    [ benchIO "concatMapWithD D.interleave outer=inner=(sqrt Max)"
        $ concatMapWithD Stream.interleave streamLen2 streamLen2
    , benchIO "concatMapWithD D.roundRobin outer=inner=(sqrt Max)"
        $ concatMapWithD Stream.roundRobin streamLen2 streamLen2
    ]

    where

    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop

o_n_heap_concat :: Int -> [Benchmark]
o_n_heap_concat streamLen =
    [ benchIO "mergeMapWith interleave outer=Max inner=1"
        $ mergeMapWith StreamK.interleave streamLen 1
    , benchIO "mergeMapWith interleave outer=inner=(sqrt Max)"
        $ mergeMapWith StreamK.interleave streamLen2 streamLen2
    , benchIO "mergeMapWith interleave outer=1 inner=Max"
        $ mergeMapWith StreamK.interleave 1 streamLen

    , benchIO "mergeMapWithD D.interleave outer=inner=(sqrt Max)"
        $ mergeMapWithD Stream.interleave streamLen2 streamLen2
    , benchIO "mergeMapWithD D.roundRobin outer=inner=(sqrt Max)"
        $ mergeMapWithD Stream.roundRobin streamLen2 streamLen2

    , benchIO "mergeMapWith (mergeBy compare) outer=Max inner=1"
        $ mergeMapWith (StreamK.mergeBy compare) streamLen 1
    , benchIO "mergeMapWith (mergeBy compare) outer=inner=(sqrt Max)"
        $ mergeMapWith (StreamK.mergeBy compare) streamLen2 streamLen2
    , benchIO "mergeMapWith (mergeBy compare) outer=1 inner=Max"
        $ mergeMapWith (StreamK.mergeBy compare) 1 streamLen

    , benchIO "mergeMapWith (mergeBy (flip compare)) outer=Max inner=1"
        $ mergeMapWith (StreamK.mergeBy (flip compare)) streamLen 1
    , benchIO "mergeMapWith (mergeBy (flip compare)) outer=inner=(sqrt Max)"
        $ mergeMapWith (StreamK.mergeBy (flip compare)) streamLen2 streamLen2
    , benchIO "mergeMapWith (mergeBy (flip compare)) outer=1 inner=Max"
        $ mergeMapWith (StreamK.mergeBy (flip compare)) 1 streamLen

      {- -- This fails with stack overflow.
        benchIO "concatMapWithZip (n of 1)"
        (concatMapWithZip value 1)
    -- Not correct because of nil stream at end issue.
    , benchIO "concatMapWithZip (sqrtVal of sqrtVal)"
        (concatMapWithZip sqrtVal sqrtVal)
    -}
    , benchIO "mergeMapWith (zipWith (+)) outer=Max inner=1"
        $ mergeMapWith (StreamK.zipWith (+)) streamLen 1
    , benchIO "mergeMapWith (zipWith (+)) outer=inner=(sqrt Max)"
        $ mergeMapWith (StreamK.zipWith (+)) streamLen2 streamLen2
    ]

    where

    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop

{- HLINT ignore "Use sort" -}
o_n_heap_sorting :: Int -> [Benchmark]
o_n_heap_sorting streamLen =
    [ benchIO "sortBy compare"           $ sortBy compare streamLen
    , benchIO "sortBy (flip compare)"    $ sortBy (flip compare) streamLen
    , benchIO "sortBy compare randomized" $ sortByCompareRandomized streamLen
    , bench "List.sortBy compare"
        $ nf (\x -> List.sortBy compare [1..x]) streamLen
    , bench "List.sortBy (flip compare)"
        $ nf (\x -> List.sortBy (flip compare) [1..x]) streamLen
    , bench "sortByLists compare randomized"
        $ nf (\x -> List.sortBy compare
                (List.map (\n -> if even n then n + 2 else n) [1..x])
             )
             streamLen
    ]

o_1_space_filtering :: Int -> [Benchmark]
o_1_space_filtering streamLen =
    [ benchIO "filter-even"     $ filterEven 1 streamLen
    , benchIO "filter-all-out"  $ filterAllOut 1 streamLen
    , benchIO "filter-all-in"   $ filterAllIn 1 streamLen
    , benchIO "take-all"        $ takeAll 1 streamLen
    , benchIO "takeWhile-true"  $ takeWhileTrue 1 streamLen
    , benchIO "drop-one"        $ dropOne 1 streamLen
    , benchIO "drop-all"        $ dropAll 1 streamLen
    , benchIO "dropWhile-true"  $ dropWhileTrue 1 streamLen
    , benchIO "dropWhile-false" $ dropWhileFalse 1 streamLen
    ]

o_1_space_filteringX4 :: Int -> [Benchmark]
o_1_space_filteringX4 streamLen =
    [ benchIO "filter-evenX4"     $ filterEven 4 streamLen
    , benchIO "filter-all-outX4"  $ filterAllOut 4 streamLen
    , benchIO "filter-all-inX4"   $ filterAllIn 4 streamLen
    , benchIO "take-allX4"        $ takeAll 4 streamLen
    , benchIO "takeWhile-trueX4"  $ takeWhileTrue 4 streamLen
    , benchIO "drop-oneX4"        $ dropOne 4 streamLen
    , benchIO "drop-allX4"        $ dropAll 4 streamLen
    , benchIO "dropWhile-trueX4"  $ dropWhileTrue 4 streamLen
    , benchIO "dropWhile-falseX4" $ dropWhileFalse 4 streamLen
    ]

o_1_space_mixed :: Int -> [Benchmark]
o_1_space_mixed streamLen =
    [ benchIO "scan-map"    $ scanMap 1 streamLen
    , benchIO "drop-map"    $ dropMap 1 streamLen
    , benchIO "drop-scan"   $ dropScan 1 streamLen
    , benchIO "take-drop"   $ takeDrop 1 streamLen
    , benchIO "take-scan"   $ takeScan 1 streamLen
    , benchIO "take-map"    $ takeMap 1 streamLen
    , benchIO "filter-drop" $ filterDrop 1 streamLen
    , benchIO "filter-take" $ filterTake 1 streamLen
    , benchIO "filter-scan" $ filterScan 1 streamLen
    , benchIO "filter-map"  $ filterMap 1 streamLen
    ]

o_1_space_mixedX2 :: Int -> [Benchmark]
o_1_space_mixedX2 streamLen =
    [ benchIO "scan-mapX2"    $ scanMap 2 streamLen
    , benchIO "drop-mapX2"    $ dropMap 2 streamLen
    , benchIO "drop-scanX2"   $ dropScan 2 streamLen
    , benchIO "take-dropX2"   $ takeDrop 2 streamLen
    , benchIO "take-scanX2"   $ takeScan 2 streamLen
    , benchIO "take-mapX2"    $ takeMap 2 streamLen
    , benchIO "filter-dropX2" $ filterDrop 2 streamLen
    , benchIO "filter-takeX2" $ filterTake 2 streamLen
    , benchIO "filter-scanX2" $ filterScan 2 streamLen
    , benchIO "filter-mapX2"  $ filterMap 2 streamLen
    ]

o_1_space_mixedX4 :: Int -> [Benchmark]
o_1_space_mixedX4 streamLen =
    [ benchIO "scan-mapX4"    $ scanMap 4 streamLen
    , benchIO "drop-mapX4"    $ dropMap 4 streamLen
    , benchIO "drop-scanX4"   $ dropScan 4 streamLen
    , benchIO "take-dropX4"   $ takeDrop 4 streamLen
    , benchIO "take-scanX4"   $ takeScan 4 streamLen
    , benchIO "take-mapX4"    $ takeMap 4 streamLen
    , benchIO "filter-dropX4" $ filterDrop 4 streamLen
    , benchIO "filter-takeX4" $ filterTake 4 streamLen
    , benchIO "filter-scanX4" $ filterScan 4 streamLen
    , benchIO "filter-mapX4"  $ filterMap 4 streamLen
    ]

o_1_space_list :: Int -> [Benchmark]
o_1_space_list streamLen =
    [ benchIO "list last"          $ lastList streamLen
    , benchIO "list ap drain2"      $ listApDrain2 streamLen2
    , benchIO "list monad drain2"        $ listMonadDrain2 streamLen2
    , benchIO "list monad drain3"        $ listMonadDrain3 streamLen3
    , benchIO "list monad filterAllIn2"  $ listMonadFilterAllIn2 streamLen2
    , benchIO "list monad filterAllOut2" $ listMonadFilterAllOut2 streamLen2
    ]
    where

    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop
    streamLen3 = round (P.fromIntegral streamLen**(1/3::P.Double)) -- triple nested loop

o_n_heap_transformation :: Int -> [Benchmark]
o_n_heap_transformation streamLen =
    [ benchIO "foldlS" $ foldlS 1 streamLen
    ]

o_n_stack_transformation :: Int -> [Benchmark]
o_n_stack_transformation streamLen =
    -- XXX why do these need so much stack
    [ benchIO "intersperse"     $ intersperse streamLen 1 streamLen2
    , benchIO "interspersePure" $ interspersePure streamLen 1 streamLen2
    ]
    where
    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double))

o_n_stack_transformationX4 :: Int -> [Benchmark]
o_n_stack_transformationX4 streamLen =
    [ benchIO "intersperseX4" $ intersperse streamLen 4 streamLen16
    ]
    where
    streamLen16 = round (P.fromIntegral streamLen**(1/16::P.Double))

o_n_stack_iterated :: Int -> Int -> Int -> [Benchmark]
o_n_stack_iterated streamLen iterStreamLen maxIters =
    [ benchIO "iterated mapM"                 $ iterateMapM iterStreamLen maxIters
    , benchIO "iterated scan(1/10)"           $ iterateScan iterStreamLen maxIters
    , benchIO "iterated filterEven"           $ iterateFilterEven iterStreamLen maxIters
    , benchIO "iterated takeAll"              $ iterateTakeAll streamLen iterStreamLen maxIters
    , benchIO "iterated dropOne"              $ iterateDropOne iterStreamLen maxIters
    , benchIO "iterated dropWhileFalse(1/10)" $ iterateDropWhileFalse streamLen iterStreamLen maxIters
    , benchIO "iterated dropWhileTrue"        $ iterateDropWhileTrue streamLen iterStreamLen maxIters
    ]

benchmarks :: Int -> Int -> Int -> [(SpaceComplexity, Benchmark)]
benchmarks streamLen iterStreamLen maxIters =
    -- O(1) space
       fmap (SpaceO_1,) (concat
            [ o_1_space_generation streamLen
            , o_1_space_elimination streamLen
            , o_1_space_ap streamLen
            , o_1_space_monad streamLen
            , o_1_space_bind streamLen
            , o_1_space_transformation streamLen
            , o_1_space_transformationX4 streamLen
            , o_1_space_concat streamLen
            , o_1_space_filtering streamLen
            , o_1_space_filteringX4 streamLen
            , o_1_space_joining streamLen
            , o_1_space_mixed streamLen
            , o_1_space_mixedX2 streamLen
            , o_1_space_mixedX4 streamLen
            , o_1_space_list streamLen
            ])
    -- O(n) heap
    ++ fmap (HeapO_n,) (concat
            [ o_n_heap_transformation streamLen
            , o_n_heap_concat streamLen
            , o_n_heap_sorting streamLen
            ])
    -- O(n) stack
    ++ fmap (StackO_n,)
            (  [ benchIO "tail"     $ tail streamLen
               , benchIO "nullTail" $ nullTail streamLen
               , benchIO "headTail" $ headTail streamLen
               ]
            ++ o_n_stack_transformation streamLen
            ++ o_n_stack_transformationX4 streamLen
            ++ o_n_stack_iterated streamLen iterStreamLen maxIters
            )
    -- O(n) space
    ++ fmap (SpaceO_n,)
            (  [ benchIO "toList" $ toList streamLen ]
            ++ o_n_space_concat streamLen
            )

main :: IO ()
main = do
    runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks streamLen =
        let !iterStreamLen = 10
            !maxIters = streamLen `div` iterStreamLen
            allBenches = benchmarks streamLen iterStreamLen maxIters
            get x = P.map snd $ filter ((==) x . fst) allBenches
            o1 = get SpaceO_1
            o_n_heap = get HeapO_n
            o_n_stack = get StackO_n
            o_n_space = get SpaceO_n
        in
        [ bgroup (o_1_space_prefix moduleName) o1
        , bgroup (o_n_stack_prefix moduleName) o_n_stack
        , bgroup (o_n_heap_prefix moduleName) o_n_heap
        , bgroup (o_n_space_prefix moduleName) o_n_space
        ]
