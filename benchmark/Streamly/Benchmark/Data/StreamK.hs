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
import Control.Monad (when)
import Data.Maybe (isJust)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.StreamK (StreamK)
import System.Random (randomRIO)
import Test.Tasty.Bench (bench, nfIO, bgroup, Benchmark)

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
import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

{-# INLINE unfoldr #-}
unfoldr :: Int -> Int -> StreamK m Int
unfoldr streamLen n = StreamK.unfoldr step n
    where
    step cnt =
        if cnt > n + streamLen
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE unfoldrM #-}
unfoldrM :: Monad m => Int -> Int -> StreamK m Int
unfoldrM streamLen n = StreamK.unfoldrMWith StreamK.consM step n
    where
    step cnt =
        if cnt > n + streamLen
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE repeat #-}
repeat :: Int -> Int -> StreamK m Int
repeat streamLen = StreamK.take streamLen . StreamK.repeat

{-# INLINE repeatM #-}
repeatM :: Monad m => Int -> Int -> StreamK m Int
repeatM streamLen = StreamK.take streamLen . StreamK.repeatM . return

{-# INLINE replicate #-}
replicate :: Int -> Int -> StreamK m Int
replicate = StreamK.replicate

{-# INLINE replicateM #-}
replicateM :: Monad m => Int -> Int -> StreamK m Int
replicateM streamLen = StreamK.replicateMWith StreamK.consM streamLen . return

{-# INLINE iterate #-}
iterate :: Int -> Int -> StreamK m Int
iterate streamLen = StreamK.take streamLen . StreamK.iterate (+1)

{-# INLINE iterateM #-}
iterateM :: Monad m => Int -> Int -> StreamK m Int
iterateM streamLen = StreamK.take streamLen . StreamK.iterateM (return . (+1)) . return

{-# INLINE fromFoldable #-}
fromFoldable :: Int -> Int -> StreamK m Int
fromFoldable streamLen n = StreamK.fromFoldable [n..n+streamLen]

{- HLINT ignore "Fuse foldr/fmap" -}
{-# INLINE fromFoldableM #-}
fromFoldableM :: Monad m => Int -> Int -> StreamK m Int
fromFoldableM streamLen n =
    List.foldr StreamK.consM StreamK.nil (Prelude.fmap return [n..n+streamLen])

{-# INLINABLE concatMapFoldableWith #-}
concatMapFoldableWith :: P.Foldable f
    => (StreamK m b -> StreamK m b -> StreamK m b)
    -> (a -> StreamK m b)
    -> f a
    -> StreamK m b
concatMapFoldableWith f g = P.foldr (f . g) StreamK.nil

{-# INLINE concatMapFoldableSerial #-}
concatMapFoldableSerial :: Int -> Int -> StreamK m Int
concatMapFoldableSerial streamLen n =
    concatMapFoldableWith StreamK.append StreamK.fromPure [n..n+streamLen]

{-# INLINE concatMapFoldableSerialM #-}
concatMapFoldableSerialM :: Monad m => Int -> Int -> StreamK m Int
concatMapFoldableSerialM streamLen n =
    concatMapFoldableWith StreamK.append (StreamK.fromEffect . return) [n..n+streamLen]

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE drain #-}
drain :: Monad m => StreamK m a -> m ()
drain = StreamK.drain

{-# INLINE mapM_ #-}
mapM_ :: Monad m => StreamK m a -> m ()
mapM_ = StreamK.mapM_ (\_ -> return ())

{-# INLINE uncons #-}
uncons :: Monad m => StreamK m Int -> m ()
uncons s = do
    r <- StreamK.uncons s
    case r of
        Nothing -> return ()
        Just (_, t) -> uncons t

{-# INLINE init #-}
init :: Monad m => StreamK m a -> m ()
init s = do
    t <- StreamK.init s
    P.mapM_ StreamK.drain t

{-# INLINE tail #-}
tail :: Monad m => StreamK m a -> m ()
tail s = StreamK.tail s >>= P.mapM_ tail

{-# INLINE nullTail #-}
nullTail :: Monad m => StreamK m Int -> m ()
nullTail s = do
    r <- StreamK.null s
    when (not r) $ StreamK.tail s >>= P.mapM_ nullTail

{-# INLINE headTail #-}
headTail :: Monad m => StreamK m Int -> m ()
headTail s = do
    h <- StreamK.head s
    when (isJust h) $ StreamK.tail s >>= P.mapM_ headTail

{-# INLINE toList #-}
toList :: Monad m => StreamK m Int -> m [Int]
toList = StreamK.toList

{-# INLINE foldl' #-}
foldl' :: Monad m => StreamK m Int -> m Int
foldl' = StreamK.foldl' (+) 0

{-# INLINE foldlM' #-}
foldlM' :: Monad m => StreamK m Int -> m Int
foldlM' = StreamK.foldlM' (\b a -> return (b + a)) (return 0)

{-# INLINE last #-}
last :: Monad m => StreamK m Int -> m (Maybe Int)
last = StreamK.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE composeN #-}
composeN
    :: Monad m
    => Int -> (StreamK m Int -> StreamK m Int) -> StreamK m Int -> m ()
composeN n f =
    case n of
        1 -> drain . f
        2 -> drain . f . f
        3 -> drain . f . f . f
        4 -> drain . f . f . f . f
        _ -> undefined

{-# INLINE scanl' #-}
scanl' :: Monad m => Int -> StreamK m Int -> m ()
scanl' n = composeN n $ StreamK.scanl' (+) 0

{-# INLINE map #-}
map :: Monad m => Int -> StreamK m Int -> m ()
map n = composeN n $ StreamK.map (+ 1)

{-# INLINE fmapK #-}
fmapK :: Monad m => Int -> StreamK m Int -> m ()
fmapK n = composeN n $ P.fmap (+ 1)

{-# INLINE mapM #-}
mapM :: Monad m => Int -> StreamK m Int -> m ()
mapM n = composeN n $ StreamK.mapMWith StreamK.consM return

{-# INLINE mapMSerial #-}
mapMSerial :: Monad m => Int -> StreamK m Int -> m ()
mapMSerial n = composeN n $ StreamK.mapMSerial return

{-# INLINE filterEven #-}
filterEven :: Monad m => Int -> StreamK m Int -> m ()
filterEven n = composeN n $ StreamK.filter even

{-# INLINE filterAllOut #-}
filterAllOut :: Monad m => Int -> Int -> StreamK m Int -> m ()
filterAllOut streamLen n = composeN n $ StreamK.filter (> streamLen)

{-# INLINE filterAllIn #-}
filterAllIn :: Monad m => Int -> Int -> StreamK m Int -> m ()
filterAllIn streamLen n = composeN n $ StreamK.filter (<= streamLen)

{-# INLINE _takeOne #-}
_takeOne :: Monad m => Int -> StreamK m Int -> m ()
_takeOne n = composeN n $ StreamK.take 1

{-# INLINE takeAll #-}
takeAll :: Monad m => Int -> Int -> StreamK m Int -> m ()
takeAll streamLen n = composeN n $ StreamK.take streamLen

{-# INLINE takeWhileTrue #-}
takeWhileTrue :: Monad m => Int -> Int -> StreamK m Int -> m ()
takeWhileTrue streamLen n = composeN n $ StreamK.takeWhile (<= streamLen)

{-# INLINE dropOne #-}
dropOne :: Monad m => Int -> StreamK m Int -> m ()
dropOne n = composeN n $ StreamK.drop 1

{-# INLINE dropAll #-}
dropAll :: Monad m => Int -> Int -> StreamK m Int -> m ()
dropAll streamLen n = composeN n $ StreamK.drop streamLen

{-# INLINE dropWhileTrue #-}
dropWhileTrue :: Monad m => Int -> Int -> StreamK m Int -> m ()
dropWhileTrue streamLen n = composeN n $ StreamK.dropWhile (<= streamLen)

{-# INLINE dropWhileFalse #-}
dropWhileFalse :: Monad m => Int -> StreamK m Int -> m ()
dropWhileFalse n = composeN n $ StreamK.dropWhile (<= 1)

{-# INLINE foldrS #-}
foldrS :: Monad m => Int -> StreamK m Int -> m ()
foldrS n = composeN n $ StreamK.foldrS StreamK.cons StreamK.nil

{-# INLINE foldlS #-}
foldlS :: Monad m => Int -> StreamK m Int -> m ()
foldlS n = composeN n $ StreamK.foldlS (flip StreamK.cons) StreamK.nil

{-# INLINE intersperse #-}
intersperse :: Monad m => Int -> Int -> StreamK m Int -> m ()
intersperse streamLen n = composeN n $ StreamK.intersperse streamLen

-------------------------------------------------------------------------------
-- Iteration
-------------------------------------------------------------------------------

{-# INLINE iterateSource #-}
iterateSource
    :: Monad m => Int -> (StreamK m Int -> StreamK m Int) -> Int -> Int -> StreamK m Int
iterateSource iterStreamLen g i n = f i (unfoldrM iterStreamLen n)
    where
        f (0 :: Int) m = g m
        f x m = g (f (x P.- 1) m)

-- this is quadratic
{-# INLINE iterateScan #-}
iterateScan :: Monad m => Int -> Int -> Int -> StreamK m Int
iterateScan iterStreamLen maxIters =
    iterateSource iterStreamLen (StreamK.scanl' (+) 0) (maxIters `div` 10)

-- this is quadratic
{-# INLINE iterateDropWhileFalse #-}
iterateDropWhileFalse :: Monad m => Int -> Int -> Int -> Int -> StreamK m Int
iterateDropWhileFalse streamLen iterStreamLen maxIters =
    iterateSource iterStreamLen (StreamK.dropWhile (> streamLen)) (maxIters `div` 10)

{-# INLINE iterateMapM #-}
iterateMapM :: Monad m => Int -> Int -> Int -> StreamK m Int
iterateMapM iterStreamLen =
    iterateSource iterStreamLen (StreamK.mapMWith StreamK.consM return)

{-# INLINE iterateFilterEven #-}
iterateFilterEven :: Monad m => Int -> Int -> Int -> StreamK m Int
iterateFilterEven iterStreamLen = iterateSource iterStreamLen (StreamK.filter even)

{-# INLINE iterateTakeAll #-}
iterateTakeAll :: Monad m => Int -> Int -> Int -> Int -> StreamK m Int
iterateTakeAll streamLen iterStreamLen =
    iterateSource iterStreamLen (StreamK.take streamLen)

{-# INLINE iterateDropOne #-}
iterateDropOne :: Monad m => Int -> Int -> Int -> StreamK m Int
iterateDropOne iterStreamLen = iterateSource iterStreamLen (StreamK.drop 1)

{-# INLINE iterateDropWhileTrue #-}
iterateDropWhileTrue ::
    Monad m => Int -> Int -> Int -> Int -> StreamK m Int
iterateDropWhileTrue streamLen iterStreamLen =
    iterateSource iterStreamLen (StreamK.dropWhile (<= streamLen))

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

{-# INLINE zipWith #-}
zipWith :: Monad m => StreamK m Int -> m ()
zipWith src = drain $ StreamK.zipWith (,) src src

{-# INLINE zipWithM #-}
zipWithM :: Monad m => StreamK m Int -> m ()
zipWithM src = drain $ StreamK.zipWithM (curry return) src src

{-# INLINE sortByK #-}
sortByK :: (a -> a -> Ordering) -> StreamK m a -> StreamK m a
sortByK f = StreamK.mergeMapWith (StreamK.mergeBy f) StreamK.fromPure

{-# INLINE sortBy #-}
sortBy :: Monad m => StreamK m Int -> m ()
sortBy = drain . sortByK compare

-------------------------------------------------------------------------------
-- Joining
-------------------------------------------------------------------------------

{-# INLINE interleave2 #-}
interleave2 :: Int -> Int -> IO ()
interleave2 value n =
    StreamK.drain $ StreamK.interleave
        (unfoldrM (value `div` 2) n)
        (unfoldrM (value `div` 2) (n + 1))

{-# INLINE concatMapWith #-}
concatMapWith
    :: (StreamK IO Int -> StreamK IO Int -> StreamK IO Int)
    -> Int
    -> Int
    -> Int
    -> IO ()
concatMapWith op outer inner n =
    StreamK.drain $ StreamK.concatMapWith op
        (unfoldrM inner)
        (unfoldrM outer n)

{-# INLINE concatMapWithD #-}
concatMapWithD
    :: (Stream IO Int -> Stream IO Int -> Stream IO Int)
    -> Int
    -> Int
    -> Int
    -> IO ()
concatMapWithD op outer inner n =
    StreamK.drain $ StreamK.concatMapWith op1
        (unfoldrM inner)
        (unfoldrM outer n)

    where

    op1 s1 s2 = StreamK.fromStream $ op (StreamK.toStream s1) (StreamK.toStream s2)

{-# INLINE mergeMapWith #-}
mergeMapWith
    :: (StreamK IO Int -> StreamK IO Int -> StreamK IO Int)
    -> Int
    -> Int
    -> Int
    -> IO ()
mergeMapWith op outer inner n =
    StreamK.drain $ StreamK.mergeMapWith op
        (unfoldrM inner)
        (unfoldrM outer n)

{-# INLINE mergeMapWithD #-}
mergeMapWithD
    :: (Stream IO Int -> Stream IO Int -> Stream IO Int)
    -> Int
    -> Int
    -> Int
    -> IO ()
mergeMapWithD op outer inner n =
    StreamK.drain $ StreamK.mergeMapWith op1
        (unfoldrM inner)
        (unfoldrM outer n)

    where

    op1 s1 s2 = StreamK.fromStream $ op (StreamK.toStream s1) (StreamK.toStream s2)

-------------------------------------------------------------------------------
-- Mixed Composition
-------------------------------------------------------------------------------

{-# INLINE scanMap #-}
scanMap :: Monad m => Int -> StreamK m Int -> m ()
scanMap n = composeN n $ StreamK.map (subtract 1) . StreamK.scanl' (+) 0

{-# INLINE dropMap #-}
dropMap :: Monad m => Int -> StreamK m Int -> m ()
dropMap n = composeN n $ StreamK.map (subtract 1) . StreamK.drop 1

{-# INLINE dropScan #-}
dropScan :: Monad m => Int -> StreamK m Int -> m ()
dropScan n = composeN n $ StreamK.scanl' (+) 0 . StreamK.drop 1

{-# INLINE takeDrop #-}
takeDrop :: Monad m => Int -> Int -> StreamK m Int -> m ()
takeDrop streamLen n = composeN n $ StreamK.drop 1 . StreamK.take streamLen

{-# INLINE takeScan #-}
takeScan :: Monad m => Int -> Int -> StreamK m Int -> m ()
takeScan streamLen n = composeN n $ StreamK.scanl' (+) 0 . StreamK.take streamLen

{-# INLINE takeMap #-}
takeMap :: Monad m => Int -> Int -> StreamK m Int -> m ()
takeMap streamLen n = composeN n $ StreamK.map (subtract 1) . StreamK.take streamLen

{-# INLINE filterDrop #-}
filterDrop :: Monad m => Int -> Int -> StreamK m Int -> m ()
filterDrop streamLen n = composeN n $ StreamK.drop 1 . StreamK.filter (<= streamLen)

{-# INLINE filterTake #-}
filterTake :: Monad m => Int -> Int -> StreamK m Int -> m ()
filterTake streamLen n = composeN n $ StreamK.take streamLen . StreamK.filter (<= streamLen)

{-# INLINE filterScan #-}
filterScan :: Monad m => Int -> StreamK m Int -> m ()
filterScan n = composeN n $ StreamK.scanl' (+) 0 . StreamK.filter (<= maxBound)

{-# INLINE filterMap #-}
filterMap :: Monad m => Int -> Int -> StreamK m Int -> m ()
filterMap streamLen n = composeN n $ StreamK.map (subtract 1) . StreamK.filter (<= streamLen)

-------------------------------------------------------------------------------
-- ConcatMap
-------------------------------------------------------------------------------

-- concatMap unfoldrM/unfoldrM

{-# INLINE concatMap #-}
concatMap :: Int -> Int -> Int -> IO ()
concatMap outer inner n =
    StreamK.drain $ StreamK.concatMap
        (\_ -> unfoldrM inner n)
        (unfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMap
#endif

-- concatMap unfoldr/unfoldr

{-# INLINE concatMapPure #-}
concatMapPure :: Int -> Int -> Int -> IO ()
concatMapPure outer inner n =
    StreamK.drain $ StreamK.concatMap
        (\_ -> unfoldr inner n)
        (unfoldr outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapPure
#endif

-- concatMap replicate/unfoldrM

{-# INLINE concatMapRepl #-}
concatMapRepl :: Int -> Int -> Int -> IO ()
concatMapRepl outer inner n =
    StreamK.drain $ StreamK.concatMap (StreamK.replicate inner) (unfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapRepl
#endif

-- concatMapWith

{-# INLINE sourceConcatMapId #-}
sourceConcatMapId :: Monad m
    => Int -> Int -> StreamK m (StreamK m Int)
sourceConcatMapId val n =
    StreamK.fromFoldable $ fmap (StreamK.fromEffect . return) [n..n+val]

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
drainApplicative :: Monad m => StreamK m Int -> m ()
drainApplicative s = drain $ do
    (+) <$> s <*> s

{-# INLINE drainMonad #-}
drainMonad :: Monad m => StreamK m Int -> m ()
drainMonad s = drain $ do
    x <- s
    y <- s
    return $ x + y

{-# INLINE drainConcatFor1 #-}
drainConcatFor1 :: Monad m => StreamK m Int -> m ()
drainConcatFor1 s = drain $ do
    StreamK.concatFor s $ \x ->
            StreamK.fromPure $ x + 1

{-# INLINE drainConcatFor #-}
drainConcatFor :: Monad m => StreamK m Int -> m ()
drainConcatFor s = drain $ do
    StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            StreamK.fromPure $ x + y

{-# INLINE drainConcatForM #-}
drainConcatForM :: Monad m => StreamK m Int -> m ()
drainConcatForM s = drain $ do
    StreamK.concatForM s $ \x ->
        pure $ StreamK.concatForM s $ \y ->
            pure $ StreamK.fromPure $ x + y

{-# INLINE drainMonad3 #-}
drainMonad3 :: Monad m => StreamK m Int -> m ()
drainMonad3 s = drain $ do
    x <- s
    y <- s
    z <- s
    return $ x + y + z

{-# INLINE drainConcatFor3 #-}
drainConcatFor3 :: Monad m => StreamK m Int -> m ()
drainConcatFor3 s = drain $ do
    StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            StreamK.concatFor s $ \z ->
                StreamK.fromPure $ x + y + z

{-# INLINE drainConcatFor3M #-}
drainConcatFor3M :: Monad m => StreamK m Int -> m ()
drainConcatFor3M s = drain $ do
    StreamK.concatForM s $ \x ->
        pure $ StreamK.concatForM s $ \y ->
            pure $ StreamK.concatForM s $ \z ->
                pure $ StreamK.fromPure $ x + y + z

{-# INLINE drainConcatFor4 #-}
drainConcatFor4 :: Monad m => StreamK m Int -> m ()
drainConcatFor4 s = drain $ do
    StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            StreamK.concatFor s $ \z ->
                StreamK.concatFor s $ \w ->
                    StreamK.fromPure $ x + y + z + w

{-# INLINE drainConcatFor5 #-}
drainConcatFor5 :: Monad m => StreamK m Int -> m ()
drainConcatFor5 s = drain $ do
    StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            StreamK.concatFor s $ \z ->
                StreamK.concatFor s $ \w ->
                    StreamK.concatFor s $ \u ->
                        StreamK.fromPure $ x + y + z + w + u

{-# INLINE filterAllOutMonad #-}
filterAllOutMonad
    :: Monad m
    => StreamK m Int -> m ()
filterAllOutMonad str = drain $ do
    x <- str
    y <- str
    let s = x + y
    if s < 0
    then return s
    else StreamK.nil

{-# INLINE filterAllOutConcatFor #-}
filterAllOutConcatFor
    :: Monad m
    => StreamK m Int -> m ()
filterAllOutConcatFor s = drain $ do
    StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            let s1 = x + y
             in if s1 < 0
                then StreamK.fromPure s1
                else StreamK.nil

{-# INLINE filterAllInMonad #-}
filterAllInMonad
    :: Monad m
    => StreamK m Int -> m ()
filterAllInMonad str = drain $ do
    x <- str
    y <- str
    let s = x + y
    if s > 0
    then return s
    else StreamK.nil

{-# INLINE filterAllInConcatFor #-}
filterAllInConcatFor
    :: Monad m
    => StreamK m Int -> m ()
filterAllInConcatFor s = drain $ do
    StreamK.concatFor s $ \x ->
        StreamK.concatFor s $ \y ->
            let s1 = x + y
             in if s1 > 0
                then StreamK.fromPure s1
                else StreamK.nil

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

{-# INLINE toNullApNestedList #-}
toNullApNestedList :: [Int] -> [Int]
toNullApNestedList s = (+) <$> s <*> s

{-# INLINE toNullNestedList #-}
toNullNestedList :: [Int] -> [Int]
toNullNestedList s = do
    x <- s
    y <- s
    return $ x + y

{-# INLINE toNullNestedList3 #-}
toNullNestedList3 :: [Int] -> [Int]
toNullNestedList3 s = do
    x <- s
    y <- s
    z <- s
    return $ x + y + z

{-# INLINE filterAllOutNestedList #-}
filterAllOutNestedList :: [Int] -> [Int]
filterAllOutNestedList str = do
    x <- str
    y <- str
    let s = x + y
    if s < 0
    then return s
    else []

{-# INLINE filterAllInNestedList #-}
filterAllInNestedList :: [Int] -> [Int]
filterAllInNestedList str = do
    x <- str
    y <- str
    let s = x + y
    if s > 0
    then return s
    else []

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.StreamK"

o_1_space_generation :: Int -> Benchmark
o_1_space_generation streamLen =
    bgroup "generation"
        [ benchFold "unfoldr" drain (unfoldr streamLen)
        , benchFold "unfoldrM" drain (unfoldrM streamLen)
        , benchFold "repeat" drain (repeat streamLen)
        , benchFold "repeatM" drain (repeatM streamLen)
        , benchFold "replicate" drain (replicate streamLen)
        , benchFold "replicateM" drain (replicateM streamLen)
        , benchFold "iterate" drain (iterate streamLen)
        , benchFold "iterateM" drain (iterateM streamLen)

        , benchFold "fromFoldable"  drain (fromFoldable streamLen)
        , benchFold "fromFoldableM" drain (fromFoldableM streamLen)

        -- appends
        , benchFold "concatMapFoldableWith"  drain (concatMapFoldableSerial streamLen)
        , benchFold "concatMapFoldableWithM" drain (concatMapFoldableSerialM streamLen)
        ]

o_1_space_elimination :: Int -> Benchmark
o_1_space_elimination streamLen =
    bgroup "elimination"
        [ benchFold "toNull"   drain   (unfoldrM streamLen)
        , benchFold "mapM_"    mapM_    (unfoldrM streamLen)
        , benchFold "uncons"   uncons   (unfoldrM streamLen)
        , benchFold "init"   init     (unfoldrM streamLen)
        , benchFold "foldl'" foldl'    (unfoldrM streamLen)
        , benchFold "foldlM'" foldlM' (unfoldrM streamLen)
        , benchFold "last"   last     (unfoldrM streamLen)
        ]

o_1_space_ap :: Int -> Benchmark
o_1_space_ap streamLen =
    bgroup "Applicative"
        [ benchFold "drain2" drainApplicative (unfoldrM streamLen2)
        , benchFold "pureDrain2" drainApplicative (unfoldr streamLen2)
        ]
    where
    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop

o_1_space_monad :: Int -> Benchmark
o_1_space_monad streamLen =
    bgroup "Monad"
        [ benchFold "drain2"   drainMonad   (unfoldrM streamLen2)
        , benchFold "drain3"  drainMonad3  (unfoldrM streamLen3)
        , benchFold "filterAllIn2"  filterAllInMonad  (unfoldrM streamLen2)
        , benchFold "filterAllOut2" filterAllOutMonad (unfoldrM streamLen2)
        , benchFold "pureDrain2"   drainMonad   (unfoldr streamLen2)
        , benchFold "pureDrain3"  drainMonad3  (unfoldr streamLen3)
        , benchFold "pureFilterAllIn2"  filterAllInMonad  (unfoldr streamLen2)
        , benchFold "pureFilterAllOut2" filterAllOutMonad (unfoldr streamLen2)
        ]
    where
    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop
    streamLen3 = round (P.fromIntegral streamLen**(1/3::P.Double)) -- triple nested loop

o_1_space_bind :: Int -> Benchmark
o_1_space_bind streamLen =
    bgroup "concatFor"
        [ benchFold "drain1"   drainConcatFor1   (unfoldrM streamLen)
        , benchFold "drain2"   drainConcatFor   (unfoldrM streamLen2)
        , benchFold "drainM2"   drainConcatForM   (unfoldrM streamLen2)
        , benchFold "drain3"   drainConcatFor3   (unfoldrM streamLen3)
        , benchFold "drain4"   drainConcatFor4   (unfoldrM streamLen4)
        , benchFold "drain5"   drainConcatFor5   (unfoldrM streamLen5)
        , benchFold "drainM3"   drainConcatFor3M   (unfoldrM streamLen3)
        , benchFold "filterAllIn2"  filterAllInConcatFor  (unfoldrM streamLen2)
        , benchFold "filterAllOut2" filterAllOutConcatFor (unfoldrM streamLen2)
        ]
    where
    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop
    streamLen3 = round (P.fromIntegral streamLen**(1/3::P.Double)) -- triple nested loop
    streamLen4 = round (P.fromIntegral streamLen**(1/4::P.Double)) -- 4 times nested loop
    streamLen5 = round (P.fromIntegral streamLen**(1/5::P.Double)) -- 5 times nested loop

o_1_space_transformation :: Int -> Benchmark
o_1_space_transformation streamLen =
    bgroup "transformation"
        [ benchFold "foldrS" (foldrS 1) (unfoldrM streamLen)
        , benchFold "scanl'"   (scanl' 1) (unfoldrM streamLen)
        , benchFold "map"    (map  1) (unfoldrM streamLen)
        , benchFold "fmap"   (fmapK 1) (unfoldrM streamLen)
        , benchFold "mapM"   (mapM 1) (unfoldrM streamLen)
        , benchFold "mapMSerial"  (mapMSerial 1) (unfoldrM streamLen)
        ]

o_1_space_transformationX4 :: Int -> Benchmark
o_1_space_transformationX4 streamLen =
    bgroup "transformationX4"
        [ benchFold "scanl'"   (scanl' 4) (unfoldrM streamLen)
        , benchFold "map"    (map  4) (unfoldrM streamLen)
        , benchFold "fmap"   (fmapK 4) (unfoldrM streamLen)
        , benchFold "mapM"   (mapM 4) (unfoldrM streamLen)
        , benchFold "mapMSerial" (mapMSerial 4) (unfoldrM streamLen)
        -- XXX this is horribly slow
        -- , benchFold "concatMap" (concatMap 4) (unfoldrM streamLen16)
        ]

o_1_space_joining :: Int -> Benchmark
o_1_space_joining streamLen =
    bgroup "joining"
        [ bgroup "(2 of n/2)"
            [ benchIOSrc1 "interleave" (interleave2 streamLen)

            -- join 2 streams using concatMapWith
            , benchIOSrc1
                  "concatMapWith interleave"
                  (concatMapWith StreamK.interleave 2 (streamLen `div` 2))
            , benchIOSrc1
                  "concatMapWith D.interleave"
                  (concatMapWithD Stream.interleave 2 (streamLen `div` 2))
            , benchIOSrc1
                  "concatMapWith D.roundRobin"
                  (concatMapWithD Stream.roundRobin 2 (streamLen `div` 2))

            -- join 2 streams using mergeMapWith
            , benchIOSrc1
                "mergeMapWith interleave"
                (mergeMapWith StreamK.interleave 2 (streamLen `div` 2))
            , benchIOSrc1
                "mergeMapWith D.interleave"
                (mergeMapWithD Stream.interleave 2 (streamLen `div` 2))
            , benchIOSrc1
                "mergeMapWith D.roundRobin"
                (mergeMapWithD Stream.roundRobin 2 (streamLen `div` 2))
            ]
        ]

o_1_space_concat :: Int -> Benchmark
o_1_space_concat streamLen =
    bgroup "concat"
        [ benchIOSrc1 "concatMapPure outer=Max inner=1"
            (concatMapPure streamLen 1)
        , benchIOSrc1 "concatMapPure outer=inner=(sqrt Max)"
            (concatMapPure streamLen2 streamLen2)
        , benchIOSrc1 "concatMapPure outer=1 inner=Max"
            (concatMapPure 1 streamLen)

        , benchIOSrc1 "concatMap outer=Max inner=1"
            (concatMap streamLen 1)
        , benchIOSrc1 "concatMap outer=inner=(sqrt Max)"
            (concatMap streamLen2 streamLen2)
        , benchIOSrc1 "concatMap outer=1 inner=Max"
            (concatMap 1 streamLen)

        , benchIOSrc1 "concatMapRepl outer=inner=(sqrt Max)"
            (concatMapRepl streamLen2 streamLen2)

        -- This is for comparison with concatMapFoldableWith
        , benchIOSrc1 "concatMapWithId outer=Max inner=1 (fromFoldable)"
            (StreamK.drain
                . StreamK.concatMapWith StreamK.append id
                . sourceConcatMapId streamLen)

        , benchIOSrc1 "concatMapWith append outer=Max inner=1"
            (concatMapWith StreamK.append streamLen 1)
        , benchIOSrc1 "concatMapWith append outer=inner=(sqrt Max)"
            (concatMapWith StreamK.append streamLen2 streamLen2)
        , benchIOSrc1 "concatMapWith append outer=1 inner=Max"
            (concatMapWith StreamK.append 1 streamLen)

        -- interleave with concatMapWith is O(1)
        , benchIOSrc1 "concatMapWith interleave outer=Max inner=1"
            (concatMapWith StreamK.interleave streamLen 1)
        , benchIOSrc1 "concatMapWith interleave outer=inner=(sqrt Max)"
            (concatMapWith StreamK.interleave streamLen2 streamLen2)
        , benchIOSrc1 "concatMapWith interleave outer=1 inner=Max"
            (concatMapWith StreamK.interleave 1 streamLen)
        ]

    where

    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop

o_n_space_concat :: Int -> Benchmark
o_n_space_concat streamLen =
    bgroup "concat"
        [
        -- concatMapWith using StreamD versions of interleave operations are
        -- all quadratic, we just measure the sqrtVal benchmark for comparison.
          benchIOSrc1 "concatMapWithD D.interleave outer=inner=(sqrt Max)"
            (concatMapWithD Stream.interleave streamLen2 streamLen2)
        , benchIOSrc1 "concatMapWithD D.roundRobin outer=inner=(sqrt Max)"
            (concatMapWithD Stream.roundRobin streamLen2 streamLen2)
        ]

    where

    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop

o_n_heap_concat :: Int -> Benchmark
o_n_heap_concat streamLen =
    bgroup "concat"
        [
          benchIOSrc1 "mergeMapWith interleave outer=Max inner=1"
            (mergeMapWith StreamK.interleave streamLen 1)
        , benchIOSrc1 "mergeMapWith interleave outer=inner=(sqrt Max)"
            (mergeMapWith StreamK.interleave streamLen2 streamLen2)
        , benchIOSrc1 "mergeMapWith interleave outer=1 inner=Max"
            (mergeMapWith StreamK.interleave 1 streamLen)

        , benchIOSrc1 "mergeMapWithD D.interleave outer=inner=(sqrt Max)"
            (mergeMapWithD Stream.interleave streamLen2 streamLen2)
        , benchIOSrc1 "mergeMapWithD D.roundRobin outer=inner=(sqrt Max)"
            (mergeMapWithD Stream.roundRobin streamLen2 streamLen2)
        ]

    where

    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop

o_1_space_filtering :: Int -> Benchmark
o_1_space_filtering streamLen =
    bgroup "filtering"
        [ benchFold "filter-even"     (filterEven     1) (unfoldrM streamLen)
        , benchFold "filter-all-out"  (filterAllOut streamLen   1) (unfoldrM streamLen)
        , benchFold "filter-all-in"   (filterAllIn streamLen    1) (unfoldrM streamLen)
        , benchFold "take-all"        (takeAll streamLen        1) (unfoldrM streamLen)
        , benchFold "takeWhile-true"  (takeWhileTrue streamLen  1) (unfoldrM streamLen)
        , benchFold "drop-one"        (dropOne        1) (unfoldrM streamLen)
        , benchFold "drop-all"        (dropAll streamLen        1) (unfoldrM streamLen)
        , benchFold "dropWhile-true"  (dropWhileTrue streamLen  1) (unfoldrM streamLen)
        , benchFold "dropWhile-false" (dropWhileFalse 1) (unfoldrM streamLen)
        ]

o_1_space_filteringX4 :: Int -> Benchmark
o_1_space_filteringX4 streamLen =
    bgroup "filteringX4"
        [ benchFold "filter-even"     (filterEven     4) (unfoldrM streamLen)
        , benchFold "filter-all-out"  (filterAllOut streamLen   4) (unfoldrM streamLen)
        , benchFold "filter-all-in"   (filterAllIn streamLen    4) (unfoldrM streamLen)
        , benchFold "take-all"        (takeAll streamLen        4) (unfoldrM streamLen)
        , benchFold "takeWhile-true"  (takeWhileTrue streamLen  4) (unfoldrM streamLen)
        , benchFold "drop-one"        (dropOne        4) (unfoldrM streamLen)
        , benchFold "drop-all"        (dropAll streamLen        4) (unfoldrM streamLen)
        , benchFold "dropWhile-true"  (dropWhileTrue streamLen  4) (unfoldrM streamLen)
        , benchFold "dropWhile-false" (dropWhileFalse 4) (unfoldrM streamLen)
        ]

o_1_space_zipping :: Int -> Benchmark
o_1_space_zipping streamLen =
    bgroup "zipping"
        [ benchFold "zipWith" zipWith (unfoldrM streamLen)
        , benchFold "zipWithM" zipWithM (unfoldrM streamLen)
        ]

o_1_space_mixed :: Int -> Benchmark
o_1_space_mixed streamLen =
    bgroup "mixed"
        [ benchFold "scan-map"    (scanMap    1) (unfoldrM streamLen)
        , benchFold "drop-map"    (dropMap    1) (unfoldrM streamLen)
        , benchFold "drop-scan"   (dropScan   1) (unfoldrM streamLen)
        , benchFold "take-drop"   (takeDrop streamLen   1) (unfoldrM streamLen)
        , benchFold "take-scan"   (takeScan streamLen   1) (unfoldrM streamLen)
        , benchFold "take-map"    (takeMap streamLen   1) (unfoldrM streamLen)
        , benchFold "filter-drop" (filterDrop streamLen 1) (unfoldrM streamLen)
        , benchFold "filter-take" (filterTake streamLen 1) (unfoldrM streamLen)
        , benchFold "filter-scan" (filterScan 1) (unfoldrM streamLen)
        , benchFold "filter-map"  (filterMap streamLen 1) (unfoldrM streamLen)
        ]

o_1_space_mixedX2 :: Int -> Benchmark
o_1_space_mixedX2 streamLen =
    bgroup "mixedX2"
        [ benchFold "scan-map"    (scanMap    2) (unfoldrM streamLen)
        , benchFold "drop-map"    (dropMap    2) (unfoldrM streamLen)
        , benchFold "drop-scan"   (dropScan   2) (unfoldrM streamLen)
        , benchFold "take-drop"   (takeDrop streamLen   2) (unfoldrM streamLen)
        , benchFold "take-scan"   (takeScan streamLen   2) (unfoldrM streamLen)
        , benchFold "take-map"    (takeMap streamLen   2) (unfoldrM streamLen)
        , benchFold "filter-drop" (filterDrop streamLen 2) (unfoldrM streamLen)
        , benchFold "filter-take" (filterTake streamLen 2) (unfoldrM streamLen)
        , benchFold "filter-scan" (filterScan 2) (unfoldrM streamLen)
        , benchFold "filter-map"  (filterMap streamLen 2) (unfoldrM streamLen)
        ]

o_1_space_mixedX4 :: Int -> Benchmark
o_1_space_mixedX4 streamLen =
    bgroup "mixedX4"
        [ benchFold "scan-map"    (scanMap    4) (unfoldrM streamLen)
        , benchFold "drop-map"    (dropMap    4) (unfoldrM streamLen)
        , benchFold "drop-scan"   (dropScan   4) (unfoldrM streamLen)
        , benchFold "take-drop"   (takeDrop streamLen   4) (unfoldrM streamLen)
        , benchFold "take-scan"   (takeScan streamLen   4) (unfoldrM streamLen)
        , benchFold "take-map"    (takeMap streamLen   4) (unfoldrM streamLen)
        , benchFold "filter-drop" (filterDrop streamLen 4) (unfoldrM streamLen)
        , benchFold "filter-take" (filterTake streamLen 4) (unfoldrM streamLen)
        , benchFold "filter-scan" (filterScan 4) (unfoldrM streamLen)
        , benchFold "filter-map"  (filterMap streamLen 4) (unfoldrM streamLen)
        ]

{- HLINT ignore "Use <&>" -}
{-# INLINE benchList #-}
benchList :: P.String -> ([Int] -> [Int]) -> (Int -> [Int]) -> Benchmark
benchList name run f = bench name $ nfIO $ randomRIO (1,1) >>= return . run . f

o_1_space_list :: Int -> Benchmark
o_1_space_list streamLen =
    bgroup "list"
      [ bgroup "elimination"
        [ benchList "last" (\xs -> [List.last xs]) (unfoldrList streamLen)
        ]
      , bgroup "Applicative"
        [ benchList "drain2" toNullApNestedList (unfoldrList streamLen2)
        ]
      , bgroup "Monad"
        [ benchList "drain2"   toNullNestedList (unfoldrList streamLen2)
        , benchList "drain3"  toNullNestedList3 (unfoldrList streamLen3)
        , benchList "filterAllIn2"  filterAllInNestedList (unfoldrList streamLen2)
        , benchList "filterAllOut2"  filterAllOutNestedList (unfoldrList streamLen2)
        ]
      ]
    where

    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop
    streamLen3 = round (P.fromIntegral streamLen**(1/3::P.Double)) -- triple nested loop

o_1_space :: Int -> Benchmark
o_1_space streamLen =
    bgroup (o_1_space_prefix moduleName)
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
      , o_1_space_zipping streamLen
      , o_1_space_joining streamLen
      , o_1_space_mixed streamLen
      , o_1_space_mixedX2 streamLen
      , o_1_space_mixedX4 streamLen
      , o_1_space_list streamLen
      ]

o_n_heap :: Int -> Benchmark
o_n_heap streamLen =
    bgroup (o_n_heap_prefix moduleName)
      [ bgroup "transformation"
        [ benchFold "foldlS" (foldlS 1) (unfoldrM streamLen)
        ]
      , bgroup "concat"
        [ benchFold "sortBy" sortBy (unfoldrM streamLen)
        ]
      , o_n_heap_concat streamLen
      ]

{-# INLINE benchK #-}
benchK :: P.String -> (Int -> StreamK P.IO Int) -> Benchmark
benchK name f = bench name $ nfIO $ randomRIO (1,1) >>= drain . f

o_n_stack :: Int -> Int -> Int -> Benchmark
o_n_stack streamLen iterStreamLen maxIters =
    bgroup (o_n_stack_prefix moduleName)
      [ bgroup "elimination"
        [ benchFold "tail"   tail     (unfoldrM streamLen)
        , benchFold "nullTail" nullTail (unfoldrM streamLen)
        , benchFold "headTail" headTail (unfoldrM streamLen)
        ]
      , bgroup "transformation"
        [
          -- XXX why do these need so much stack
          benchFold "intersperse" (intersperse streamLen 1) (unfoldrM streamLen2)
        , benchFold "interspersePure" (intersperse streamLen 1) (unfoldr streamLen2)
        ]
      , bgroup "transformationX4"
        [
          benchFold "intersperse" (intersperse streamLen 4) (unfoldrM streamLen16)
        ]
      , bgroup "iterated"
        [ benchK "mapM"                 (iterateMapM iterStreamLen maxIters)
        , benchK "scan(1/10)"           (iterateScan iterStreamLen maxIters)
        , benchK "filterEven"           (iterateFilterEven iterStreamLen maxIters)
        , benchK "takeAll"              (iterateTakeAll streamLen iterStreamLen maxIters)
        , benchK "dropOne"              (iterateDropOne iterStreamLen maxIters)
        , benchK "dropWhileFalse(1/10)" (iterateDropWhileFalse streamLen iterStreamLen maxIters)
        , benchK "dropWhileTrue"        (iterateDropWhileTrue streamLen iterStreamLen maxIters)
        ]
      ]
    where
    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop
    streamLen16 = round (P.fromIntegral streamLen**(1/16::P.Double)) -- triple nested loop

o_n_space :: Int -> Benchmark
o_n_space streamLen =
    bgroup (o_n_space_prefix moduleName)
      [ bgroup "elimination"
        [ benchFold "toList" toList   (unfoldrM streamLen)
        ]
      , o_n_space_concat streamLen
      ]

main :: IO ()
main = do
    runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks streamLen =
        let !iterStreamLen = 10
            !maxIters = streamLen `div` iterStreamLen
         in [ o_1_space streamLen
            , o_n_stack streamLen iterStreamLen maxIters
            , o_n_heap streamLen
            , o_n_space streamLen
            ]
