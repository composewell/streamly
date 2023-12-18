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

-- import Control.Applicative (liftA2)
-- import Control.Monad (when)
-- import Data.Maybe (isJust)
-- import System.Random (randomRIO)
import Prelude hiding
    ( tail, mapM_, foldl, last, map, mapM, concatMap, zipWith, init, iterate
    , repeat, replicate
    )

import qualified Prelude as P
-- import qualified Data.List as List

import qualified Streamly.Internal.Data.StreamK as S
import qualified Streamly.Internal.Data.Stream as D
import qualified Streamly.Internal.Data.Fold as Fold

import Test.Tasty.Bench (bgroup, Benchmark, defaultMain)

import Streamly.Benchmark.Common

#ifdef INSPECTION
import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream m a = S.Stream m a

{-# INLINE unfoldrD #-}
unfoldrD :: Monad m => Int -> Int -> Stream m Int
unfoldrD streamLen n = D.toStreamK (D.unfoldr step n)
    where
    step cnt =
        if cnt > n + streamLen
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE unfoldrMD #-}
unfoldrMD :: Monad m => Int -> Int -> Stream m Int
unfoldrMD streamLen n = D.toStreamK (D.unfoldrM step n)
    where
    step cnt =
        if cnt > n + streamLen
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-
{-# INLINE unfoldrK #-}
unfoldrK :: Int -> Int -> Stream m Int
unfoldrK streamLen n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + streamLen
        then Nothing
        else Just (cnt, cnt + 1)
-}

{-# INLINE unfoldrMK #-}
unfoldrMK :: Monad m => Int -> Int -> Stream m Int
unfoldrMK streamLen n = S.unfoldrMWith S.consM step n
    where
    step cnt =
        if cnt > n + streamLen
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE repeat #-}
repeat :: Monad m => Int -> Int -> Stream m Int
repeat streamLen = S.take streamLen . D.toStreamK . D.repeat

{-# INLINE repeatM #-}
repeatM :: Monad m => Int -> Int -> Stream m Int
repeatM streamLen = S.take streamLen . D.toStreamK . D.repeatM . return

{-# INLINE replicate #-}
replicate :: Monad m => Int -> Int -> Stream m Int
replicate x y = D.toStreamK $ D.replicate x y

{-# INLINE replicateM #-}
replicateM :: Monad m => Int -> Int -> Stream m Int
replicateM streamLen = D.toStreamK . D.replicateM streamLen . return

{-# INLINE iterate #-}
iterate :: Monad m => Int -> Int -> Stream m Int
iterate streamLen = S.take streamLen . D.toStreamK . D.iterate (+1)

{-# INLINE iterateM #-}
iterateM :: Monad m => Int -> Int -> Stream m Int
iterateM streamLen = S.take streamLen . D.toStreamK . D.iterateM (return . (+1)) . return

{-# INLINE fromFoldable #-}
fromFoldable :: Int -> Int -> Stream m Int
fromFoldable streamLen n = S.fromFoldable [n..n+streamLen]

{-# INLINE fromFoldableM #-}
fromFoldableM :: Monad m => Int -> Int -> Stream m Int
fromFoldableM streamLen n =
    Prelude.foldr (S.consM . return) S.nil [n .. n + streamLen]

{-
{-# INLINABLE concatMapFoldableWith #-}
concatMapFoldableWith :: Foldable f
    => (Stream m b -> Stream m b -> Stream m b)
    -> (a -> Stream m b)
    -> f a
    -> Stream m b
concatMapFoldableWith f g = Prelude.foldr (f . g) S.nil
-}

{-# INLINE concatMapFoldableSerial #-}
concatMapFoldableSerial :: Monad m => Int -> Int -> Stream m Int
concatMapFoldableSerial streamLen n =
    D.toStreamK $ D.concatMap D.fromPure $ D.fromStreamK $ S.fromList [n..n+streamLen]

{-# INLINE concatMapFoldableSerialM #-}
concatMapFoldableSerialM :: Monad m => Int -> Int -> Stream m Int
concatMapFoldableSerialM streamLen n =
    -- concatMapFoldableWith S.serial (S.fromEffect . return) [n..n+streamLen]
    D.toStreamK $ D.concatMap (D.fromEffect . return) $ D.fromStreamK $ S.fromList [n..n+streamLen]

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE drainD #-}
drainD :: Monad m => Stream m a -> m ()
drainD = D.drain . D.fromStreamK

{-# INLINE drain #-}
drain :: Monad m => Stream m a -> m ()
drain = S.drain

{-# INLINE mapM_ #-}
mapM_ :: Monad m => Stream m a -> m ()
mapM_ s = D.mapM_ (\_ -> return ()) $ D.fromStreamK s

{-
{-# INLINE uncons #-}
uncons :: Monad m => Stream m Int -> m ()
uncons s = do
    r <- D.uncons $ D.fromStreamK s
    case r of
        Nothing -> return ()
        Just (_, t) -> uncons (D.toStreamK t)

{-# INLINE init #-}
init :: Monad m => Stream m a -> m ()
init s = do
    t <- S.init s
    P.mapM_ S.drain t

{-# INLINE tail #-}
tail :: Monad m => Stream m a -> m ()
tail s = S.tail s >>= P.mapM_ tail

{-# INLINE nullTail #-}
nullTail :: Monad m => Stream m Int -> m ()
nullTail s = do
    r <- S.null s
    when (not r) $ S.tail s >>= P.mapM_ nullTail

{-# INLINE headTail #-}
headTail :: Monad m => Stream m Int -> m ()
headTail s = do
    h <- S.head s
    when (isJust h) $ S.tail s >>= P.mapM_ headTail
-}

{-# INLINE toList #-}
toList :: Monad m => Stream m Int -> m [Int]
toList = D.fold Fold.toList . D.fromStreamK

{-# INLINE foldl' #-}
foldl' :: Monad m => Stream m Int -> m Int
foldl' = D.fold (Fold.foldl' (+) 0) . D.fromStreamK

{-# INLINE last #-}
last :: Monad m => Stream m Int -> m (Maybe Int)
last = D.fold Fold.latest . D.fromStreamK

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE composeN #-}
composeN
    :: Monad m
    => Int -> (Stream m Int -> Stream m Int) -> Stream m Int -> m ()
composeN n f =
    case n of
        1 -> drain . f
        2 -> drain . f . f
        3 -> drain . f . f . f
        4 -> drain . f . f . f . f
        _ -> undefined

{-# INLINE scanl' #-}
scanl' :: Monad m => Int -> Stream m Int -> m ()
scanl' n =
    composeN n (D.toStreamK . D.scan (Fold.foldl' (+) 0) . D.fromStreamK)

{-# INLINE map #-}
map :: Monad m => Int -> Stream m Int -> m ()
map n = composeN n (D.toStreamK . D.map (+ 1) . D.fromStreamK)

{-
{-# INLINE fmapK #-}
fmapK :: Monad m => Int -> Stream m Int -> m ()
fmapK n = composeN n $ P.fmap (+ 1)
-}

{-# INLINE mapM #-}
mapM :: Monad m => Int -> Stream m Int -> m ()
mapM n = composeN n (D.toStreamK . D.mapM return . D.fromStreamK)

{-
{-# INLINE mapMSerial #-}
mapMSerial :: S.MonadAsync m => Int -> Stream m Int -> m ()
mapMSerial n = composeN n $ S.mapMSerial return
-}

{-# INLINE filterEven #-}
filterEven :: Monad m => Int -> Stream m Int -> m ()
filterEven n = composeN n (D.toStreamK . D.filter even . D.fromStreamK)

{-
{-# INLINE filterAllOut #-}
filterAllOut :: Monad m => Int -> Int -> Stream m Int -> m ()
filterAllOut streamLen n = composeN n $ S.filter (> streamLen)

{-# INLINE filterAllIn #-}
filterAllIn :: Monad m => Int -> Int -> Stream m Int -> m ()
filterAllIn streamLen n = composeN n $ S.filter (<= streamLen)

{-# INLINE _takeOne #-}
_takeOne :: Monad m => Int -> Stream m Int -> m ()
_takeOne n = composeN n $ S.take 1

{-# INLINE takeAll #-}
takeAll :: Monad m => Int -> Int -> Stream m Int -> m ()
takeAll streamLen n = composeN n $ S.take streamLen

{-# INLINE takeWhileTrue #-}
takeWhileTrue :: Monad m => Int -> Int -> Stream m Int -> m ()
takeWhileTrue streamLen n = composeN n $ S.takeWhile (<= streamLen)

{-# INLINE dropOne #-}
dropOne :: Monad m => Int -> Stream m Int -> m ()
dropOne n = composeN n $ S.drop 1

{-# INLINE dropAll #-}
dropAll :: Monad m => Int -> Int -> Stream m Int -> m ()
dropAll streamLen n = composeN n $ S.drop streamLen

{-# INLINE dropWhileTrue #-}
dropWhileTrue :: Monad m => Int -> Int -> Stream m Int -> m ()
dropWhileTrue streamLen n = composeN n $ S.dropWhile (<= streamLen)

{-# INLINE dropWhileFalse #-}
dropWhileFalse :: Monad m => Int -> Stream m Int -> m ()
dropWhileFalse n = composeN n $ S.dropWhile (<= 1)
-}

{-
{-# INLINE foldrS #-}
foldrS :: Monad m => Int -> Stream m Int -> m ()
foldrS n = composeN n $ S.foldrS S.cons S.nil

{-# INLINE foldlS #-}
foldlS :: Monad m => Int -> Stream m Int -> m ()
foldlS n = composeN n $ S.foldlS (flip S.cons) S.nil
-}

{-
{-# INLINE intersperse #-}
intersperse :: S.MonadAsync m => Int -> Int -> Stream m Int -> m ()
intersperse streamLen n = composeN n $ S.intersperse streamLen
-}

-------------------------------------------------------------------------------
-- Iteration
-------------------------------------------------------------------------------

{-
{-# INLINE iterateSource #-}
iterateSource
    :: S.MonadAsync m
    => Int -> (Stream m Int -> Stream m Int) -> Int -> Int -> Stream m Int
iterateSource iterStreamLen g i n = f i (unfoldrM iterStreamLen n)
    where
        f (0 :: Int) m = g m
        f x m = g (f (x P.- 1) m)

-- this is quadratic
{-# INLINE iterateScan #-}
iterateScan :: S.MonadAsync m => Int -> Int -> Int -> Stream m Int
iterateScan iterStreamLen maxIters =
    iterateSource iterStreamLen (S.scanl' (+) 0) (maxIters `div` 10)

-- this is quadratic
{-# INLINE iterateDropWhileFalse #-}
iterateDropWhileFalse :: S.MonadAsync m => Int -> Int -> Int -> Int -> Stream m Int
iterateDropWhileFalse streamLen iterStreamLen maxIters =
    iterateSource iterStreamLen (S.dropWhile (> streamLen)) (maxIters `div` 10)

{-# INLINE iterateMapM #-}
iterateMapM :: S.MonadAsync m => Int -> Int -> Int -> Stream m Int
iterateMapM iterStreamLen =
    iterateSource iterStreamLen (S.mapMWith S.consM return)

{-# INLINE iterateFilterEven #-}
iterateFilterEven :: S.MonadAsync m => Int -> Int -> Int -> Stream m Int
iterateFilterEven iterStreamLen = iterateSource iterStreamLen (S.filter even)

{-# INLINE iterateTakeAll #-}
iterateTakeAll :: S.MonadAsync m => Int -> Int -> Int -> Int -> Stream m Int
iterateTakeAll streamLen iterStreamLen =
    iterateSource iterStreamLen (S.take streamLen)

{-# INLINE iterateDropOne #-}
iterateDropOne :: S.MonadAsync m => Int -> Int -> Int -> Stream m Int
iterateDropOne iterStreamLen = iterateSource iterStreamLen (S.drop 1)

{-# INLINE iterateDropWhileTrue #-}
iterateDropWhileTrue :: S.MonadAsync m =>
    Int -> Int -> Int -> Int -> Stream m Int
iterateDropWhileTrue streamLen iterStreamLen =
    iterateSource iterStreamLen (S.dropWhile (<= streamLen))
-}

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

{-
{-# INLINE zipWith #-}
zipWith :: Monad m => Stream m Int -> m ()
zipWith src = drain $ S.zipWith (,) src src

{-# INLINE zipWithM #-}
zipWithM :: Monad m => Stream m Int -> m ()
zipWithM src = drain $ S.zipWithM (curry return) src src

{-# INLINE sortByK #-}
sortByK :: (a -> a -> Ordering) -> Stream m a -> Stream m a
sortByK f = S.concatPairsWith (S.mergeBy f) S.fromPure

{-# INLINE sortBy #-}
sortBy :: Monad m => Stream m Int -> m ()
sortBy = drain . sortByK compare

-------------------------------------------------------------------------------
-- Mixed Composition
-------------------------------------------------------------------------------

{-# INLINE scanMap #-}
scanMap :: Monad m => Int -> Stream m Int -> m ()
scanMap n = composeN n $ S.map (subtract 1) . S.scanl' (+) 0

{-# INLINE dropMap #-}
dropMap :: Monad m => Int -> Stream m Int -> m ()
dropMap n = composeN n $ S.map (subtract 1) . S.drop 1

{-# INLINE dropScan #-}
dropScan :: Monad m => Int -> Stream m Int -> m ()
dropScan n = composeN n $ S.scanl' (+) 0 . S.drop 1

{-# INLINE takeDrop #-}
takeDrop :: Monad m => Int -> Int -> Stream m Int -> m ()
takeDrop streamLen n = composeN n $ S.drop 1 . S.take streamLen

{-# INLINE takeScan #-}
takeScan :: Monad m => Int -> Int -> Stream m Int -> m ()
takeScan streamLen n = composeN n $ S.scanl' (+) 0 . S.take streamLen

{-# INLINE takeMap #-}
takeMap :: Monad m => Int -> Int -> Stream m Int -> m ()
takeMap streamLen n = composeN n $ S.map (subtract 1) . S.take streamLen

{-# INLINE filterDrop #-}
filterDrop :: Monad m => Int -> Int -> Stream m Int -> m ()
filterDrop streamLen n = composeN n $ S.drop 1 . S.filter (<= streamLen)

{-# INLINE filterTake #-}
filterTake :: Monad m => Int -> Int -> Stream m Int -> m ()
filterTake streamLen n = composeN n $ S.take streamLen . S.filter (<= streamLen)

{-# INLINE filterScan #-}
filterScan :: Monad m => Int -> Stream m Int -> m ()
filterScan n = composeN n $ S.scanl' (+) 0 . S.filter (<= maxBound)

{-# INLINE filterMap #-}
filterMap :: Monad m => Int -> Int -> Stream m Int -> m ()
filterMap streamLen n = composeN n $ S.map (subtract 1) . S.filter (<= streamLen)
-}

-------------------------------------------------------------------------------
-- ConcatMap
-------------------------------------------------------------------------------

-- concatMap unfoldrM/unfoldrM

{-# INLINE concatMap #-}
concatMap :: Int -> Int -> Int -> IO ()
concatMap outer inner n =
    S.drain $ D.toStreamK $ D.concatMap
        (\_ -> D.fromStreamK $ unfoldrMK inner n)
        (D.fromStreamK $ unfoldrMK outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMap
#endif

-- concatMap unfoldr/unfoldr

{-
{-# INLINE concatMapPure #-}
concatMapPure :: Int -> Int -> Int -> IO ()
concatMapPure outer inner n =
    S.drain $ S.concatMap
        (\_ -> unfoldr inner n)
        (unfoldr outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapPure
#endif

-- concatMap replicate/unfoldrM

{-# INLINE concatMapRepl #-}
concatMapRepl :: Int -> Int -> Int -> IO ()
concatMapRepl outer inner n =
    S.drain $ S.concatMap (S.replicate inner) (unfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapRepl
#endif

-- concatMapWith

{-# INLINE sourceConcatMapId #-}
sourceConcatMapId :: Monad m
    => Int -> Int -> Stream m (Stream m Int)
sourceConcatMapId val n =
    S.fromFoldable $ fmap (S.fromEffect . return) [n..n+val]

{-# INLINE concatMapBySerial #-}
concatMapBySerial :: Int -> Int -> Int -> IO ()
concatMapBySerial outer inner n =
    S.drain $ S.concatMapWith S.serial
        (unfoldrM inner)
        (unfoldrM outer n)
-}

{-
-------------------------------------------------------------------------------
-- Nested Composition
-------------------------------------------------------------------------------

instance Monad m => Applicative (S.Stream m) where
    {-# INLINE pure #-}
    pure = S.fromPure

    {-# INLINE (<*>) #-}
    (<*>) = S.crossApply

    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)

    {-# INLINE (*>) #-}
    (*>) = S.crossApplySnd

    {-# INLINE (<*) #-}
    (<*) = S.crossApplyFst

-- NOTE: even though concatMap for StreamD is 3x faster compared to StreamK,
-- the monad instance of StreamD is slower than StreamK after foldr/build
-- fusion.
instance Monad m => Monad (S.Stream m) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = flip S.concatMap

{-# INLINE drainApplicative #-}
drainApplicative :: Monad m => Stream m Int -> m ()
drainApplicative s = drain $ do
    (+) <$> s <*> s

{-# INLINE drainMonad #-}
drainMonad :: Monad m => Stream m Int -> m ()
drainMonad s = drain $ do
    x <- s
    y <- s
    return $ x + y

{-# INLINE drainMonad3 #-}
drainMonad3 :: Monad m => Stream m Int -> m ()
drainMonad3 s = drain $ do
    x <- s
    y <- s
    z <- s
    return $ x + y + z

{-# INLINE filterAllOutMonad #-}
filterAllOutMonad
    :: Monad m
    => Stream m Int -> m ()
filterAllOutMonad str = drain $ do
    x <- str
    y <- str
    let s = x + y
    if s < 0
    then return s
    else S.nil

{-# INLINE filterAllInMonad #-}
filterAllInMonad
    :: Monad m
    => Stream m Int -> m ()
filterAllInMonad str = drain $ do
    x <- str
    y <- str
    let s = x + y
    if s > 0
    then return s
    else S.nil

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
        -}

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Stream.ToStreamK"

-- Generation of StreamK using StreamD generation functions and eleminating
-- using StreamK drain.
o_1_space_generation :: Int -> Benchmark
o_1_space_generation streamLen =
    bgroup "generation"
        [ benchFold "unfoldr" drain (unfoldrD streamLen)
        , benchFold "unfoldrM" drain (unfoldrMD streamLen)
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

-- Generating using StreamK and eliminating using StreamD folds.
o_1_space_elimination :: Int -> Benchmark
o_1_space_elimination streamLen =
    bgroup "elimination"
        [ benchFold "toNull"   drainD   (unfoldrMK streamLen)
        , benchFold "mapM_"    mapM_    (unfoldrMK streamLen)
        -- , benchFold "uncons"   uncons   (unfoldrMK streamLen)
        -- , benchFold "init"   init     (unfoldrM streamLen)
        , benchFold "foldl'" foldl'    (unfoldrMK streamLen)
        , benchFold "last"   last     (unfoldrMK streamLen)
        ]

{-
o_1_space_nested :: Int -> Benchmark
o_1_space_nested streamLen =
    bgroup "nested"
        [ benchFold "drainApplicative" drainApplicative (unfoldrM streamLen2)
        , benchFold "drainMonad"   drainMonad   (unfoldrM streamLen2)
        , benchFold "drainMonad3"  drainMonad3  (unfoldrM streamLen3)
        , benchFold "filterAllInMonad"  filterAllInMonad  (unfoldrM streamLen2)
        , benchFold "filterAllOutMonad" filterAllOutMonad (unfoldrM streamLen2)
        , benchFold "drainApplicative (pure)" drainApplicative (unfoldr streamLen2)
        , benchFold "drainMonad (pure)"   drainMonad   (unfoldr streamLen2)
        , benchFold "drainMonad3 (pure)"  drainMonad3  (unfoldr streamLen3)
        , benchFold "filterAllInMonad (pure)"  filterAllInMonad  (unfoldr streamLen2)
        , benchFold "filterAllOutMonad (pure)" filterAllOutMonad (unfoldr streamLen2)
        ]
    where
    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop
    streamLen3 = round (P.fromIntegral streamLen**(1/3::P.Double)) -- triple nested loop
-}

-- Generate using StreamK and transform using StreamD transformation functions
-- and then drain using StreamK.
o_1_space_transformation :: Int -> Benchmark
o_1_space_transformation streamLen =
    bgroup "transformation"
        [ -- benchFold "foldrS" (foldrS 1) (unfoldrM streamLen)
          benchFold "scanl'"   (scanl' 1) (unfoldrMK streamLen)
        , benchFold "map"    (map  1) (unfoldrMK streamLen)
        -- , benchFold "fmap"   (fmapK 1) (unfoldrM streamLen)
        , benchFold "mapM"   (mapM 1) (unfoldrMK streamLen)
        -- , benchFold "mapMSerial"  (mapMSerial 1) (unfoldrM streamLen)
        ]

o_1_space_transformationX4 :: Int -> Benchmark
o_1_space_transformationX4 streamLen =
    bgroup "transformationX4"
        [ benchFold "scanl'"   (scanl' 4) (unfoldrMK streamLen)
        , benchFold "map"    (map  4) (unfoldrMK streamLen)
        -- , benchFold "fmap"   (fmapK 4) (unfoldrMK streamLen)
        , benchFold "mapM"   (mapM 4) (unfoldrMK streamLen)
        -- , benchFold "mapMSerial" (mapMSerial 4) (unfoldrM streamLen)
        -- XXX this is horribly slow
        -- , benchFold "concatMap" (concatMap 4) (unfoldrM streamLen16)
        ]

-- Generate using K, fold using K, concat using D.concatMap
o_1_space_concat :: Int -> Benchmark
o_1_space_concat streamLen =
    bgroup "concat"
        [ {- benchIOSrc1 "concatMapPure (n of 1)"
            (concatMapPure streamLen 1)
        , benchIOSrc1 "concatMapPure (sqrt n of sqrt n)"
            (concatMapPure streamLen2 streamLen2)
        , benchIOSrc1 "concatMapPure (1 of n)"
            (concatMapPure 1 streamLen)
            -}

          benchIOSrc1 "concatMap (n of 1)"
            (concatMap streamLen 1)
        , benchIOSrc1 "concatMap (sqrt n of sqrt n)"
            (concatMap streamLen2 streamLen2)
        , benchIOSrc1 "concatMap (1 of n)"
            (concatMap 1 streamLen)

{-
        , benchIOSrc1 "concatMapRepl (sqrt n of sqrt n)"
            (concatMapRepl streamLen2 streamLen2)

        -- This is for comparison with concatMapFoldableWith
        , benchIOSrc1 "concatMapWithId (n of 1) (fromFoldable)"
            (S.drain
                . S.concatMapWith S.serial id
                . sourceConcatMapId streamLen)

        , benchIOSrc1 "concatMapBy serial (n of 1)"
            (concatMapBySerial streamLen 1)
        , benchIOSrc1 "concatMapBy serial (sqrt n of sqrt n)"
            (concatMapBySerial streamLen2 streamLen2)
        , benchIOSrc1 "concatMapBy serial (1 of n)"
            (concatMapBySerial 1 streamLen)
        , benchFold "sortBy" sortBy (unfoldrM streamLen)
        -}
        ]
    where
    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop

o_1_space_filtering :: Int -> Benchmark
o_1_space_filtering streamLen =
    bgroup "filtering"
        [ benchFold "filter-even"     (filterEven     1) (unfoldrMK streamLen)
        {-
        , benchFold "filter-all-out"  (filterAllOut streamLen   1) (unfoldrM streamLen)
        , benchFold "filter-all-in"   (filterAllIn streamLen    1) (unfoldrM streamLen)
        , benchFold "take-all"        (takeAll streamLen        1) (unfoldrM streamLen)
        , benchFold "takeWhile-true"  (takeWhileTrue streamLen  1) (unfoldrM streamLen)
        , benchFold "drop-one"        (dropOne        1) (unfoldrM streamLen)
        , benchFold "drop-all"        (dropAll streamLen        1) (unfoldrM streamLen)
        , benchFold "dropWhile-true"  (dropWhileTrue streamLen  1) (unfoldrM streamLen)
        , benchFold "dropWhile-false" (dropWhileFalse 1) (unfoldrM streamLen)
        -}
        ]

o_1_space_filteringX4 :: Int -> Benchmark
o_1_space_filteringX4 streamLen =
    bgroup "filteringX4"
        [ benchFold "filter-even"     (filterEven     4) (unfoldrMK streamLen)
        {-
        , benchFold "filter-all-out"  (filterAllOut streamLen   4) (unfoldrM streamLen)
        , benchFold "filter-all-in"   (filterAllIn streamLen    4) (unfoldrM streamLen)
        , benchFold "take-all"        (takeAll streamLen        4) (unfoldrM streamLen)
        , benchFold "takeWhile-true"  (takeWhileTrue streamLen  4) (unfoldrM streamLen)
        , benchFold "drop-one"        (dropOne        4) (unfoldrM streamLen)
        , benchFold "drop-all"        (dropAll streamLen        4) (unfoldrM streamLen)
        , benchFold "dropWhile-true"  (dropWhileTrue streamLen  4) (unfoldrM streamLen)
        , benchFold "dropWhile-false" (dropWhileFalse 4) (unfoldrM streamLen)
        -}
        ]

{-
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
        -}

{-
o_1_space_list :: Int -> Benchmark
o_1_space_list streamLen =
    bgroup "list"
      [ bgroup "elimination"
        [ benchList "last" (\xs -> [List.last xs]) (unfoldrList streamLen)
        ]
      , bgroup "nested"
        [ benchList "toNullAp" toNullApNestedList (unfoldrList streamLen2)
        , benchList "toNull"   toNullNestedList (unfoldrList streamLen2)
        , benchList "toNull3"  toNullNestedList3 (unfoldrList streamLen3)
        , benchList "filterAllIn"  filterAllInNestedList (unfoldrList streamLen2)
        , benchList "filterAllOut"  filterAllOutNestedList (unfoldrList streamLen2)
        ]
      ]
    where

    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop
    streamLen3 = round (P.fromIntegral streamLen**(1/3::P.Double)) -- triple nested loop
-}

o_1_space :: Int -> Benchmark
o_1_space streamLen =
    bgroup (o_1_space_prefix moduleName)
      [ o_1_space_generation streamLen
      , o_1_space_elimination streamLen
      -- , o_1_space_nested streamLen
      , o_1_space_transformation streamLen
      , o_1_space_transformationX4 streamLen
      , o_1_space_concat streamLen
      , o_1_space_filtering streamLen
      , o_1_space_filteringX4 streamLen
      -- , o_1_space_zipping streamLen
      -- , o_1_space_mixed streamLen
      -- , o_1_space_mixedX2 streamLen
      -- , o_1_space_mixedX4 streamLen
      -- , o_1_space_list streamLen
      ]

{-
o_n_heap :: Int -> Benchmark
o_n_heap streamLen =
    bgroup (o_n_heap_prefix moduleName)
      [ bgroup "transformation"
        [ benchFold "foldlS" (foldlS 1) (unfoldrM streamLen)
        ]
      ]

{-# INLINE benchK #-}
benchK :: P.String -> (Int -> Stream P.IO Int) -> Benchmark
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
-}

o_n_space :: Int -> Benchmark
o_n_space streamLen =
    bgroup (o_n_space_prefix moduleName)
      [ bgroup "elimination"
        [ benchFold "toList" toList   (unfoldrMK streamLen)
        ]
      ]

{-
{-# INLINE benchList #-}
benchList :: P.String -> ([Int] -> [Int]) -> (Int -> [Int]) -> Benchmark
benchList name run f = bench name $ nfIO $ randomRIO (1,1) >>= return . run . f
-}

main :: IO ()
main =
    defaultMain
        [ o_1_space streamLen
        -- , o_n_stack streamLen iterStreamLen maxIters
        -- , o_n_heap streamLen
        , o_n_space streamLen
        ]

    where

    streamLen = 100000
    -- maxIters = 10000
    -- iterStreamLen = 10
