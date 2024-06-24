-- |
-- Module      : Streamly.Benchmark.Data.StreamK.FromStream
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
--
-- There are two ways to benchmark mixed use of Stream and StreamK.
--
-- First, benchmark one StreamK operation using all other operations from
-- Stream. This is for comparison of all pure StreamK operations vs using
-- operations from Stream whenever possible. We should compare these benchmarks
-- with pure StreamK benchmarks.
--
-- Second, benchmark one Stream operation with all other operations from
-- StreamK.
--
-- XXX This module uses both of the above, we need to separate the two in
-- separate benchmark groups.

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

import Control.DeepSeq (NFData(..))
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (StateT, get, put)
import Data.Functor.Identity (Identity, runIdentity)
import GHC.Exception (ErrorCall)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.StreamK (StreamK, CrossStreamK)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Random (randomRIO)
import Test.Tasty.Bench (bgroup, Benchmark, defaultMain, nf, nfIO, bench)

import qualified Prelude as P
-- import qualified Data.List as List
import qualified Control.Monad.State.Strict as State
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.StreamK as StreamK
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Fold as Fold

import Prelude hiding
    ( tail, mapM_, foldl, last, map, mapM, concatMap, zipWith, init, iterate
    , repeat, replicate, reverse
    )
import Streamly.Benchmark.Common
#ifdef INSPECTION
import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- Bench utilities
-------------------------------------------------------------------------------

{-# NOINLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchIOSrc #-}
benchIOSrc
    :: String
    -> (Int -> StreamK IO a)
    -> Benchmark
benchIOSrc name f =
    bench name $ nfIO $ randomRIO (1,1) >>= drain . f

{-# INLINE benchIOSink #-}
benchIOSink
    :: (NFData b)
    => Int -> String -> (StreamK IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . unfoldrMD value

-- XXX We should be using sourceUnfoldrM for fair comparison with IO monad, but
-- we can't use it as it requires MonadAsync constraint.
{-# INLINE benchIdentitySink #-}
benchIdentitySink
    :: (NFData b)
    => Int -> String -> (Stream Identity Int -> Identity b) -> Benchmark
benchIdentitySink value name f = bench name $ nf (f . unfoldr value) 1

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

{-# INLINE unfoldr #-}
unfoldr :: Monad m => Int -> Int -> Stream m Int
unfoldr streamLen n = Stream.unfoldr step n
    where
    step cnt =
        if cnt > n + streamLen
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE unfoldrD #-}
unfoldrD :: Monad m => Int -> Int -> StreamK m Int
unfoldrD streamLen n = Stream.toStreamK $ unfoldr streamLen n

{-# INLINE unfoldrMD #-}
unfoldrMD :: Monad m => Int -> Int -> StreamK m Int
unfoldrMD streamLen n = Stream.toStreamK (Stream.unfoldrM step n)
    where
    step cnt =
        if cnt > n + streamLen
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-
{-# INLINE unfoldrK #-}
unfoldrK :: Int -> Int -> StreamK m Int
unfoldrK streamLen n = StreamK.unfoldr step n
    where
    step cnt =
        if cnt > n + streamLen
        then Nothing
        else Just (cnt, cnt + 1)
-}

{-# INLINE unfoldrMK #-}
unfoldrMK :: Monad m => Int -> Int -> StreamK m Int
unfoldrMK streamLen n = StreamK.unfoldrMWith StreamK.consM step n
    where
    step cnt =
        if cnt > n + streamLen
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE repeat #-}
repeat :: Monad m => Int -> Int -> StreamK m Int
repeat streamLen = StreamK.take streamLen . Stream.toStreamK . Stream.repeat

{-# INLINE repeatM #-}
repeatM :: Monad m => Int -> Int -> StreamK m Int
repeatM streamLen = StreamK.take streamLen . Stream.toStreamK . Stream.repeatM . return

{-# INLINE replicate #-}
replicate :: Monad m => Int -> Int -> StreamK m Int
replicate x y = Stream.toStreamK $ Stream.replicate x y

{-# INLINE replicateM #-}
replicateM :: Monad m => Int -> Int -> StreamK m Int
replicateM streamLen = Stream.toStreamK . Stream.replicateM streamLen . return

{-# INLINE iterate #-}
iterate :: Monad m => Int -> Int -> StreamK m Int
iterate streamLen = StreamK.take streamLen . Stream.toStreamK . Stream.iterate (+1)

{-# INLINE iterateM #-}
iterateM :: Monad m => Int -> Int -> StreamK m Int
iterateM streamLen = StreamK.take streamLen . Stream.toStreamK . Stream.iterateM (return . (+1)) . return

{-# INLINE fromFoldable #-}
fromFoldable :: Int -> Int -> StreamK m Int
fromFoldable streamLen n = StreamK.fromFoldable [n..n+streamLen]

{-# INLINE fromFoldableM #-}
fromFoldableM :: Monad m => Int -> Int -> StreamK m Int
fromFoldableM streamLen n =
    Prelude.foldr (StreamK.consM . return) StreamK.nil [n .. n + streamLen]

{-
{-# INLINABLE concatMapFoldableWith #-}
concatMapFoldableWith :: Foldable f
    => (StreamK m b -> StreamK m b -> StreamK m b)
    -> (a -> StreamK m b)
    -> f a
    -> StreamK m b
concatMapFoldableWith f g = Prelude.foldr (f . g) StreamK.nil
-}

{-# INLINE concatMapFoldableSerial #-}
concatMapFoldableSerial :: Monad m => Int -> Int -> StreamK m Int
concatMapFoldableSerial streamLen n =
    Stream.toStreamK $ Stream.concatMap Stream.fromPure $ Stream.fromStreamK $ StreamK.fromList [n..n+streamLen]

{-# INLINE concatMapFoldableSerialM #-}
concatMapFoldableSerialM :: Monad m => Int -> Int -> StreamK m Int
concatMapFoldableSerialM streamLen n =
    -- concatMapFoldableWith StreamK.serial (StreamK.fromEffect . return) [n..n+streamLen]
    Stream.toStreamK $ Stream.concatMap (Stream.fromEffect . return) $ Stream.fromStreamK $ StreamK.fromList [n..n+streamLen]

{-# INLINE mfixUnfold #-}
mfixUnfold :: Int -> Int -> StreamK IO (Int, Int)
mfixUnfold count start = StreamK.mfix f
    where
    f action = StreamK.unCross $ do
        let incr n act = fmap ((+n) . snd)  $ unsafeInterleaveIO act
        x <- StreamK.mkCross (StreamK.fromStream $ Stream.fromListM [incr 1 action, incr 2 action])
        y <- StreamK.mkCross (unfoldrD count start)
        return (x, y)

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE drainD #-}
drainD :: Monad m => StreamK m a -> m ()
drainD = Stream.drain . Stream.fromStreamK

{-# INLINE drain #-}
drain :: Monad m => StreamK m a -> m ()
-- XXX Use "Stream" instead of "StreamK" when eliminating?
-- drain = drainD
drain = StreamK.drain

{-# INLINE mapM_ #-}
mapM_ :: Monad m => StreamK m a -> m ()
mapM_ s = Stream.mapM_ (\_ -> return ()) $ Stream.fromStreamK s

{-# INLINE uncons #-}
uncons :: Monad m => StreamK m Int -> m ()
uncons s = do
    r <- StreamK.uncons s
    case r of
        Nothing -> return ()
        Just (_, t) -> uncons t

-- Recursive call, does it work well?
{-# INLINE unconsD #-}
unconsD :: Monad m => StreamK m Int -> m ()
unconsD s = do
    r <- Stream.uncons $ Stream.fromStreamK s
    case r of
        Nothing -> return ()
        Just (_, t) -> uncons (Stream.toStreamK t)

{-
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
-}

{-# INLINE toList #-}
toList :: Monad m => StreamK m Int -> m [Int]
toList = Stream.fold Fold.toList . Stream.fromStreamK

{-# INLINE foldl' #-}
foldl' :: Monad m => StreamK m Int -> m Int
foldl' = Stream.fold (Fold.foldl' (+) 0) . Stream.fromStreamK

{-# INLINE last #-}
last :: Monad m => StreamK m Int -> m (Maybe Int)
last = Stream.fold Fold.latest . Stream.fromStreamK

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
scanl' n =
    composeN n (Stream.toStreamK . Stream.scan (Fold.foldl' (+) 0) . Stream.fromStreamK)

{-# INLINE map #-}
map :: Monad m => Int -> StreamK m Int -> m ()
map n = composeN n (Stream.toStreamK . Stream.map (+ 1) . Stream.fromStreamK)

{-
{-# INLINE fmapK #-}
fmapK :: Monad m => Int -> StreamK m Int -> m ()
fmapK n = composeN n $ P.fmap (+ 1)
-}

{-# INLINE mapM #-}
mapM :: Monad m => Int -> StreamK m Int -> m ()
mapM n = composeN n (Stream.toStreamK . Stream.mapM return . Stream.fromStreamK)

{-
{-# INLINE mapMSerial #-}
mapMSerial :: StreamK.MonadAsync m => Int -> StreamK m Int -> m ()
mapMSerial n = composeN n $ StreamK.mapMSerial return
-}

{-# INLINE filterEven #-}
filterEven :: Monad m => Int -> StreamK m Int -> m ()
filterEven n = composeN n (Stream.toStreamK . Stream.filter even . Stream.fromStreamK)

{-
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
-}

{-
{-# INLINE foldlS #-}
foldlS :: Monad m => Int -> StreamK m Int -> m ()
foldlS n = composeN n $ StreamK.foldlS (flip StreamK.cons) StreamK.nil
-}

{-# INLINE foldrS #-}
foldrS :: MonadIO m => Int -> StreamK m Int -> m ()
foldrS n =
    composeN n (StreamK.foldrS StreamK.cons StreamK.nil)

{-# INLINE foldrSMap #-}
foldrSMap :: MonadIO m => Int -> StreamK m Int -> m ()
foldrSMap n =
    composeN n
        (
          StreamK.foldrS (\x xs -> x + 1 `StreamK.cons` xs) StreamK.nil
        )

{-# INLINE foldrToStream #-}
foldrToStream :: Monad m => Stream m Int -> m (StreamK Identity Int)
foldrToStream = Stream.foldr StreamK.cons StreamK.nil

{-
{-# INLINE intersperse #-}
intersperse :: StreamK.MonadAsync m => Int -> Int -> StreamK m Int -> m ()
intersperse streamLen n = composeN n $ StreamK.intersperse streamLen
-}

-------------------------------------------------------------------------------
-- Traversable Instance
-------------------------------------------------------------------------------

{-# INLINE traversableTraverse #-}
traversableTraverse :: StreamK Identity Int -> IO (StreamK Identity Int)
traversableTraverse = traverse return

{- HLINT ignore "Use traverse" -}
{-# INLINE traversableSequenceA #-}
traversableSequenceA :: StreamK Identity Int -> IO (StreamK Identity Int)
traversableSequenceA = sequenceA . Prelude.fmap return

{-# INLINE traversableMapM #-}
traversableMapM :: StreamK Identity Int -> IO (StreamK Identity Int)
traversableMapM = P.mapM return

{- HLINT ignore "Use mapM" -}
{-# INLINE traversableSequence #-}
traversableSequence :: StreamK Identity Int -> IO (StreamK Identity Int)
traversableSequence = Prelude.sequence . Prelude.fmap return

{-# INLINE benchPureSinkIO #-}
benchPureSinkIO
    :: NFData b
    => Int -> String -> (StreamK Identity Int -> IO b) -> Benchmark
benchPureSinkIO value name f =
    bench name
        $ nfIO $ randomRIO (1, 1) >>= f . unfoldrD value

instance NFData a => NFData (StreamK Identity a) where
    {-# INLINE rnf #-}
    rnf xs =
        runIdentity
            $ Stream.fold (Fold.foldl' (\_ x -> rnf x) ()) (StreamK.toStream xs)

o_n_space_traversable :: Int -> Benchmark
o_n_space_traversable value =
    -- Buffering operations using heap proportional to number of elements.
    bgroup "traversable"
        -- Traversable instance
        [ benchPureSinkIO value "traverse" traversableTraverse
        , benchPureSinkIO value "sequenceA" traversableSequenceA
        , benchPureSinkIO value "mapM" traversableMapM
        , benchPureSinkIO value "sequence" traversableSequence
        ]

o_1_space_mapping :: Int -> Benchmark
o_1_space_mapping value =
    bgroup
        "mapping"
        [
        -- Right folds
          benchIOSink value "foldrS" (foldrS 1)
        , benchIOSink value "foldrSMap" (foldrSMap 1)
        ]

-------------------------------------------------------------------------------
-- Iteration of transformations
-------------------------------------------------------------------------------

{-# INLINE iterateSource #-}
iterateSource ::
       Monad m
    => (StreamK m Int -> StreamK m Int)
    -> Int
    -> Int
    -> Int
    -> StreamK m Int
iterateSource g count len n = f count (unfoldrMD len n)

    where

    f (0 :: Int) stream = stream
    f i stream = f (i - 1) (g stream)

-- this is quadratic
{-# INLINE iterateScan #-}
iterateScan :: Monad m => Int -> Int -> Int -> StreamK m Int
iterateScan = iterateSource (StreamK.scanl' (+) 0)

{-# INLINE iterateMapM #-}
iterateMapM :: Monad m => Int -> Int -> Int -> StreamK m Int
iterateMapM = iterateSource (StreamK.mapM return)

{-# INLINE iterateFilterEven #-}
iterateFilterEven :: Monad m => Int -> Int -> Int -> StreamK m Int
iterateFilterEven = iterateSource (StreamK.filter even)

{-# INLINE iterateTakeAll #-}
iterateTakeAll :: Monad m => Int -> Int -> Int -> Int -> StreamK m Int
iterateTakeAll value = iterateSource (StreamK.take (value + 1))

{-# INLINE iterateDropOne #-}
iterateDropOne :: Monad m => Int -> Int -> Int -> StreamK m Int
iterateDropOne = iterateSource (StreamK.drop 1)

{-# INLINE iterateDropWhileTrue #-}
iterateDropWhileTrue :: Monad m
    => Int -> Int -> Int -> Int -> StreamK m Int
iterateDropWhileTrue value = iterateSource (StreamK.dropWhile (<= (value + 1)))

{-# INLINE iterateDropWhileFalse #-}
iterateDropWhileFalse :: Monad m
    => Int -> Int -> Int -> Int -> StreamK m Int
iterateDropWhileFalse value = iterateSource (StreamK.dropWhile (> (value + 1)))

-- Head recursive operations.
o_n_stack_iterated :: Int -> Benchmark
o_n_stack_iterated value = by10 `seq` by100 `seq`
    bgroup "iterated"
        [
          benchIOSrc "mapM (n/10 x 10)" $ iterateMapM by10 10
        , benchIOSrc "scanl' (quadratic) (n/100 x 100)" $
            iterateScan by100 100
        , benchIOSrc "filterEven (n/10 x 10)" $
            iterateFilterEven by10 10
        , benchIOSrc "takeAll (n/10 x 10)" $
            iterateTakeAll value by10 10
        , benchIOSrc "dropOne (n/10 x 10)" $ iterateDropOne by10 10
        , benchIOSrc "dropWhileTrue (n/10 x 10)" $
            iterateDropWhileTrue value by10 10
        , benchIOSrc "dropWhileFalse (n/10 x 10)" $
            iterateDropWhileFalse value by10 10
        {-
        , benchFold "tail"   tail     (unfoldrM streamLen)
        , benchFold "nullTail" nullTail (unfoldrM streamLen)
        , benchFold "headTail" headTail (unfoldrM streamLen)
        -}
        ]

    where

    by10 = value `div` 10
    by100 = value `div` 100

-------------------------------------------------------------------------------
-- Iteration Applicative
-------------------------------------------------------------------------------

{-# INLINE iterateN #-}
iterateN :: (Int -> a -> a) -> a -> Int -> a
iterateN g initial count = f count initial

    where

    f (0 :: Int) x = x
    f i x = f (i - 1) (g i x)

-- Iterate a transformation over a singleton stream
{-# INLINE iterateSingleton #-}
iterateSingleton ::
       (Int -> CrossStreamK m Int -> CrossStreamK m Int)
    -> Int
    -> Int
    -> StreamK m Int
iterateSingleton g count n =
      StreamK.unCross
    $ iterateN g (StreamK.mkCross (StreamK.fromPure n)) count

{-
-- XXX need to check why this is slower than the explicit recursion above, even
-- if the above code is written in a foldr like head recursive way. We also
-- need to try this with foldlM' once #150 is fixed.
-- However, it is perhaps best to keep the iteration benchmarks independent of
-- foldrM and any related fusion issues.
{-# INLINE _iterateSingleton #-}
_iterateSingleton ::
       Monad m
    => (Int -> Stream m Int -> Stream m Int)
    -> Int
    -> Int
    -> Stream m Int
_iterateSingleton g value n = S.foldrM g (return n) $ sourceIntFromTo value n
-}

{- HLINT ignore "Redundant <*" -}
o_n_space_applicative :: Int -> Benchmark
o_n_space_applicative value =
    bgroup "iterated"
        [ benchIOSrc "(*>) (n times)" $
            iterateSingleton ((*>) . pure) value
        , benchIOSrc "(<*) (n times)" $
            iterateSingleton (\x xs -> xs <* pure x) value
        , benchIOSrc "(<*>) (n times)" $
            iterateSingleton (\x xs -> pure (+ x) <*> xs) value
        , benchIOSrc "liftA2 (n times)" $
            iterateSingleton (liftA2 (+) . pure) value
        ]

-------------------------------------------------------------------------------
-- Iteration Monadic
-------------------------------------------------------------------------------

-- This is a good benchmark but inefficient way to compute primes. As we see a
-- new prime we keep appending a division filter for all the future numbers.
{-# INLINE sieve #-}
sieve :: Monad m => StreamK m Int -> StreamK m Int
sieve s = StreamK.concatEffect $ do
    r <- StreamK.uncons s
    case r of
        Just (prime, rest) ->
            -- XXX Use K.filter or rewrite to K.filter
            let f = Stream.filter (\n -> n `mod` prime /= 0)
             in pure $ prime `StreamK.cons` sieve (StreamK.fromStream $ f $ StreamK.toStream rest)
        Nothing -> pure StreamK.nil

o_n_space_iterated :: Int -> Benchmark
o_n_space_iterated value =
    bgroup "iterated"
        [
          benchIO "concatEffect prime sieve (n/4)"
            (\n ->
                  Stream.fold Fold.sum
                $ StreamK.toStream
                $ sieve
                $ StreamK.fromStream
                $ Stream.enumerateFromTo 2 (value `div` 4 + n))
        , benchIOSrc "(>>) (n times)" $
            iterateSingleton ((>>) . pure) value
        , benchIOSrc "(>>=) (n times)" $
            iterateSingleton (\x xs -> xs >>= \y -> return (x + y)) value
        ]

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

{-
{-# INLINE zipWith #-}
zipWith :: Monad m => StreamK m Int -> m ()
zipWith src = drain $ StreamK.zipWith (,) src src

{-# INLINE zipWithM #-}
zipWithM :: Monad m => StreamK m Int -> m ()
zipWithM src = drain $ StreamK.zipWithM (curry return) src src

{-# INLINE sortByK #-}
sortByK :: (a -> a -> Ordering) -> StreamK m a -> StreamK m a
sortByK f = StreamK.concatPairsWith (StreamK.mergeBy f) StreamK.fromPure

{-# INLINE sortBy #-}
sortBy :: Monad m => StreamK m Int -> m ()
sortBy = drain . sortByK compare

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
-}

-------------------------------------------------------------------------------
-- Applicative
-------------------------------------------------------------------------------

{-# INLINE apDiscardFst #-}
apDiscardFst :: Monad m =>
    Int -> Int -> m ()
apDiscardFst linearCount start = drain $ StreamK.unCross $
    StreamK.mkCross (unfoldrMD nestedCount2 start)
        *> StreamK.mkCross (unfoldrMD nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE apDiscardSnd #-}
apDiscardSnd :: Monad m => Int -> Int -> m ()
apDiscardSnd linearCount start = drain $ StreamK.unCross $
    StreamK.mkCross (unfoldrMD nestedCount2 start)
        <* StreamK.mkCross (unfoldrMD nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE apLiftA2 #-}
apLiftA2 :: Monad m => Int -> Int -> m ()
apLiftA2 linearCount start = drain $ StreamK.unCross $
    liftA2 (+) (StreamK.mkCross (unfoldrMD nestedCount2 start))
        (StreamK.mkCross (unfoldrMD nestedCount2 start))

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullAp #-}
toNullAp :: Monad m => Int -> Int -> m ()
toNullAp linearCount start = drain $ StreamK.unCross $
    (+) <$> StreamK.mkCross (unfoldrMD nestedCount2 start)
        <*> StreamK.mkCross (unfoldrMD nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

o_1_space_applicative :: Int -> Benchmark
o_1_space_applicative value =
    bgroup "Applicative"
        [ benchIO "(*>) (sqrt n x sqrt n)" $ apDiscardFst value
        , benchIO "(<*) (sqrt n x sqrt n)" $ apDiscardSnd value
        , benchIO "(<*>) (sqrt n x sqrt n)" $ toNullAp value
        , benchIO "liftA2 (sqrt n x sqrt n)" $ apLiftA2 value
        ]

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

{-# INLINE monadThen #-}
monadThen :: Monad m => Int -> Int -> m ()
monadThen linearCount start = drain $ StreamK.unCross $ do
    StreamK.mkCross (unfoldrMD nestedCount2 start) >>
        StreamK.mkCross (unfoldrMD nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullM #-}
toNullM :: Monad m => Int -> Int -> m ()
toNullM linearCount start = drain $ StreamK.unCross $ do
    x <- StreamK.mkCross (unfoldrMD nestedCount2 start)
    y <- StreamK.mkCross (unfoldrMD nestedCount2 start)
    return $ x + y

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullM3 #-}
toNullM3 :: Monad m => Int -> Int -> m ()
toNullM3 linearCount start = drain $ StreamK.unCross $ do
    x <- StreamK.mkCross (unfoldrMD nestedCount3 start)
    y <- StreamK.mkCross (unfoldrMD nestedCount3 start)
    z <- StreamK.mkCross (unfoldrMD nestedCount3 start)
    return $ x + y + z
  where
    nestedCount3 = round (fromIntegral linearCount**(1/3::Double))

{-# INLINE filterAllOutM #-}
filterAllOutM :: Monad m => Int -> Int -> m ()
filterAllOutM linearCount start = drain $ StreamK.unCross $ do
    x <- StreamK.mkCross (unfoldrMD nestedCount2 start)
    y <- StreamK.mkCross (unfoldrMD nestedCount2 start)
    let s = x + y
    if s < 0
    then return s
    else StreamK.mkCross StreamK.nil
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE filterAllInM #-}
filterAllInM :: Monad m => Int -> Int -> m ()
filterAllInM linearCount start = drain $ StreamK.unCross $ do
    x <- StreamK.mkCross (unfoldrMD nestedCount2 start)
    y <- StreamK.mkCross (unfoldrMD nestedCount2 start)
    let s = x + y
    if s > 0
    then return s
    else StreamK.mkCross StreamK.nil
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE filterSome #-}
filterSome :: Monad m => Int -> Int -> m ()
filterSome linearCount start = drain $ StreamK.unCross $ do
    x <- StreamK.mkCross (unfoldrMD nestedCount2 start)
    y <- StreamK.mkCross (unfoldrMD nestedCount2 start)
    let s = x + y
    if s > 1100000
    then return s
    else StreamK.mkCross StreamK.nil
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE breakAfterSome #-}
breakAfterSome :: Int -> Int -> IO ()
breakAfterSome linearCount start = do
    (_ :: Either ErrorCall ()) <- try $ drain $ StreamK.unCross $ do
        x <- StreamK.mkCross (unfoldrMD nestedCount2 start)
        y <- StreamK.mkCross (unfoldrMD nestedCount2 start)
        let s = x + y
        if s > 1100000
        then error "break"
        else return s
    return ()
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toListM #-}
toListM :: Monad m => Int -> Int -> m [Int]
toListM linearCount start = toList $ StreamK.unCross $ do
    x <- StreamK.mkCross (unfoldrMD nestedCount2 start)
    y <- StreamK.mkCross (unfoldrMD nestedCount2 start)
    return $ x + y
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

-- Taking a specified number of elements is very expensive in logict so we have
-- a test to measure the same.
{-# INLINE toListSome #-}
toListSome :: Monad m => Int -> Int -> m [Int]
toListSome linearCount start =
    toList $ StreamK.take 10000 $ StreamK.unCross $ do
        x <- StreamK.mkCross (unfoldrMD nestedCount2 start)
        y <- StreamK.mkCross (unfoldrMD nestedCount2 start)
        return $ x + y
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

o_1_space_monad :: Int -> Benchmark
o_1_space_monad value =
    bgroup "Monad"
        [ benchIO "(>>) (sqrt n x sqrt n)" $ monadThen value
        , benchIO "(>>=) (sqrt n x sqrt n)" $ toNullM value
        , benchIO "(>>=) (sqrt n x sqrt n) (filterAllOut)" $
            filterAllOutM value
        , benchIO "(>>=) (sqrt n x sqrt n) (filterAllIn)" $
            filterAllInM value
        , benchIO "(>>=) (sqrt n x sqrt n) (filterSome)" $
            filterSome value
        , benchIO "(>>=) (sqrt n x sqrt n) (breakAfterSome)" $
            breakAfterSome value
        , benchIO "(>>=) (cubert n x cubert n x cubert n)" $
            toNullM3 value
        ]

o_n_space_monad :: Int -> Benchmark
o_n_space_monad value =
    bgroup "Monad"
        [ benchIO "(>>=) (sqrt n x sqrt n) (toList)" $
            toListM value
        , benchIO "(>>=) (sqrt n x sqrt n) (toListSome)" $
            toListSome value
        ]

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

{-# INLINE serial2 #-}
serial2 :: Int -> Int -> IO ()
serial2 count n =
    drain $
        StreamK.append
            (unfoldrMD count n)
            (unfoldrMD count (n + 1))

{-# INLINE serial4 #-}
serial4 :: Int -> Int -> IO ()
serial4 count n =
    drain $
    StreamK.append
        (StreamK.append
            (unfoldrMD count n)
            (unfoldrMD count (n + 1)))
        (StreamK.append
              (unfoldrMD count (n + 2))
              (unfoldrMD count (n + 3)))

o_1_space_joining :: Int -> Benchmark
o_1_space_joining value =
    bgroup "joining"
        [ benchIOSrc1 "serial (2,x/2)" (serial2 (value `div` 2))
        , benchIOSrc1 "serial (2,2,x/4)" (serial4 (value `div` 4))
        ]

-------------------------------------------------------------------------------
-- ConcatMap
-------------------------------------------------------------------------------

-- concatMap unfoldrM/unfoldrM

{-# INLINE sourceConcatMapId #-}
sourceConcatMapId :: Monad m
    => Int -> Int -> StreamK m (StreamK m Int)
sourceConcatMapId val n =
    StreamK.fromFoldable $ fmap (StreamK.fromEffect . return) [n..n+val]

{-# INLINE concatMap #-}
concatMap :: Int -> Int -> Int -> IO ()
concatMap outer inner n =
    drain $ StreamK.concatMap
        (\_ -> unfoldrMD inner n)
        (unfoldrMD outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMap
#endif

{-# INLINE concatMapPure #-}
concatMapPure :: Int -> Int -> Int -> IO ()
concatMapPure outer inner n =
    drain $ StreamK.concatMap
        (\_ -> unfoldrMD inner n)
        (unfoldrMD outer n)

#ifdef INSPECTION
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'concatMapPure [''Applicative]
#else
inspect $ hasNoTypeClasses 'concatMapPure
#endif
inspect $ 'concatMapPure `hasNoType` ''SPEC
#endif

{-# INLINE concatMapRepl #-}
concatMapRepl :: Int -> Int -> Int -> IO ()
concatMapRepl outer inner n =
    drain $ StreamK.concatMap
        (StreamK.replicate inner) (unfoldrMD outer n)

#ifdef INSPECTION
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'concatMapRepl [''Applicative]
#else
inspect $ hasNoTypeClasses 'concatMapRepl
#endif
inspect $ 'concatMapRepl `hasNoType` ''SPEC
#endif

-- concatMapWith

{-# INLINE concatStreamsWith #-}
concatStreamsWith
    :: (StreamK IO Int -> StreamK IO Int -> StreamK IO Int)
    -> Int
    -> Int
    -> Int
    -> IO ()
concatStreamsWith op outer inner n =
    drain $ StreamK.concatMapWith op
        (unfoldrMD inner)
        (unfoldrMD outer n)

{-# INLINE mergeMapWith #-}
mergeMapWith
    :: (StreamK IO Int -> StreamK IO Int -> StreamK IO Int)
    -> Int
    -> Int
    -> Int
    -> IO ()
mergeMapWith op outer inner n =
    drain $ StreamK.mergeMapWith op
        (unfoldrMD inner)
        (unfoldrMD outer n)

{-# INLINE concatMapWithSerial #-}
concatMapWithSerial :: Int -> Int -> Int -> IO ()
concatMapWithSerial = concatStreamsWith StreamK.append

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithSerial
inspect $ 'concatMapWithSerial `hasNoType` ''SPEC
#endif

{-
{-# INLINE concatMapWithAppend #-}
concatMapWithAppend :: Int -> Int -> Int -> IO ()
concatMapWithAppend = concatStreamsWith Stream.append

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithAppend
inspect $ 'concatMapWithAppend `hasNoType` ''SPEC
#endif
-}

-- mergeMapWith

{-# INLINE mergeMapWithSerial #-}
mergeMapWithSerial :: Int -> Int -> Int -> IO ()
mergeMapWithSerial = mergeMapWith StreamK.append

{-
{-# INLINE mergeMapWithAppend #-}
mergeMapWithAppend :: Int -> Int -> Int -> IO ()
mergeMapWithAppend = mergeMapWith Stream.append
-}

{-
-------------------------------------------------------------------------------
-- Nested Composition
-------------------------------------------------------------------------------

-- XXX Use CrossStreamK instead
instance Monad m => Applicative (StreamK.Stream m) where
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
instance Monad m => Monad (StreamK.Stream m) where
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

{-# INLINE drainMonad3 #-}
drainMonad3 :: Monad m => StreamK m Int -> m ()
drainMonad3 s = drain $ do
    x <- s
    y <- s
    z <- s
    return $ x + y + z

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
-}

-------------------------------------------------------------------------------
-- Buffering
-------------------------------------------------------------------------------

{-# INLINE reverse #-}
reverse :: MonadIO m => Int -> StreamK m Int -> m ()
reverse n = composeN n StreamK.reverse

-------------------------------------------------------------------------------
-- Reduce
-------------------------------------------------------------------------------

{-# INLINE parseBreak #-}
parseBreak :: Monad m => StreamK m Int -> m ()
parseBreak s = do
    r <- StreamK.parseDBreak Parser.one s
    case r of
         (Left _, _) -> return ()
         (Right _, s1) -> parseBreak s1

-------------------------------------------------------------------------------
-- Lift
-------------------------------------------------------------------------------

{-# INLINE iterateStateIO #-}
iterateStateIO ::
       Monad m
    => Int
    -> StateT Int m Int
iterateStateIO n = do
    x <- get
    if x > n
    then do
        put (x - 1)
        iterateStateIO n
    else return x

-- XXX This is basically testing the perf of concatEffect, change it to just
-- use concatEffect and move it along with other concatMap benchmarks.
{-# INLINE iterateStateT #-}
iterateStateT :: Int -> StreamK (StateT Int IO) Int
iterateStateT n = StreamK.concatEffect $ do
    x <- get
    if x > n
    then do
        put (x - 1)
        return $ iterateStateT n
    else return $ StreamK.fromPure x

o_n_heap_transformer :: Int -> Benchmark
o_n_heap_transformer value =
    bgroup "transformer"
        [ benchIO "StateT Int IO (n times) (baseline)" $ \n ->
            State.evalStateT (iterateStateIO n) value
        , benchIO "Stream (StateT Int IO) (n times)" $ \n ->
            State.evalStateT (drain (iterateStateT n)) value
        ]

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.StreamK.FromStream"

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

        , benchIOSrc "mfix_10" (mfixUnfold 10)
        , benchIOSrc "mfix_100" (mfixUnfold 100)
        , benchIOSrc "mfix_1000" (mfixUnfold 1000)

        -- appends
        , benchFold "concatMapFoldableWith"  drain (concatMapFoldableSerial streamLen)
        , benchFold "concatMapFoldableWithM" drain (concatMapFoldableSerialM streamLen)
        ]

-- Generating using StreamK and eliminating using StreamD folds.
o_1_space_elimination :: Int -> Benchmark
o_1_space_elimination streamLen =
    bgroup "elimination"
        [ benchFold "toNull" drainD (unfoldrMK streamLen)
        , benchFold "mapM_" mapM_ (unfoldrMK streamLen)
        , benchIOSink streamLen "uncons" uncons
        , benchFold "unconsD" unconsD (unfoldrMK streamLen)
        -- , benchFold "init"   init     (unfoldrM streamLen)
        , benchFold "foldl'" foldl'    (unfoldrMK streamLen)
        , benchFold "last"   last     (unfoldrMK streamLen)
        , bgroup "build"
            [
              bgroup "Identity"
                  [
                    benchIdentitySink streamLen "foldrToStreamLength"
                        (Stream.fold Fold.length . StreamK.toStream . runIdentity . foldrToStream)
                  {-
                  , benchIdentitySink 16 "foldrToStreamLength (16)"
                        (S.fold Fold.length . toStream . runIdentity . foldrToStream)
                  -}
                  ]
            ]
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

o_1_space_concat :: Int -> Benchmark
o_1_space_concat streamLen =
    bgroup "concat"
        [ benchIOSrc1 "concatMapPure (n of 1)"
            (concatMapPure streamLen 1)
        , benchIOSrc1 "concatMapPure (sqrt n of sqrt n)"
            (concatMapPure streamLen2 streamLen2)
        , benchIOSrc1 "concatMapPure (1 of n)"
            (concatMapPure 1 streamLen)

        , benchIOSrc1 "concatMap (n of 1)"
            (concatMap streamLen 1)
        , benchIOSrc1 "concatMap (sqrt n of sqrt n)"
            (concatMap streamLen2 streamLen2)
        , benchIOSrc1 "concatMap (1 of n)"
            (concatMap 1 streamLen)

        -- This is for comparison with foldMapWith
        , benchIOSrc "concatMapWithId (n of 1) (fromFoldable)"
            (StreamK.concatMapWith StreamK.append id . sourceConcatMapId streamLen)

        , benchIOSrc1 "concatMapWith (n of 1)"
            (concatMapWithSerial streamLen 1)
        , benchIOSrc1 "concatMapWith (sqrt n of sqrt n)"
            (concatMapWithSerial streamLen2 streamLen2)
        , benchIOSrc1 "concatMapWith (1 of n)"
            (concatMapWithSerial 1 streamLen)

        {-
        -- quadratic with number of outer streams
        , benchIOSrc1 "concatMapWithAppend (2 of n/2)"
            (concatMapWithAppend 2 (value `div` 2))
        -}

        , benchIOSrc1 "concatMapRepl (sqrt n of sqrt n)"
            (concatMapRepl streamLen2 streamLen2)

        {-
        , benchFold "sortBy" sortBy (unfoldrM streamLen)
        -}
        ]
    where
    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop

o_n_space_merge :: Int -> Benchmark
o_n_space_merge value = sqrtVal `seq`
    bgroup "concat"
        [
        -------------------mergeMapWith-----------------

        -- Use large number of streams to check scalability

          benchIOSrc1 "mergeMapWithSerial (n of 1)"
            (mergeMapWithSerial value 1)
        , benchIOSrc1 "mergeMapWithSerial (sqrtVal of sqrtVal)"
            (mergeMapWithSerial sqrtVal sqrtVal)
        , benchIOSrc1 "mergeMapWithSerial (2 of n/2)"
            (mergeMapWithSerial 2 (value `div` 2))

        {-
        , benchIOSrc1 "mergeMapWithAppend (n of 1)"
            (mergeMapWithAppend value 1)
        , benchIOSrc1 "mergeMapWithAppend (sqrtVal of sqrtVal)"
            (mergeMapWithAppend sqrtVal sqrtVal)
        -}
        ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)

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

o_1_space_grouping :: Int -> Benchmark
o_1_space_grouping value =
    bgroup "grouping"
        [
          benchIOSink value "parseBreak (recursive)" parseBreak
        ]

o_1_space :: Int -> Benchmark
o_1_space streamLen =
    bgroup (o_1_space_prefix moduleName)
      [ o_1_space_generation streamLen
      , o_1_space_elimination streamLen
      -- , o_1_space_nested streamLen
      , o_1_space_joining streamLen
      , o_1_space_mapping streamLen
      , o_1_space_transformation streamLen
      , o_1_space_transformationX4 streamLen
      , o_1_space_concat streamLen
      , o_1_space_applicative streamLen
      , o_1_space_monad streamLen
      , o_1_space_filtering streamLen
      , o_1_space_filteringX4 streamLen
      , o_1_space_grouping streamLen
      -- , o_1_space_zipping streamLen
      -- , o_1_space_mixed streamLen
      -- , o_1_space_mixedX2 streamLen
      -- , o_1_space_mixedX4 streamLen
      -- , o_1_space_list streamLen
      ]

o_n_heap :: Int -> Benchmark
o_n_heap streamLen =
    bgroup (o_n_heap_prefix moduleName)
      [ {- bgroup "transformation"
        [ benchFold "foldlS" (foldlS 1) (unfoldrM streamLen)
        ] -}
        bgroup "buffered" [benchIOSink streamLen "reverse" (reverse 1)]
      , o_n_heap_transformer streamLen
      ]

o_n_stack :: Int -> Benchmark
o_n_stack streamLen =
    bgroup (o_n_stack_prefix moduleName)
      [
      {-
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
        -}
          o_n_stack_iterated streamLen
        ]
      {-
    where
    streamLen2 = round (P.fromIntegral streamLen**(1/2::P.Double)) -- double nested loop
    streamLen16 = round (P.fromIntegral streamLen**(1/16::P.Double)) -- triple nested loop
    -}

o_n_space :: Int -> Benchmark
o_n_space streamLen =
    bgroup (o_n_space_prefix moduleName)
      [ bgroup "elimination"
        [ benchFold "toList" toList (unfoldrMK streamLen)
        ]
      , o_n_space_merge streamLen
      , o_n_space_monad streamLen
      , o_n_space_iterated streamLen
      , o_n_space_applicative streamLen
      , o_n_space_traversable streamLen
      ]

main :: IO ()
main =
    defaultMain
        [ o_1_space streamLen
        , o_n_stack streamLen
        , o_n_heap streamLen
        , o_n_space streamLen
        ]

    where

    streamLen = 100000
