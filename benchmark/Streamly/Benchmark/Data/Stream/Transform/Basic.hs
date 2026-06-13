-- |
-- Module      : Stream.Transform
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-orphans #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Stream.Transform.Basic (benchmarks) where

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection
import qualified Streamly.Internal.Data.Producer as Producer
#endif

import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Internal.Data.Stream (Stream)

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold

import Test.Tasty.Bench
import Stream.Common hiding (scanl', benchIO)
import Stream.Type (benchIO, withRandomIntIO, withStream)
import Streamly.Benchmark.Common
import Prelude hiding (sequence, mapM, reverse)

-------------------------------------------------------------------------------
-- Pipelines (stream-to-stream transformations)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- one-to-one transformations
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- maps and scans
-------------------------------------------------------------------------------

{-# INLINE scanl' #-}
scanl' :: MonadIO m => Int -> Stream m Int -> m ()
scanl' n = composeN n $ Stream.scanl' (+) 0

scanl'1 :: Int -> IO ()
scanl'1 value = withStream value (scanl' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl'1
inspect $ 'scanl'1 `hasNoType` ''Stream.Step
#endif

scanl'4 :: Int -> IO ()
scanl'4 value = withStream value (scanl' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl'4
inspect $ 'scanl'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE scanlM' #-}
scanlM' :: MonadIO m => Int -> Stream m Int -> m ()
scanlM' n = composeN n $ Stream.scanlM' (\b a -> return $ b + a) (return 0)

scanlM'1 :: Int -> IO ()
scanlM'1 value = withStream value (scanlM' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanlM'1
inspect $ 'scanlM'1 `hasNoType` ''Stream.Step
#endif

scanlM'4 :: Int -> IO ()
scanlM'4 value = withStream value (scanlM' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanlM'4
inspect $ 'scanlM'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE scanl1' #-}
scanl1' :: MonadIO m => Int -> Stream m Int -> m ()
scanl1' n = composeN n $ Stream.scanl1' (+)

scanl1'1 :: Int -> IO ()
scanl1'1 value = withStream value (scanl1' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl1'1
inspect $ 'scanl1'1 `hasNoType` ''Stream.Step
#endif

scanl1'4 :: Int -> IO ()
scanl1'4 value = withStream value (scanl1' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl1'4
inspect $ 'scanl1'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE scanl1M' #-}
scanl1M' :: MonadIO m => Int -> Stream m Int -> m ()
scanl1M' n = composeN n $ Stream.scanl1M' (\b a -> return $ b + a)

scanl1M'1 :: Int -> IO ()
scanl1M'1 value = withStream value (scanl1M' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl1M'1
inspect $ 'scanl1M'1 `hasNoType` ''Stream.Step
#endif

scanl1M'4 :: Int -> IO ()
scanl1M'4 value = withStream value (scanl1M' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl1M'4
inspect $ 'scanl1M'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE scan #-}
scan :: MonadIO m => Int -> Stream m Int -> m ()
scan n = composeN n $ Stream.scanl Scanl.sum

scan1 :: Int -> IO ()
scan1 value = withStream value (scan 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scan1
inspect $ 'scan1 `hasNoType` ''Stream.Step
inspect $ 'scan1 `hasNoType` ''Stream.ScanState
inspect $ 'scan1 `hasNoType` ''FL.Step
inspect $ 'scan1 `hasNoType` ''SPEC
#endif

scan4 :: Int -> IO ()
scan4 value = withStream value (scan 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scan4
inspect $ 'scan4 `hasNoType` ''Stream.Step
inspect $ 'scan4 `hasNoType` ''Stream.ScanState
inspect $ 'scan4 `hasNoType` ''FL.Step
inspect $ 'scan4 `hasNoType` ''SPEC
#endif

{-# INLINE postscan #-}
postscan :: MonadIO m => Int -> Stream m Int -> m ()
postscan n = composeN n $ Stream.postscanl Scanl.sum

postscan1 :: Int -> IO ()
postscan1 value = withStream value (postscan 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscan1
inspect $ 'postscan1 `hasNoType` ''Stream.Step
inspect $ 'postscan1 `hasNoType` ''Stream.ScanState
inspect $ 'postscan1 `hasNoType` ''FL.Step
inspect $ 'postscan1 `hasNoType` ''SPEC
#endif

postscan4 :: Int -> IO ()
postscan4 value = withStream value (postscan 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscan4
inspect $ 'postscan4 `hasNoType` ''Stream.Step
inspect $ 'postscan4 `hasNoType` ''Stream.ScanState
inspect $ 'postscan4 `hasNoType` ''FL.Step
inspect $ 'postscan4 `hasNoType` ''SPEC
#endif

{-# INLINE postscanl' #-}
postscanl' :: MonadIO m => Int -> Stream m Int -> m ()
postscanl' n = composeN n $ Stream.postscanl' (+) 0

postscanl'1 :: Int -> IO ()
postscanl'1 value = withStream value (postscanl' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanl'1
inspect $ 'postscanl'1 `hasNoType` ''Stream.Step
#endif

postscanl'4 :: Int -> IO ()
postscanl'4 value = withStream value (postscanl' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanl'4
-- inspect $ 'postscanl'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE postscanlM' #-}
postscanlM' :: MonadIO m => Int -> Stream m Int -> m ()
postscanlM' n = composeN n $ Stream.postscanlM' (\b a -> return $ b + a) (return 0)

postscanlM'1 :: Int -> IO ()
postscanlM'1 value = withStream value (postscanlM' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanlM'1
inspect $ 'postscanlM'1 `hasNoType` ''Stream.Step
#endif

postscanlM'4 :: Int -> IO ()
postscanlM'4 value = withStream value (postscanlM' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanlM'4
inspect $ 'postscanlM'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE sequence #-}
sequence :: MonadAsync m => Stream m (m Int) -> m ()
sequence = Common.drain . Stream.sequence

sequence1 :: Int -> IO ()
sequence1 value = withRandomIntIO $ sequence . sourceUnfoldrAction value

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sequence1
inspect $ 'sequence1 `hasNoType` ''Stream.Step
inspect $ 'sequence1 `hasNoType` ''FL.Step
inspect $ 'sequence1 `hasNoType` ''SPEC
#endif

{-# INLINE tap #-}
tap :: MonadIO m => Int -> Stream m Int -> m ()
tap n = composeN n $ Stream.tap FL.sum

tap1 :: Int -> IO ()
tap1 value = withStream value (tap 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'tap1
inspect $ 'tap1 `hasNoType` ''Stream.Step
inspect $ 'tap1 `hasNoType` ''Stream.TapState
inspect $ 'tap1 `hasNoType` ''FL.Step
inspect $ 'tap1 `hasNoType` ''SPEC
#endif

{-# INLINE _timestamped #-}
_timestamped :: MonadIO m => Stream m Int -> m ()
_timestamped = Stream.drain . Stream.timestamped
{-
{-# INLINE foldrT #-}
foldrT :: MonadIO m => Int -> Stream m Int -> m ()
foldrT n = composeN n (unCrossStream . Stream.foldrT cns (CrossStream Stream.nil))

    where cns x (CrossStream xs) = CrossStream (Stream.cons x xs)

{-# INLINE foldrTMap #-}
foldrTMap :: MonadIO m => Int -> Stream m Int -> m ()
foldrTMap n = composeN n $ Stream.foldrT (\x xs -> x + 1 `Stream.cons` xs) Stream.nil
-}

{-# INLINE trace #-}
trace :: MonadAsync m => Int -> Stream m Int -> m ()
trace n = composeN n $ Stream.trace return

trace4 :: Int -> IO ()
trace4 value = withStream value (trace 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'trace4
inspect $ 'trace4 `hasNoType` ''Stream.Step
inspect $ 'trace4 `hasNoType` ''FL.Step
inspect $ 'trace4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Iteration/looping utilities
-------------------------------------------------------------------------------

{-# INLINE iterateN #-}
iterateN :: (Int -> a -> a) -> a -> Int -> a
iterateN g initial count = f count initial

    where

    f (0 :: Int) x = x
    f i x = f (i - 1) (g i x)

-- Iterate a transformation over a singleton stream
{-# INLINE iterateSingleton #-}
iterateSingleton :: Applicative m =>
       (Int -> Stream m Int -> Stream m Int)
    -> Int
    -> Int
    -> Stream m Int
iterateSingleton g count n = iterateN g (Stream.fromPure n) count

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

iteratePlusBaseline :: Int -> IO Int
iteratePlusBaseline value =
    withRandomIntIO $ \i0 ->
        iterateN (\i acc -> acc >>= \n -> return $ i + n) (return i0) value

iterateSubMap :: Int -> IO ()
iterateSubMap value = withRandomIntIO $ drain . iterateSingleton (<$) value

iterateFmap :: Int -> IO ()
iterateFmap value = withRandomIntIO $ drain . iterateSingleton (fmap . (+)) value

-------------------------------------------------------------------------------
-- Size reducing transformations (filtering)
-------------------------------------------------------------------------------

{-# INLINE filterEven #-}
filterEven :: MonadIO m => Int -> Stream m Int -> m ()
filterEven n = composeN n $ Stream.filter even

filterEven1 :: Int -> IO ()
filterEven1 value = withStream value (filterEven 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterEven1
inspect $ 'filterEven1 `hasNoType` ''Stream.Step
inspect $ 'filterEven1 `hasNoType` ''FL.Step
inspect $ 'filterEven1 `hasNoType` ''SPEC
#endif

filterEven4 :: Int -> IO ()
filterEven4 value = withStream value (filterEven 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterEven4
inspect $ 'filterEven4 `hasNoType` ''Stream.Step
inspect $ 'filterEven4 `hasNoType` ''FL.Step
inspect $ 'filterEven4 `hasNoType` ''SPEC
#endif

{-# INLINE filterAllOut #-}
filterAllOut :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterAllOut value n = composeN n $ Stream.filter (> (value + 1))

filterAllOut1 :: Int -> IO ()
filterAllOut1 value = withStream value (filterAllOut value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterAllOut1
inspect $ 'filterAllOut1 `hasNoType` ''Stream.Step
inspect $ 'filterAllOut1 `hasNoType` ''FL.Step
inspect $ 'filterAllOut1 `hasNoType` ''SPEC
#endif

filterAllOut4 :: Int -> IO ()
filterAllOut4 value = withStream value (filterAllOut value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterAllOut4
inspect $ 'filterAllOut4 `hasNoType` ''Stream.Step
inspect $ 'filterAllOut4 `hasNoType` ''FL.Step
inspect $ 'filterAllOut4 `hasNoType` ''SPEC
#endif

{-# INLINE filterAllIn #-}
filterAllIn :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterAllIn value n = composeN n $ Stream.filter (<= (value + 1))

filterAllIn1 :: Int -> IO ()
filterAllIn1 value = withStream value (filterAllIn value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterAllIn1
inspect $ 'filterAllIn1 `hasNoType` ''Stream.Step
inspect $ 'filterAllIn1 `hasNoType` ''FL.Step
inspect $ 'filterAllIn1 `hasNoType` ''SPEC
#endif

filterAllIn4 :: Int -> IO ()
filterAllIn4 value = withStream value (filterAllIn value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterAllIn4
inspect $ 'filterAllIn4 `hasNoType` ''Stream.Step
inspect $ 'filterAllIn4 `hasNoType` ''FL.Step
inspect $ 'filterAllIn4 `hasNoType` ''SPEC
#endif

{-# INLINE filterMEven #-}
filterMEven :: MonadIO m => Int -> Stream m Int -> m ()
filterMEven n = composeN n $ Stream.filterM (return . even)

filterMEven1 :: Int -> IO ()
filterMEven1 value = withStream value (filterMEven 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMEven1
inspect $ 'filterMEven1 `hasNoType` ''Stream.Step
inspect $ 'filterMEven1 `hasNoType` ''FL.Step
inspect $ 'filterMEven1 `hasNoType` ''SPEC
#endif

filterMEven4 :: Int -> IO ()
filterMEven4 value = withStream value (filterMEven 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMEven4
inspect $ 'filterMEven4 `hasNoType` ''Stream.Step
inspect $ 'filterMEven4 `hasNoType` ''FL.Step
inspect $ 'filterMEven4 `hasNoType` ''SPEC
#endif

{-# INLINE filterMAllOut #-}
filterMAllOut :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterMAllOut value n = composeN n $ Stream.filterM (\x -> return $ x > (value + 1))

filterMAllOut1 :: Int -> IO ()
filterMAllOut1 value = withStream value (filterMAllOut value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMAllOut1
inspect $ 'filterMAllOut1 `hasNoType` ''Stream.Step
inspect $ 'filterMAllOut1 `hasNoType` ''FL.Step
inspect $ 'filterMAllOut1 `hasNoType` ''SPEC
#endif

filterMAllOut4 :: Int -> IO ()
filterMAllOut4 value = withStream value (filterMAllOut value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMAllOut4
inspect $ 'filterMAllOut4 `hasNoType` ''Stream.Step
inspect $ 'filterMAllOut4 `hasNoType` ''FL.Step
inspect $ 'filterMAllOut4 `hasNoType` ''SPEC
#endif

{-# INLINE filterMAllIn #-}
filterMAllIn :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterMAllIn value n = composeN n $ Stream.filterM (\x -> return $ x <= (value + 1))

filterMAllIn1 :: Int -> IO ()
filterMAllIn1 value = withStream value (filterMAllIn value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMAllIn1
inspect $ 'filterMAllIn1 `hasNoType` ''Stream.Step
inspect $ 'filterMAllIn1 `hasNoType` ''FL.Step
inspect $ 'filterMAllIn1 `hasNoType` ''SPEC
#endif

filterMAllIn4 :: Int -> IO ()
filterMAllIn4 value = withStream value (filterMAllIn value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMAllIn4
inspect $ 'filterMAllIn4 `hasNoType` ''Stream.Step
inspect $ 'filterMAllIn4 `hasNoType` ''FL.Step
inspect $ 'filterMAllIn4 `hasNoType` ''SPEC
#endif

{-# INLINE dropOne #-}
dropOne :: MonadIO m => Int -> Stream m Int -> m ()
dropOne n = composeN n $ Stream.drop 1

dropOne1 :: Int -> IO ()
dropOne1 value = withStream value (dropOne 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropOne1
inspect $ 'dropOne1 `hasNoType` ''Stream.Step
inspect $ 'dropOne1 `hasNoType` ''FL.Step
inspect $ 'dropOne1 `hasNoType` ''SPEC
#endif

dropOne4 :: Int -> IO ()
dropOne4 value = withStream value (dropOne 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropOne4
inspect $ 'dropOne4 `hasNoType` ''Stream.Step
inspect $ 'dropOne4 `hasNoType` ''FL.Step
inspect $ 'dropOne4 `hasNoType` ''SPEC
#endif

{-# INLINE dropAll #-}
dropAll :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropAll value n = composeN n $ Stream.drop (value + 1)

dropAll1 :: Int -> IO ()
dropAll1 value = withStream value (dropAll value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropAll1
inspect $ 'dropAll1 `hasNoType` ''Stream.Step
inspect $ 'dropAll1 `hasNoType` ''FL.Step
inspect $ 'dropAll1 `hasNoType` ''SPEC
#endif

dropAll4 :: Int -> IO ()
dropAll4 value = withStream value (dropAll value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropAll4
inspect $ 'dropAll4 `hasNoType` ''Stream.Step
inspect $ 'dropAll4 `hasNoType` ''FL.Step
inspect $ 'dropAll4 `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileTrue #-}
dropWhileTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropWhileTrue value n = composeN n $ Stream.dropWhile (<= (value + 1))

dropWhileTrue1 :: Int -> IO ()
dropWhileTrue1 value = withStream value (dropWhileTrue value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropWhileTrue1
inspect $ 'dropWhileTrue1 `hasNoType` ''Stream.Step
inspect $ 'dropWhileTrue1 `hasNoType` ''Stream.DropWhileState
inspect $ 'dropWhileTrue1 `hasNoType` ''FL.Step
inspect $ 'dropWhileTrue1 `hasNoType` ''SPEC
#endif

dropWhileTrue4 :: Int -> IO ()
dropWhileTrue4 value = withStream value (dropWhileTrue value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropWhileTrue4
inspect $ 'dropWhileTrue4 `hasNoType` ''Stream.Step
inspect $ 'dropWhileTrue4 `hasNoType` ''Stream.DropWhileState
inspect $ 'dropWhileTrue4 `hasNoType` ''FL.Step
inspect $ 'dropWhileTrue4 `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileMTrue #-}
dropWhileMTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropWhileMTrue value n = composeN n $ Stream.dropWhileM (return . (<= (value + 1)))

dropWhileMTrue4 :: Int -> IO ()
dropWhileMTrue4 value = withStream value (dropWhileMTrue value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropWhileMTrue4
inspect $ 'dropWhileMTrue4 `hasNoType` ''Stream.Step
inspect $ 'dropWhileMTrue4 `hasNoType` ''Stream.DropWhileState
inspect $ 'dropWhileMTrue4 `hasNoType` ''FL.Step
inspect $ 'dropWhileMTrue4 `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileFalse #-}
dropWhileFalse :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropWhileFalse value n = composeN n $ Stream.dropWhile (> (value + 1))

dropWhileFalse1 :: Int -> IO ()
dropWhileFalse1 value = withStream value (dropWhileFalse value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropWhileFalse1
inspect $ 'dropWhileFalse1 `hasNoType` ''Stream.Step
inspect $ 'dropWhileFalse1 `hasNoType` ''Stream.DropWhileState
inspect $ 'dropWhileFalse1 `hasNoType` ''FL.Step
inspect $ 'dropWhileFalse1 `hasNoType` ''SPEC
#endif

dropWhileFalse4 :: Int -> IO ()
dropWhileFalse4 value = withStream value (dropWhileFalse value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropWhileFalse4
inspect $ 'dropWhileFalse4 `hasNoType` ''Stream.Step
inspect $ 'dropWhileFalse4 `hasNoType` ''Stream.DropWhileState
inspect $ 'dropWhileFalse4 `hasNoType` ''FL.Step
inspect $ 'dropWhileFalse4 `hasNoType` ''SPEC
#endif

{-# INLINE findIndices #-}
findIndices :: MonadIO m => Int -> Int -> Stream m Int -> m ()
findIndices value n = composeN n $ Stream.findIndices (== (value + 1))

findIndices1 :: Int -> IO ()
findIndices1 value = withStream value (findIndices value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'findIndices1
inspect $ 'findIndices1 `hasNoType` ''Stream.Step
inspect $ 'findIndices1 `hasNoType` ''FL.Step
inspect $ 'findIndices1 `hasNoType` ''SPEC
#endif

findIndices4 :: Int -> IO ()
findIndices4 value = withStream value (findIndices value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'findIndices4
inspect $ 'findIndices4 `hasNoType` ''Stream.Step
inspect $ 'findIndices4 `hasNoType` ''FL.Step
inspect $ 'findIndices4 `hasNoType` ''SPEC
#endif

{-# INLINE elemIndices #-}
elemIndices :: MonadIO m => Int -> Int -> Stream m Int -> m ()
elemIndices value n = composeN n $ Stream.elemIndices (value + 1)

elemIndices1 :: Int -> IO ()
elemIndices1 value = withStream value (elemIndices value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'elemIndices1
inspect $ 'elemIndices1 `hasNoType` ''Stream.Step
inspect $ 'elemIndices1 `hasNoType` ''FL.Step
inspect $ 'elemIndices1 `hasNoType` ''SPEC
#endif

elemIndices4 :: Int -> IO ()
elemIndices4 value = withStream value (elemIndices value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'elemIndices4
inspect $ 'elemIndices4 `hasNoType` ''Stream.Step
inspect $ 'elemIndices4 `hasNoType` ''FL.Step
inspect $ 'elemIndices4 `hasNoType` ''SPEC
#endif

findIndex :: Int -> IO (Maybe Int)
findIndex value = withStream value (Stream.head . Stream.findIndices (== (value + 1)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'findIndex
inspect $ 'findIndex `hasNoType` ''Stream.Step
inspect $ 'findIndex `hasNoType` ''FL.Step
inspect $ 'findIndex `hasNoType` ''SPEC
#endif

elemIndex :: Int -> IO (Maybe Int)
elemIndex value = withStream value (Stream.head . Stream.elemIndices (value + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'elemIndex
inspect $ 'elemIndex `hasNoType` ''Stream.Step
inspect $ 'elemIndex `hasNoType` ''FL.Step
inspect $ 'elemIndex `hasNoType` ''SPEC
#endif

{-# INLINE deleteBy #-}
deleteBy :: MonadIO m => Int -> Int -> Stream m Int -> m ()
deleteBy value n = composeN n $ Stream.deleteBy (>=) (value + 1)

deleteBy1 :: Int -> IO ()
deleteBy1 value = withStream value (deleteBy value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'deleteBy1
inspect $ 'deleteBy1 `hasNoType` ''Stream.Step
inspect $ 'deleteBy1 `hasNoType` ''FL.Step
inspect $ 'deleteBy1 `hasNoType` ''SPEC
#endif

deleteBy4 :: Int -> IO ()
deleteBy4 value = withStream value (deleteBy value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'deleteBy4
inspect $ 'deleteBy4 `hasNoType` ''Stream.Step
inspect $ 'deleteBy4 `hasNoType` ''FL.Step
inspect $ 'deleteBy4 `hasNoType` ''SPEC
#endif

-- uniq . uniq == uniq, composeN 2 ~ composeN 1
{-# INLINE uniq #-}
uniq :: MonadIO m => Int -> Stream m Int -> m ()
uniq n = composeN n Stream.uniq

uniq1 :: Int -> IO ()
uniq1 value = withStream value (uniq 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'uniq1
inspect $ 'uniq1 `hasNoType` ''Stream.Step
inspect $ 'uniq1 `hasNoType` ''FL.Step
inspect $ 'uniq1 `hasNoType` ''SPEC
#endif

uniq4 :: Int -> IO ()
uniq4 value = withStream value (uniq 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'uniq4
inspect $ 'uniq4 `hasNoType` ''Stream.Step
inspect $ 'uniq4 `hasNoType` ''FL.Step
inspect $ 'uniq4 `hasNoType` ''SPEC
#endif

{-# INLINE mapMaybe #-}
mapMaybe :: MonadIO m => Int -> Stream m Int -> m ()
mapMaybe n =
    composeN n $
    Stream.mapMaybe
        (\x ->
             if odd x
             then Nothing
             else Just x)

mapMaybe1 :: Int -> IO ()
mapMaybe1 value = withStream value (mapMaybe 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapMaybe1
inspect $ 'mapMaybe1 `hasNoType` ''Stream.Step
inspect $ 'mapMaybe1 `hasNoType` ''FL.Step
inspect $ 'mapMaybe1 `hasNoType` ''SPEC
#endif

mapMaybe4 :: Int -> IO ()
mapMaybe4 value = withStream value (mapMaybe 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapMaybe4
inspect $ 'mapMaybe4 `hasNoType` ''Stream.Step
inspect $ 'mapMaybe4 `hasNoType` ''FL.Step
inspect $ 'mapMaybe4 `hasNoType` ''SPEC
#endif

{-# INLINE mapMaybeM #-}
mapMaybeM :: MonadAsync m => Int -> Stream m Int -> m ()
mapMaybeM n =
    composeN n $
    Stream.mapMaybeM
        (\x ->
             if odd x
             then return Nothing
             else return $ Just x)

mapMaybeM1 :: Int -> IO ()
mapMaybeM1 value = withStream value (mapMaybeM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapMaybeM1
inspect $ 'mapMaybeM1 `hasNoType` ''Stream.Step
inspect $ 'mapMaybeM1 `hasNoType` ''FL.Step
inspect $ 'mapMaybeM1 `hasNoType` ''SPEC
#endif

mapMaybeM4 :: Int -> IO ()
mapMaybeM4 value = withStream value (mapMaybeM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapMaybeM4
inspect $ 'mapMaybeM4 `hasNoType` ''Stream.Step
inspect $ 'mapMaybeM4 `hasNoType` ''FL.Step
inspect $ 'mapMaybeM4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Size increasing transformations (insertions)
-------------------------------------------------------------------------------

{-# INLINE intersperse #-}
intersperse :: MonadAsync m => Int -> Int -> Stream m Int -> m ()
intersperse value n = composeN n $ Stream.intersperse (value + 1)

intersperse1 :: Int -> IO ()
intersperse1 value = withStream value (intersperse value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'intersperse1
inspect $ 'intersperse1 `hasNoType` ''Stream.Step
inspect $ 'intersperse1 `hasNoType` ''Stream.LoopState
inspect $ 'intersperse1 `hasNoType` ''FL.Step
inspect $ 'intersperse1 `hasNoType` ''SPEC
#endif

intersperse4 :: Int -> IO ()
intersperse4 value = withStream value (intersperse value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'intersperse4
inspect $ 'intersperse4 `hasNoType` ''Stream.Step
-- inspect $ 'intersperse4 `hasNoType` ''Stream.LoopState
inspect $ 'intersperse4 `hasNoType` ''FL.Step
-- inspect $ 'intersperse4 `hasNoType` ''SPEC
#endif

{-# INLINE intersperseM #-}
intersperseM :: MonadAsync m => Int -> Int -> Stream m Int -> m ()
intersperseM value n = composeN n $ Stream.intersperseM (return $ value + 1)

intersperseM1 :: Int -> IO ()
intersperseM1 value = withStream value (intersperseM value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'intersperseM1
inspect $ 'intersperseM1 `hasNoType` ''Stream.Step
inspect $ 'intersperseM1 `hasNoType` ''Stream.LoopState
inspect $ 'intersperseM1 `hasNoType` ''FL.Step
inspect $ 'intersperseM1 `hasNoType` ''SPEC
#endif

{-# INLINE insertBy #-}
insertBy :: MonadIO m => Int -> Int -> Stream m Int -> m ()
insertBy value n = composeN n $ Stream.insertBy compare (value + 1)

insertBy1 :: Int -> IO ()
insertBy1 value = withStream value (insertBy value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'insertBy1
inspect $ 'insertBy1 `hasNoType` ''Stream.Step
inspect $ 'insertBy1 `hasNoType` ''FL.Step
inspect $ 'insertBy1 `hasNoType` ''SPEC
#endif

insertBy4 :: Int -> IO ()
insertBy4 value = withStream value (insertBy value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'insertBy4
inspect $ 'insertBy4 `hasNoType` ''Stream.Step
inspect $ 'insertBy4 `hasNoType` ''FL.Step
inspect $ 'insertBy4 `hasNoType` ''SPEC
#endif

{-# INLINE interposeSuffix #-}
interposeSuffix :: Monad m => Int -> Int -> Stream m Int -> m ()
interposeSuffix value n =
    composeN n $ Stream.unfoldEachSepBy (value + 1) Unfold.identity

interposeSuffix1 :: Int -> IO ()
interposeSuffix1 value = withStream value (interposeSuffix value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'interposeSuffix1
inspect $ 'interposeSuffix1 `hasNoType` ''Stream.Step
inspect $ 'interposeSuffix1 `hasNoType` ''Stream.InterposeState
inspect $ 'interposeSuffix1 `hasNoType` ''FL.Step
inspect $ 'interposeSuffix1 `hasNoType` ''SPEC
#endif

{-# INLINE intercalateSuffix #-}
intercalateSuffix :: Monad m => Int -> Int -> Stream m Int -> m ()
intercalateSuffix value n =
    composeN n $ Stream.unfoldEachSepBySeq (value + 1) Unfold.identity

intercalateSuffix1 :: Int -> IO ()
intercalateSuffix1 value = withStream value (intercalateSuffix value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'intercalateSuffix1
inspect $ 'intercalateSuffix1 `hasNoType` ''Stream.Step
inspect $ 'intercalateSuffix1 `hasNoType` ''Stream.LoopState
inspect $ 'intercalateSuffix1 `hasNoType` ''Producer.ConcatState
inspect $ 'intercalateSuffix1 `hasNoType` ''FL.Step
inspect $ 'intercalateSuffix1 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Indexing
-------------------------------------------------------------------------------

{-# INLINE indexed #-}
indexed :: MonadIO m => Int -> Stream m Int -> m ()
indexed n = composeN n (fmap snd . Stream.indexed)

indexed1 :: Int -> IO ()
indexed1 value = withStream value (indexed 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'indexed1
inspect $ 'indexed1 `hasNoType` ''Stream.Step
inspect $ 'indexed1 `hasNoType` ''FL.Step
inspect $ 'indexed1 `hasNoType` ''SPEC
#endif

indexed4 :: Int -> IO ()
indexed4 value = withStream value (indexed 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'indexed4
inspect $ 'indexed4 `hasNoType` ''Stream.Step
inspect $ 'indexed4 `hasNoType` ''FL.Step
inspect $ 'indexed4 `hasNoType` ''SPEC
#endif

{-# INLINE indexedR #-}
indexedR :: MonadIO m => Int -> Int -> Stream m Int -> m ()
indexedR value n = composeN n (fmap snd . Stream.indexedR value)

indexedR1 :: Int -> IO ()
indexedR1 value = withStream value (indexedR value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'indexedR1
inspect $ 'indexedR1 `hasNoType` ''Stream.Step
inspect $ 'indexedR1 `hasNoType` ''FL.Step
inspect $ 'indexedR1 `hasNoType` ''SPEC
#endif

indexedR4 :: Int -> IO ()
indexedR4 value = withStream value (indexedR value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'indexedR4
inspect $ 'indexedR4 `hasNoType` ''Stream.Step
inspect $ 'indexedR4 `hasNoType` ''FL.Step
inspect $ 'indexedR4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Size conserving transformations (reordering, buffering, etc.)
-------------------------------------------------------------------------------

reverse :: Int -> IO ()
reverse value = withStream value (composeN 1 Stream.reverse)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'reverse
inspect $ 'reverse `hasNoType` ''Stream.Step
inspect $ 'reverse `hasNoType` ''FL.Step
-- inspect $ 'reverse `hasNoType` ''SPEC
#endif

reverse' :: Int -> IO ()
reverse' value = withStream value (composeN 1 Stream.reverseUnbox)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'reverse'
-- inspect $ 'reverse' `hasNoType` ''Stream.Step
#endif

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    [
    -- , benchIOSink value "foldrT" (foldrT 1)
    -- , benchIOSink value "foldrTMap" (foldrTMap 1)

    -- Mapping
      (SpaceO_1, benchIO "sequence" $ sequence1 size)
    , (SpaceO_1, benchIO "tap" $ tap1 size)
    -- XXX tasty-bench hangs benchmarking this
    -- , benchIOSink value "timestamped" _timestamped
    -- Scanning
    , (SpaceO_1, benchIO "scanl'" $ scanl'1 size)
    , (SpaceO_1, benchIO "scanl1'" $ scanl1'1 size)
    , (SpaceO_1, benchIO "scanlM'" $ scanlM'1 size)
    , (SpaceO_1, benchIO "scanl1M'" $ scanl1M'1 size)
    , (SpaceO_1, benchIO "postscanl'" $ postscanl'1 size)
    , (SpaceO_1, benchIO "postscanlM'" $ postscanlM'1 size)
    , (SpaceO_1, benchIO "scan" $ scan1 size)
    , (SpaceO_1, benchIO "postscan" $ postscan1 size)
    , (SpaceO_1, benchIO "trace x 4" $ trace4 size)

    , (SpaceO_1, benchIO "scanl' x 4" $ scanl'4 size)
    , (SpaceO_1, benchIO "scanl1' x 4" $ scanl1'4 size)
    , (SpaceO_1, benchIO "scanlM' x 4" $ scanlM'4 size)
    , (SpaceO_1, benchIO "scanl1M' x 4" $ scanl1M'4 size)
    , (SpaceO_1, benchIO "postscanl' x 4" $ postscanl'4 size)
    , (SpaceO_1, benchIO "postscanlM' x 4" $ postscanlM'4 size)
    , (SpaceO_1, benchIO "scan x 4" $ scan4 size)
    , (SpaceO_1, benchIO "postscan x 4" $ postscan4 size)
    , (SpaceO_1, benchIO "filter-even" $ filterEven1 size)
    , (SpaceO_1, benchIO "filter-all-out" $ filterAllOut1 size)
    , (SpaceO_1, benchIO "filter-all-in" $ filterAllIn1 size)

    , (SpaceO_1, benchIO "filterM-even" $ filterMEven1 size)
    , (SpaceO_1, benchIO "filterM-all-out" $ filterMAllOut1 size)
    , (SpaceO_1, benchIO "filterM-all-in" $ filterMAllIn1 size)

    , (SpaceO_1, benchIO "drop-one" $ dropOne1 size)
    , (SpaceO_1, benchIO "drop-all" $ dropAll1 size)
    , (SpaceO_1, benchIO "dropWhile-true" $ dropWhileTrue1 size)
 -- , (SpaceO_1, benchIO "dropWhileM-true" ...)
    , (SpaceO_1, benchIO "dropWhile-false" $ dropWhileFalse1 size)
    , (SpaceO_1, benchIO "deleteBy" $ deleteBy1 size)

    , (SpaceO_1, benchIO "uniq" $ uniq1 size)

    -- Map and filter
    , (SpaceO_1, benchIO "mapMaybe" $ mapMaybe1 size)
    , (SpaceO_1, benchIO "mapMaybeM" $ mapMaybeM1 size)

    -- Searching (stateful map and filter)
    , (SpaceO_1, benchIO "findIndices" $ findIndices1 size)
    , (SpaceO_1, benchIO "elemIndices" $ elemIndices1 size)
    , (SpaceO_1, benchIO "findIndex" $ findIndex size)
    , (SpaceO_1, benchIO "elemIndex" $ elemIndex size)
    , (SpaceO_1, benchIO "filter-even x 4" $ filterEven4 size)
    , (SpaceO_1, benchIO "filter-all-out x 4" $ filterAllOut4 size)
    , (SpaceO_1, benchIO "filter-all-in x 4" $ filterAllIn4 size)

    , (SpaceO_1, benchIO "filterM-even x 4" $ filterMEven4 size)
    , (SpaceO_1, benchIO "filterM-all-out x 4" $ filterMAllOut4 size)
    , (SpaceO_1, benchIO "filterM-all-in x 4" $ filterMAllIn4 size)

    , (SpaceO_1, benchIO "drop-one x 4" $ dropOne4 size)
    , (SpaceO_1, benchIO "drop-all x 4" $ dropAll4 size)
    , (SpaceO_1, benchIO "dropWhile-true x 4" $ dropWhileTrue4 size)
    , (SpaceO_1, benchIO "dropWhileM-true x 4" $ dropWhileMTrue4 size)
    -- XXX requires @-fspec-constr-recursive=12@.
    , (SpaceO_1, benchIO "dropWhile-false x 4" $ dropWhileFalse4 size)
    , (SpaceO_1, benchIO "deleteBy x 4" $ deleteBy4 size)

    , (SpaceO_1, benchIO "uniq x 4" $ uniq4 size)

    -- map and filter
    , (SpaceO_1, benchIO "mapMaybe x 4" $ mapMaybe4 size)
    , (SpaceO_1, benchIO "mapMaybeM x 4" $ mapMaybeM4 size)

    -- searching
    , (SpaceO_1, benchIO "findIndices x 4" $ findIndices4 size)
    , (SpaceO_1, benchIO "elemIndices x 4" $ elemIndices4 size)
    , (SpaceO_1, benchIO "intersperse" $ intersperse1 size)
    , (SpaceO_1, benchIO "intersperseM" $ intersperseM1 size)
    , (SpaceO_1, benchIO "insertBy" $ insertBy1 size)
    , (SpaceO_1, benchIO "interposeSuffix" $ interposeSuffix1 size)
    , (SpaceO_1, benchIO "intercalateSuffix" $ intercalateSuffix1 size)
    -- XXX requires @-fspec-constr-recursive=16@.
    , (SpaceO_1, benchIO "intersperse x 4" $ intersperse4 size)
    , (SpaceO_1, benchIO "insertBy x 4" $ insertBy4 size)
    , (SpaceO_1, benchIO "indexed" $ indexed1 size)
    , (SpaceO_1, benchIO "indexedR" $ indexedR1 size)
    , (SpaceO_1, benchIO "indexed x 4" $ indexed4 size)
    , (SpaceO_1, benchIO "indexedR x 4" $ indexedR4 size)
    , (SpaceO_n, benchIO "iterated/(+) (n times) (baseline)" $ iteratePlusBaseline size)
    , (SpaceO_n, benchIO "iterated/(<$) (n times)" $ iterateSubMap size)
    , (SpaceO_n, benchIO "iterated/fmap (n times)" $ iterateFmap size)
    {-
    , benchIOSrc fromSerial "_(<$) (n times)" $
        _iterateSingleton (<$) value
    , benchIOSrc fromSerial "_fmap (n times)" $
        _iterateSingleton (fmap . (+)) value
    -}
    -- Reversing a stream
    , (HeapO_n, benchIO "reverse" $ reverse size)
    , (HeapO_n, benchIO "reverse'" $ reverse' size)
    ]
