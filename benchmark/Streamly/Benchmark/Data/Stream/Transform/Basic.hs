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

{-# INLINE scanl'1 #-}
scanl'1 :: Int -> IO ()
scanl'1 value = withStream value (scanl' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl'1
inspect $ 'scanl'1 `hasNoType` ''Stream.Step
#endif

{-# INLINE scanl'4 #-}
scanl'4 :: Int -> IO ()
scanl'4 value = withStream value (scanl' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl'4
inspect $ 'scanl'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE scanlM' #-}
scanlM' :: MonadIO m => Int -> Stream m Int -> m ()
scanlM' n = composeN n $ Stream.scanlM' (\b a -> return $ b + a) (return 0)

{-# INLINE scanlM'1 #-}
scanlM'1 :: Int -> IO ()
scanlM'1 value = withStream value (scanlM' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanlM'1
inspect $ 'scanlM'1 `hasNoType` ''Stream.Step
#endif

{-# INLINE scanlM'4 #-}
scanlM'4 :: Int -> IO ()
scanlM'4 value = withStream value (scanlM' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanlM'4
inspect $ 'scanlM'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE scanl1' #-}
scanl1' :: MonadIO m => Int -> Stream m Int -> m ()
scanl1' n = composeN n $ Stream.scanl1' (+)

{-# INLINE scanl1'1 #-}
scanl1'1 :: Int -> IO ()
scanl1'1 value = withStream value (scanl1' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl1'1
inspect $ 'scanl1'1 `hasNoType` ''Stream.Step
#endif

{-# INLINE scanl1'4 #-}
scanl1'4 :: Int -> IO ()
scanl1'4 value = withStream value (scanl1' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl1'4
inspect $ 'scanl1'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE scanl1M' #-}
scanl1M' :: MonadIO m => Int -> Stream m Int -> m ()
scanl1M' n = composeN n $ Stream.scanl1M' (\b a -> return $ b + a)

{-# INLINE scanl1M'1 #-}
scanl1M'1 :: Int -> IO ()
scanl1M'1 value = withStream value (scanl1M' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl1M'1
inspect $ 'scanl1M'1 `hasNoType` ''Stream.Step
#endif

{-# INLINE scanl1M'4 #-}
scanl1M'4 :: Int -> IO ()
scanl1M'4 value = withStream value (scanl1M' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl1M'4
inspect $ 'scanl1M'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE scan #-}
scan :: MonadIO m => Int -> Stream m Int -> m ()
scan n = composeN n $ Stream.scanl Scanl.sum

{-# INLINE scan1 #-}
scan1 :: Int -> IO ()
scan1 value = withStream value (scan 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scan1
inspect $ 'scan1 `hasNoType` ''Stream.Step
inspect $ 'scan1 `hasNoType` ''Stream.ScanState
inspect $ 'scan1 `hasNoType` ''FL.Step
inspect $ 'scan1 `hasNoType` ''SPEC
#endif

{-# INLINE scan4 #-}
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

{-# INLINE postscan1 #-}
postscan1 :: Int -> IO ()
postscan1 value = withStream value (postscan 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscan1
inspect $ 'postscan1 `hasNoType` ''Stream.Step
inspect $ 'postscan1 `hasNoType` ''Stream.ScanState
inspect $ 'postscan1 `hasNoType` ''FL.Step
inspect $ 'postscan1 `hasNoType` ''SPEC
#endif

{-# INLINE postscan4 #-}
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

{-# INLINE postscanl'1 #-}
postscanl'1 :: Int -> IO ()
postscanl'1 value = withStream value (postscanl' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanl'1
inspect $ 'postscanl'1 `hasNoType` ''Stream.Step
#endif

{-# INLINE postscanl'4 #-}
postscanl'4 :: Int -> IO ()
postscanl'4 value = withStream value (postscanl' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanl'4
-- inspect $ 'postscanl'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE postscanlM' #-}
postscanlM' :: MonadIO m => Int -> Stream m Int -> m ()
postscanlM' n = composeN n $ Stream.postscanlM' (\b a -> return $ b + a) (return 0)

{-# INLINE postscanlM'1 #-}
postscanlM'1 :: Int -> IO ()
postscanlM'1 value = withStream value (postscanlM' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanlM'1
inspect $ 'postscanlM'1 `hasNoType` ''Stream.Step
#endif

{-# INLINE postscanlM'4 #-}
postscanlM'4 :: Int -> IO ()
postscanlM'4 value = withStream value (postscanlM' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanlM'4
inspect $ 'postscanlM'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE sequence #-}
sequence :: MonadAsync m => Stream m (m Int) -> m ()
sequence = Common.drain . Stream.sequence

{-# INLINE sequence1 #-}
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

{-# INLINE tap1 #-}
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

{-# INLINE trace4 #-}
trace4 :: Int -> IO ()
trace4 value = withStream value (trace 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'trace4
inspect $ 'trace4 `hasNoType` ''Stream.Step
inspect $ 'trace4 `hasNoType` ''FL.Step
inspect $ 'trace4 `hasNoType` ''SPEC
#endif

o_1_space_mapping :: Int -> [Benchmark]
o_1_space_mapping value =
    [ bgroup
        "mapping"
        [
        -- , benchIOSink value "foldrT" (foldrT 1)
        -- , benchIOSink value "foldrTMap" (foldrTMap 1)

        -- Mapping
          benchIO "sequence" $ sequence1 value
        , benchIO "tap" $ tap1 value
        -- XXX tasty-bench hangs benchmarking this
        -- , benchIOSink value "timestamped" _timestamped
        -- Scanning
        , benchIO "scanl'" $ scanl'1 value
        , benchIO "scanl1'" $ scanl1'1 value
        , benchIO "scanlM'" $ scanlM'1 value
        , benchIO "scanl1M'" $ scanl1M'1 value
        , benchIO "postscanl'" $ postscanl'1 value
        , benchIO "postscanlM'" $ postscanlM'1 value
        , benchIO "scan" $ scan1 value
        , benchIO "postscan" $ postscan1 value
        ]
    ]

o_1_space_mappingX4 :: Int -> [Benchmark]
o_1_space_mappingX4 value =
    [ bgroup "mappingX4"
        [ benchIO "trace" $ trace4 value

        , benchIO "scanl'" $ scanl'4 value
        , benchIO "scanl1'" $ scanl1'4 value
        , benchIO "scanlM'" $ scanlM'4 value
        , benchIO "scanl1M'" $ scanl1M'4 value
        , benchIO "postscanl'" $ postscanl'4 value
        , benchIO "postscanlM'" $ postscanlM'4 value
        , benchIO "scan" $ scan4 value
        , benchIO "postscan" $ postscan4 value
        ]
    ]

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

{-# INLINE iteratePlusBaseline #-}
iteratePlusBaseline :: Int -> IO Int
iteratePlusBaseline value =
    withRandomIntIO $ \i0 ->
        iterateN (\i acc -> acc >>= \n -> return $ i + n) (return i0) value

{-# INLINE iterateSubMap #-}
iterateSubMap :: Int -> IO ()
iterateSubMap value = withRandomIntIO $ drain . iterateSingleton (<$) value

{-# INLINE iterateFmap #-}
iterateFmap :: Int -> IO ()
iterateFmap value = withRandomIntIO $ drain . iterateSingleton (fmap . (+)) value

o_n_space_iterated :: Int -> [Benchmark]
o_n_space_iterated value =
    [ bgroup "iterated"
        [ benchIO "(+) (n times) (baseline)" $ iteratePlusBaseline value
        , benchIO "(<$) (n times)" $ iterateSubMap value
        , benchIO "fmap (n times)" $ iterateFmap value
        {-
        , benchIOSrc fromSerial "_(<$) (n times)" $
            _iterateSingleton (<$) value
        , benchIOSrc fromSerial "_fmap (n times)" $
            _iterateSingleton (fmap . (+)) value
        -}
        ]
    ]

-------------------------------------------------------------------------------
-- Size reducing transformations (filtering)
-------------------------------------------------------------------------------

{-# INLINE filterEven #-}
filterEven :: MonadIO m => Int -> Stream m Int -> m ()
filterEven n = composeN n $ Stream.filter even

{-# INLINE filterEven1 #-}
filterEven1 :: Int -> IO ()
filterEven1 value = withStream value (filterEven 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterEven1
inspect $ 'filterEven1 `hasNoType` ''Stream.Step
inspect $ 'filterEven1 `hasNoType` ''FL.Step
inspect $ 'filterEven1 `hasNoType` ''SPEC
#endif

{-# INLINE filterEven4 #-}
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

{-# INLINE filterAllOut1 #-}
filterAllOut1 :: Int -> IO ()
filterAllOut1 value = withStream value (filterAllOut value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterAllOut1
inspect $ 'filterAllOut1 `hasNoType` ''Stream.Step
inspect $ 'filterAllOut1 `hasNoType` ''FL.Step
inspect $ 'filterAllOut1 `hasNoType` ''SPEC
#endif

{-# INLINE filterAllOut4 #-}
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

{-# INLINE filterAllIn1 #-}
filterAllIn1 :: Int -> IO ()
filterAllIn1 value = withStream value (filterAllIn value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterAllIn1
inspect $ 'filterAllIn1 `hasNoType` ''Stream.Step
inspect $ 'filterAllIn1 `hasNoType` ''FL.Step
inspect $ 'filterAllIn1 `hasNoType` ''SPEC
#endif

{-# INLINE filterAllIn4 #-}
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

{-# INLINE filterMEven1 #-}
filterMEven1 :: Int -> IO ()
filterMEven1 value = withStream value (filterMEven 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMEven1
inspect $ 'filterMEven1 `hasNoType` ''Stream.Step
inspect $ 'filterMEven1 `hasNoType` ''FL.Step
inspect $ 'filterMEven1 `hasNoType` ''SPEC
#endif

{-# INLINE filterMEven4 #-}
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

{-# INLINE filterMAllOut1 #-}
filterMAllOut1 :: Int -> IO ()
filterMAllOut1 value = withStream value (filterMAllOut value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMAllOut1
inspect $ 'filterMAllOut1 `hasNoType` ''Stream.Step
inspect $ 'filterMAllOut1 `hasNoType` ''FL.Step
inspect $ 'filterMAllOut1 `hasNoType` ''SPEC
#endif

{-# INLINE filterMAllOut4 #-}
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

{-# INLINE filterMAllIn1 #-}
filterMAllIn1 :: Int -> IO ()
filterMAllIn1 value = withStream value (filterMAllIn value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMAllIn1
inspect $ 'filterMAllIn1 `hasNoType` ''Stream.Step
inspect $ 'filterMAllIn1 `hasNoType` ''FL.Step
inspect $ 'filterMAllIn1 `hasNoType` ''SPEC
#endif

{-# INLINE filterMAllIn4 #-}
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

{-# INLINE dropOne1 #-}
dropOne1 :: Int -> IO ()
dropOne1 value = withStream value (dropOne 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropOne1
inspect $ 'dropOne1 `hasNoType` ''Stream.Step
inspect $ 'dropOne1 `hasNoType` ''FL.Step
inspect $ 'dropOne1 `hasNoType` ''SPEC
#endif

{-# INLINE dropOne4 #-}
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

{-# INLINE dropAll1 #-}
dropAll1 :: Int -> IO ()
dropAll1 value = withStream value (dropAll value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropAll1
inspect $ 'dropAll1 `hasNoType` ''Stream.Step
inspect $ 'dropAll1 `hasNoType` ''FL.Step
inspect $ 'dropAll1 `hasNoType` ''SPEC
#endif

{-# INLINE dropAll4 #-}
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

{-# INLINE dropWhileTrue1 #-}
dropWhileTrue1 :: Int -> IO ()
dropWhileTrue1 value = withStream value (dropWhileTrue value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropWhileTrue1
inspect $ 'dropWhileTrue1 `hasNoType` ''Stream.Step
inspect $ 'dropWhileTrue1 `hasNoType` ''Stream.DropWhileState
inspect $ 'dropWhileTrue1 `hasNoType` ''FL.Step
inspect $ 'dropWhileTrue1 `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileTrue4 #-}
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

{-# INLINE dropWhileMTrue4 #-}
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

{-# INLINE dropWhileFalse1 #-}
dropWhileFalse1 :: Int -> IO ()
dropWhileFalse1 value = withStream value (dropWhileFalse value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropWhileFalse1
inspect $ 'dropWhileFalse1 `hasNoType` ''Stream.Step
inspect $ 'dropWhileFalse1 `hasNoType` ''Stream.DropWhileState
inspect $ 'dropWhileFalse1 `hasNoType` ''FL.Step
inspect $ 'dropWhileFalse1 `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileFalse4 #-}
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

{-# INLINE findIndices1 #-}
findIndices1 :: Int -> IO ()
findIndices1 value = withStream value (findIndices value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'findIndices1
inspect $ 'findIndices1 `hasNoType` ''Stream.Step
inspect $ 'findIndices1 `hasNoType` ''FL.Step
inspect $ 'findIndices1 `hasNoType` ''SPEC
#endif

{-# INLINE findIndices4 #-}
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

{-# INLINE elemIndices1 #-}
elemIndices1 :: Int -> IO ()
elemIndices1 value = withStream value (elemIndices value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'elemIndices1
inspect $ 'elemIndices1 `hasNoType` ''Stream.Step
inspect $ 'elemIndices1 `hasNoType` ''FL.Step
inspect $ 'elemIndices1 `hasNoType` ''SPEC
#endif

{-# INLINE elemIndices4 #-}
elemIndices4 :: Int -> IO ()
elemIndices4 value = withStream value (elemIndices value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'elemIndices4
inspect $ 'elemIndices4 `hasNoType` ''Stream.Step
inspect $ 'elemIndices4 `hasNoType` ''FL.Step
inspect $ 'elemIndices4 `hasNoType` ''SPEC
#endif

{-# INLINE findIndex #-}
findIndex :: Int -> IO (Maybe Int)
findIndex value = withStream value (Stream.head . Stream.findIndices (== (value + 1)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'findIndex
inspect $ 'findIndex `hasNoType` ''Stream.Step
inspect $ 'findIndex `hasNoType` ''FL.Step
inspect $ 'findIndex `hasNoType` ''SPEC
#endif

{-# INLINE elemIndex #-}
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

{-# INLINE deleteBy1 #-}
deleteBy1 :: Int -> IO ()
deleteBy1 value = withStream value (deleteBy value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'deleteBy1
inspect $ 'deleteBy1 `hasNoType` ''Stream.Step
inspect $ 'deleteBy1 `hasNoType` ''FL.Step
inspect $ 'deleteBy1 `hasNoType` ''SPEC
#endif

{-# INLINE deleteBy4 #-}
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

{-# INLINE uniq1 #-}
uniq1 :: Int -> IO ()
uniq1 value = withStream value (uniq 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'uniq1
inspect $ 'uniq1 `hasNoType` ''Stream.Step
inspect $ 'uniq1 `hasNoType` ''FL.Step
inspect $ 'uniq1 `hasNoType` ''SPEC
#endif

{-# INLINE uniq4 #-}
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

{-# INLINE mapMaybe1 #-}
mapMaybe1 :: Int -> IO ()
mapMaybe1 value = withStream value (mapMaybe 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapMaybe1
inspect $ 'mapMaybe1 `hasNoType` ''Stream.Step
inspect $ 'mapMaybe1 `hasNoType` ''FL.Step
inspect $ 'mapMaybe1 `hasNoType` ''SPEC
#endif

{-# INLINE mapMaybe4 #-}
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

{-# INLINE mapMaybeM1 #-}
mapMaybeM1 :: Int -> IO ()
mapMaybeM1 value = withStream value (mapMaybeM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapMaybeM1
inspect $ 'mapMaybeM1 `hasNoType` ''Stream.Step
inspect $ 'mapMaybeM1 `hasNoType` ''FL.Step
inspect $ 'mapMaybeM1 `hasNoType` ''SPEC
#endif

{-# INLINE mapMaybeM4 #-}
mapMaybeM4 :: Int -> IO ()
mapMaybeM4 value = withStream value (mapMaybeM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapMaybeM4
inspect $ 'mapMaybeM4 `hasNoType` ''Stream.Step
inspect $ 'mapMaybeM4 `hasNoType` ''FL.Step
inspect $ 'mapMaybeM4 `hasNoType` ''SPEC
#endif

o_1_space_filtering :: Int -> [Benchmark]
o_1_space_filtering value =
    [ bgroup "filtering"
        [ benchIO "filter-even" $ filterEven1 value
        , benchIO "filter-all-out" $ filterAllOut1 value
        , benchIO "filter-all-in" $ filterAllIn1 value

        , benchIO "filterM-even" $ filterMEven1 value
        , benchIO "filterM-all-out" $ filterMAllOut1 value
        , benchIO "filterM-all-in" $ filterMAllIn1 value

        , benchIO "drop-one" $ dropOne1 value
        , benchIO "drop-all" $ dropAll1 value
        , benchIO "dropWhile-true" $ dropWhileTrue1 value
     -- , benchIO "dropWhileM-true" ...
        , benchIO "dropWhile-false" $ dropWhileFalse1 value
        , benchIO "deleteBy" $ deleteBy1 value

        , benchIO "uniq" $ uniq1 value

        -- Map and filter
        , benchIO "mapMaybe" $ mapMaybe1 value
        , benchIO "mapMaybeM" $ mapMaybeM1 value

        -- Searching (stateful map and filter)
        , benchIO "findIndices" $ findIndices1 value
        , benchIO "elemIndices" $ elemIndices1 value
        , benchIO "findIndex" $ findIndex value
        , benchIO "elemIndex" $ elemIndex value
        ]
    ]

o_1_space_filteringX4 :: Int -> [Benchmark]
o_1_space_filteringX4 value =
    [ bgroup "filteringX4"
        [ benchIO "filter-even" $ filterEven4 value
        , benchIO "filter-all-out" $ filterAllOut4 value
        , benchIO "filter-all-in" $ filterAllIn4 value

        , benchIO "filterM-even" $ filterMEven4 value
        , benchIO "filterM-all-out" $ filterMAllOut4 value
        , benchIO "filterM-all-in" $ filterMAllIn4 value

        , benchIO "drop-one" $ dropOne4 value
        , benchIO "drop-all" $ dropAll4 value
        , benchIO "dropWhile-true" $ dropWhileTrue4 value
        , benchIO "dropWhileM-true" $ dropWhileMTrue4 value
        -- XXX requires @-fspec-constr-recursive=12@.
        , benchIO "dropWhile-false" $ dropWhileFalse4 value
        , benchIO "deleteBy" $ deleteBy4 value

        , benchIO "uniq" $ uniq4 value

        -- map and filter
        , benchIO "mapMaybe" $ mapMaybe4 value
        , benchIO "mapMaybeM" $ mapMaybeM4 value

        -- searching
        , benchIO "findIndices" $ findIndices4 value
        , benchIO "elemIndices" $ elemIndices4 value
        ]
    ]

-------------------------------------------------------------------------------
-- Size increasing transformations (insertions)
-------------------------------------------------------------------------------

{-# INLINE intersperse #-}
intersperse :: MonadAsync m => Int -> Int -> Stream m Int -> m ()
intersperse value n = composeN n $ Stream.intersperse (value + 1)

{-# INLINE intersperse1 #-}
intersperse1 :: Int -> IO ()
intersperse1 value = withStream value (intersperse value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'intersperse1
inspect $ 'intersperse1 `hasNoType` ''Stream.Step
inspect $ 'intersperse1 `hasNoType` ''Stream.LoopState
inspect $ 'intersperse1 `hasNoType` ''FL.Step
inspect $ 'intersperse1 `hasNoType` ''SPEC
#endif

{-# INLINE intersperse4 #-}
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

{-# INLINE intersperseM1 #-}
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

{-# INLINE insertBy1 #-}
insertBy1 :: Int -> IO ()
insertBy1 value = withStream value (insertBy value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'insertBy1
inspect $ 'insertBy1 `hasNoType` ''Stream.Step
inspect $ 'insertBy1 `hasNoType` ''FL.Step
inspect $ 'insertBy1 `hasNoType` ''SPEC
#endif

{-# INLINE insertBy4 #-}
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

{-# INLINE interposeSuffix1 #-}
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

{-# INLINE intercalateSuffix1 #-}
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

o_1_space_inserting :: Int -> [Benchmark]
o_1_space_inserting value =
    [ bgroup "inserting"
        [ benchIO "intersperse" $ intersperse1 value
        , benchIO "intersperseM" $ intersperseM1 value
        , benchIO "insertBy" $ insertBy1 value
        , benchIO "interposeSuffix" $ interposeSuffix1 value
        , benchIO "intercalateSuffix" $ intercalateSuffix1 value
        ]
    ]

o_1_space_insertingX4 :: Int -> [Benchmark]
o_1_space_insertingX4 value =
    [ bgroup "insertingX4"
        [
          -- XXX requires @-fspec-constr-recursive=16@.
          benchIO "intersperse" $ intersperse4 value
        , benchIO "insertBy" $ insertBy4 value
        ]
    ]

-------------------------------------------------------------------------------
-- Indexing
-------------------------------------------------------------------------------

{-# INLINE indexed #-}
indexed :: MonadIO m => Int -> Stream m Int -> m ()
indexed n = composeN n (fmap snd . Stream.indexed)

{-# INLINE indexed1 #-}
indexed1 :: Int -> IO ()
indexed1 value = withStream value (indexed 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'indexed1
inspect $ 'indexed1 `hasNoType` ''Stream.Step
inspect $ 'indexed1 `hasNoType` ''FL.Step
inspect $ 'indexed1 `hasNoType` ''SPEC
#endif

{-# INLINE indexed4 #-}
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

{-# INLINE indexedR1 #-}
indexedR1 :: Int -> IO ()
indexedR1 value = withStream value (indexedR value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'indexedR1
inspect $ 'indexedR1 `hasNoType` ''Stream.Step
inspect $ 'indexedR1 `hasNoType` ''FL.Step
inspect $ 'indexedR1 `hasNoType` ''SPEC
#endif

{-# INLINE indexedR4 #-}
indexedR4 :: Int -> IO ()
indexedR4 value = withStream value (indexedR value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'indexedR4
inspect $ 'indexedR4 `hasNoType` ''Stream.Step
inspect $ 'indexedR4 `hasNoType` ''FL.Step
inspect $ 'indexedR4 `hasNoType` ''SPEC
#endif

o_1_space_indexing :: Int -> [Benchmark]
o_1_space_indexing value =
    [ bgroup "indexing"
        [ benchIO "indexed" $ indexed1 value
        , benchIO "indexedR" $ indexedR1 value
        ]
    ]

o_1_space_indexingX4 :: Int -> [Benchmark]
o_1_space_indexingX4 value =
    [ bgroup "indexingx4"
        [ benchIO "indexed" $ indexed4 value
        , benchIO "indexedR" $ indexedR4 value
        ]
    ]

-------------------------------------------------------------------------------
-- Size conserving transformations (reordering, buffering, etc.)
-------------------------------------------------------------------------------

{-# INLINE reverse #-}
reverse :: Int -> IO ()
reverse value = withStream value (composeN 1 Stream.reverse)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'reverse
inspect $ 'reverse `hasNoType` ''Stream.Step
inspect $ 'reverse `hasNoType` ''FL.Step
-- inspect $ 'reverse `hasNoType` ''SPEC
#endif

{-# INLINE reverse' #-}
reverse' :: Int -> IO ()
reverse' value = withStream value (composeN 1 Stream.reverseUnbox)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'reverse'
-- inspect $ 'reverse' `hasNoType` ''Stream.Step
#endif

o_n_heap_buffering :: Int -> [Benchmark]
o_n_heap_buffering value =
    [ bgroup "buffered"
        [
        -- Reversing a stream
          benchIO "reverse" $ reverse value
        , benchIO "reverse'" $ reverse' value
        ]
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    map (SpaceO_1,) (Prelude.concat
        [ o_1_space_mapping size
        , o_1_space_mappingX4 size
        , o_1_space_filtering size
        , o_1_space_filteringX4 size
        , o_1_space_inserting size
        , o_1_space_insertingX4 size
        , o_1_space_indexing size
        , o_1_space_indexingX4 size
        ])
    ++ map (SpaceO_n,) (o_n_space_iterated size)
    ++ map (HeapO_n,) (o_n_heap_buffering size)
