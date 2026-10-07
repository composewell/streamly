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

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Stream.Transform.Basic (benchmarks) where

#ifdef INSPECTION
import Test.Inspection
import qualified Streamly.Internal.Data.Producer as Producer
#endif

import GHC.Types (SPEC(..))
import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Internal.Data.Stream (Stream, Step, LoopState)

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold

import Test.Tasty.Bench
import Stream.Common hiding (scanl', benchIO)
import Stream.Type (benchIO, withStream)
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import Streamly.Internal.Data.SVar.Type (State)
import Streamly.Data.Array (Array)
import Streamly.Data.MutByteArray (MutByteArray)
import Prelude hiding (sequence, mapM, reverse)
import Streamly.Internal.Data.MutArray (GroupState)

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

{-# ANN scanl'_x1 (PermitPatternMatches []) #-}
{-# ANN scanl'_x1 (PermitConstructions []) #-}
{-# ANN scanl'_x1 (PermitTypeClasses []) #-}
{-# NOINLINE scanl'_x1 #-}
scanl'_x1 :: Int -> Int -> IO ()
scanl'_x1 value = withStream value (scanl' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl'_x1
inspect $ 'scanl'_x1 `hasNoType` ''Step
#endif

{-# ANN scanl'_x4 (PermitPatternMatches []) #-}
{-# ANN scanl'_x4 (PermitConstructions []) #-}
{-# ANN scanl'_x4 (PermitTypeClasses []) #-}
{-# NOINLINE scanl'_x4 #-}
scanl'_x4 :: Int -> Int -> IO ()
scanl'_x4 value = withStream value (scanl' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl'_x4
inspect $ 'scanl'_x4 `hasNoType` ''Step
#endif

{-# INLINE scanlM' #-}
scanlM' :: MonadIO m => Int -> Stream m Int -> m ()
scanlM' n = composeN n $ Stream.scanlM' (\b a -> return $ b + a) (return 0)

{-# ANN scanlM'_x1 (PermitPatternMatches []) #-}
{-# ANN scanlM'_x1 (PermitConstructions []) #-}
{-# ANN scanlM'_x1 (PermitTypeClasses []) #-}
{-# NOINLINE scanlM'_x1 #-}
scanlM'_x1 :: Int -> Int -> IO ()
scanlM'_x1 value = withStream value (scanlM' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanlM'_x1
inspect $ 'scanlM'_x1 `hasNoType` ''Step
#endif

{-# ANN scanlM'_x4 (PermitPatternMatches []) #-}
{-# ANN scanlM'_x4 (PermitConstructions []) #-}
{-# ANN scanlM'_x4 (PermitTypeClasses []) #-}
{-# NOINLINE scanlM'_x4 #-}
scanlM'_x4 :: Int -> Int -> IO ()
scanlM'_x4 value = withStream value (scanlM' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanlM'_x4
inspect $ 'scanlM'_x4 `hasNoType` ''Step
#endif

{-# INLINE scanl1' #-}
scanl1' :: MonadIO m => Int -> Stream m Int -> m ()
scanl1' n = composeN n $ Stream.scanl1' (+)

{-# ANN scanl1'_x1 (PermitPatternMatches []) #-}
{-# ANN scanl1'_x1 (PermitConstructions []) #-}
{-# ANN scanl1'_x1 (PermitTypeClasses []) #-}
{-# NOINLINE scanl1'_x1 #-}
scanl1'_x1 :: Int -> Int -> IO ()
scanl1'_x1 value = withStream value (scanl1' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl1'_x1
inspect $ 'scanl1'_x1 `hasNoType` ''Step
#endif

{-# ANN scanl1'_x4 (PermitPatternMatches []) #-}
{-# ANN scanl1'_x4 (PermitConstructions []) #-}
{-# ANN scanl1'_x4 (PermitTypeClasses []) #-}
{-# NOINLINE scanl1'_x4 #-}
scanl1'_x4 :: Int -> Int -> IO ()
scanl1'_x4 value = withStream value (scanl1' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl1'_x4
inspect $ 'scanl1'_x4 `hasNoType` ''Step
#endif

{-# INLINE scanl1M' #-}
scanl1M' :: MonadIO m => Int -> Stream m Int -> m ()
scanl1M' n = composeN n $ Stream.scanl1M' (\b a -> return $ b + a)

{-# ANN scanl1M'_x1 (PermitPatternMatches []) #-}
{-# ANN scanl1M'_x1 (PermitConstructions []) #-}
{-# ANN scanl1M'_x1 (PermitTypeClasses []) #-}
{-# NOINLINE scanl1M'_x1 #-}
scanl1M'_x1 :: Int -> Int -> IO ()
scanl1M'_x1 value = withStream value (scanl1M' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl1M'_x1
inspect $ 'scanl1M'_x1 `hasNoType` ''Step
#endif

{-# ANN scanl1M'_x4 (PermitPatternMatches []) #-}
{-# ANN scanl1M'_x4 (PermitConstructions []) #-}
{-# ANN scanl1M'_x4 (PermitTypeClasses []) #-}
{-# NOINLINE scanl1M'_x4 #-}
scanl1M'_x4 :: Int -> Int -> IO ()
scanl1M'_x4 value = withStream value (scanl1M' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl1M'_x4
inspect $ 'scanl1M'_x4 `hasNoType` ''Step
#endif

{-# INLINE scan #-}
scan :: MonadIO m => Int -> Stream m Int -> m ()
scan n = composeN n $ Stream.scanl Scanl.sum

{-# ANN scanl_x1 (PermitPatternMatches []) #-}
{-# ANN scanl_x1 (PermitConstructions []) #-}
{-# ANN scanl_x1 (PermitTypeClasses []) #-}
{-# NOINLINE scanl_x1 #-}
scanl_x1 :: Int -> Int -> IO ()
scanl_x1 value = withStream value (scan 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl_x1
inspect $ 'scanl_x1 `hasNoType` ''Step
inspect $ 'scanl_x1 `hasNoType` ''Stream.ScanState
inspect $ 'scanl_x1 `hasNoType` ''FL.Step
inspect $ 'scanl_x1 `hasNoType` ''SPEC
#endif

{-# ANN scanl_x4 (PermitPatternMatches []) #-}
{-# ANN scanl_x4 (PermitConstructions []) #-}
{-# ANN scanl_x4 (PermitTypeClasses []) #-}
{-# NOINLINE scanl_x4 #-}
scanl_x4 :: Int -> Int -> IO ()
scanl_x4 value = withStream value (scan 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl_x4
inspect $ 'scanl_x4 `hasNoType` ''Step
inspect $ 'scanl_x4 `hasNoType` ''Stream.ScanState
inspect $ 'scanl_x4 `hasNoType` ''FL.Step
inspect $ 'scanl_x4 `hasNoType` ''SPEC
#endif

{-# INLINE postscan #-}
postscan :: MonadIO m => Int -> Stream m Int -> m ()
postscan n = composeN n $ Stream.postscanl Scanl.sum

{-# ANN postscanl_x1 (PermitPatternMatches []) #-}
{-# ANN postscanl_x1 (PermitConstructions []) #-}
{-# ANN postscanl_x1 (PermitTypeClasses []) #-}
{-# NOINLINE postscanl_x1 #-}
postscanl_x1 :: Int -> Int -> IO ()
postscanl_x1 value = withStream value (postscan 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanl_x1
inspect $ 'postscanl_x1 `hasNoType` ''Step
inspect $ 'postscanl_x1 `hasNoType` ''Stream.ScanState
inspect $ 'postscanl_x1 `hasNoType` ''FL.Step
inspect $ 'postscanl_x1 `hasNoType` ''SPEC
#endif

{-# ANN postscanl_x4 (PermitPatternMatches []) #-}
{-# ANN postscanl_x4 (PermitConstructions []) #-}
{-# ANN postscanl_x4 (PermitTypeClasses []) #-}
{-# NOINLINE postscanl_x4 #-}
postscanl_x4 :: Int -> Int -> IO ()
postscanl_x4 value = withStream value (postscan 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanl_x4
inspect $ 'postscanl_x4 `hasNoType` ''Step
inspect $ 'postscanl_x4 `hasNoType` ''Stream.ScanState
inspect $ 'postscanl_x4 `hasNoType` ''FL.Step
inspect $ 'postscanl_x4 `hasNoType` ''SPEC
#endif

{-# INLINE postscanl' #-}
postscanl' :: MonadIO m => Int -> Stream m Int -> m ()
postscanl' n = composeN n $ Stream.postscanl' (+) 0

{-# ANN postscanl'_x1 (PermitPatternMatches []) #-}
{-# ANN postscanl'_x1 (PermitConstructions []) #-}
{-# ANN postscanl'_x1 (PermitTypeClasses []) #-}
{-# NOINLINE postscanl'_x1 #-}
postscanl'_x1 :: Int -> Int -> IO ()
postscanl'_x1 value = withStream value (postscanl' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanl'_x1
inspect $ 'postscanl'_x1 `hasNoType` ''Step
#endif

{-# ANN postscanl'_x4 (PermitPatternMatches
    [''Maybe,''(,),''Int,''Step,''Stream,''State]) #-}
{-# ANN postscanl'_x4 (PermitConstructions
    [''Int,''(,),''Maybe,''Step,''State,''Stream,''Bool]) #-}
{-# ANN postscanl'_x4 (PermitTypeClasses []) #-}
{-# NOINLINE postscanl'_x4 #-}
postscanl'_x4 :: Int -> Int -> IO ()
postscanl'_x4 value = withStream value (postscanl' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanl'_x4
-- inspect $ 'postscanl'_x4 `hasNoType` ''Step
#endif

{-# INLINE postscanlM' #-}
postscanlM' :: MonadIO m => Int -> Stream m Int -> m ()
postscanlM' n =
    composeN n $ Stream.postscanlM' (\b a -> return $ b + a) (return 0)

{-# ANN postscanlM'_x1 (PermitPatternMatches []) #-}
{-# ANN postscanlM'_x1 (PermitConstructions []) #-}
{-# ANN postscanlM'_x1 (PermitTypeClasses []) #-}
{-# NOINLINE postscanlM'_x1 #-}
postscanlM'_x1 :: Int -> Int -> IO ()
postscanlM'_x1 value = withStream value (postscanlM' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanlM'_x1
inspect $ 'postscanlM'_x1 `hasNoType` ''Step
#endif

{-# ANN postscanlM'_x4 (PermitPatternMatches []) #-}
{-# ANN postscanlM'_x4 (PermitConstructions []) #-}
{-# ANN postscanlM'_x4 (PermitTypeClasses []) #-}
{-# NOINLINE postscanlM'_x4 #-}
postscanlM'_x4 :: Int -> Int -> IO ()
postscanlM'_x4 value = withStream value (postscanlM' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanlM'_x4
inspect $ 'postscanlM'_x4 `hasNoType` ''Step
#endif

{-# INLINE sequence #-}
sequence :: MonadAsync m => Stream m (m Int) -> m ()
sequence = Common.drain . Stream.sequence

{-# ANN sequence_x1 (PermitPatternMatches []) #-}
{-# ANN sequence_x1 (PermitConstructions []) #-}
{-# ANN sequence_x1 (PermitTypeClasses []) #-}
{-# NOINLINE sequence_x1 #-}
sequence_x1 :: Int -> Int -> IO ()
sequence_x1 value = sequence . sourceUnfoldrAction value

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sequence_x1
inspect $ 'sequence_x1 `hasNoType` ''Step
inspect $ 'sequence_x1 `hasNoType` ''FL.Step
inspect $ 'sequence_x1 `hasNoType` ''SPEC
#endif

{-# INLINE tap #-}
tap :: MonadIO m => Int -> Stream m Int -> m ()
tap n = composeN n $ Stream.tap FL.sum

{-# ANN tap_x1 (PermitPatternMatches []) #-}
{-# ANN tap_x1 (PermitConstructions []) #-}
{-# ANN tap_x1 (PermitTypeClasses []) #-}
{-# NOINLINE tap_x1 #-}
tap_x1 :: Int -> Int -> IO ()
tap_x1 value = withStream value (tap 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'tap_x1
inspect $ 'tap_x1 `hasNoType` ''Step
inspect $ 'tap_x1 `hasNoType` ''Stream.TapState
inspect $ 'tap_x1 `hasNoType` ''FL.Step
inspect $ 'tap_x1 `hasNoType` ''SPEC
#endif

{-# INLINE _timestamped #-}
_timestamped :: MonadIO m => Stream m Int -> m ()
_timestamped = Stream.drain . Stream.timestamped
{-
{-# INLINE foldrT #-}
foldrT :: MonadIO m => Int -> Stream m Int -> m ()
foldrT n =
    composeN n (unCrossStream . Stream.foldrT cns (CrossStream Stream.nil))

    where cns x (CrossStream xs) = CrossStream (Stream.cons x xs)

{-# INLINE foldrTMap #-}
foldrTMap :: MonadIO m => Int -> Stream m Int -> m ()
foldrTMap n =
    composeN n $ Stream.foldrT (\x xs -> x + 1 `Stream.cons` xs) Stream.nil
-}

{-# INLINE trace #-}
trace :: MonadAsync m => Int -> Stream m Int -> m ()
trace n = composeN n $ Stream.trace return

{-# ANN trace_x4 (PermitPatternMatches []) #-}
{-# ANN trace_x4 (PermitConstructions []) #-}
{-# ANN trace_x4 (PermitTypeClasses []) #-}
{-# NOINLINE trace_x4 #-}
trace_x4 :: Int -> Int -> IO ()
trace_x4 value = withStream value (trace 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'trace_x4
inspect $ 'trace_x4 `hasNoType` ''Step
inspect $ 'trace_x4 `hasNoType` ''FL.Step
inspect $ 'trace_x4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Size reducing transformations (filtering)
-------------------------------------------------------------------------------

{-# INLINE filterEven #-}
filterEven :: MonadIO m => Int -> Stream m Int -> m ()
filterEven n = composeN n $ Stream.filter even

{-# ANN filter_Even_x1 (PermitPatternMatches []) #-}
{-# ANN filter_Even_x1 (PermitConstructions []) #-}
{-# ANN filter_Even_x1 (PermitTypeClasses []) #-}
{-# NOINLINE filter_Even_x1 #-}
filter_Even_x1 :: Int -> Int -> IO ()
filter_Even_x1 value = withStream value (filterEven 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_Even_x1
inspect $ 'filter_Even_x1 `hasNoType` ''Step
inspect $ 'filter_Even_x1 `hasNoType` ''FL.Step
inspect $ 'filter_Even_x1 `hasNoType` ''SPEC
#endif

{-# ANN filter_Even_x4 (PermitPatternMatches []) #-}
{-# ANN filter_Even_x4 (PermitConstructions []) #-}
{-# ANN filter_Even_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filter_Even_x4 #-}
filter_Even_x4 :: Int -> Int -> IO ()
filter_Even_x4 value = withStream value (filterEven 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_Even_x4
inspect $ 'filter_Even_x4 `hasNoType` ''Step
inspect $ 'filter_Even_x4 `hasNoType` ''FL.Step
inspect $ 'filter_Even_x4 `hasNoType` ''SPEC
#endif

{-# INLINE filterAllOut #-}
filterAllOut :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterAllOut value n = composeN n $ Stream.filter (> (value + 1))

{-# ANN filter_AllOut_x1 (PermitPatternMatches []) #-}
{-# ANN filter_AllOut_x1 (PermitConstructions []) #-}
{-# ANN filter_AllOut_x1 (PermitTypeClasses []) #-}
{-# NOINLINE filter_AllOut_x1 #-}
filter_AllOut_x1 :: Int -> Int -> IO ()
filter_AllOut_x1 value = withStream value (filterAllOut value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_AllOut_x1
inspect $ 'filter_AllOut_x1 `hasNoType` ''Step
inspect $ 'filter_AllOut_x1 `hasNoType` ''FL.Step
inspect $ 'filter_AllOut_x1 `hasNoType` ''SPEC
#endif

{-# ANN filter_AllOut_x4 (PermitPatternMatches []) #-}
{-# ANN filter_AllOut_x4 (PermitConstructions []) #-}
{-# ANN filter_AllOut_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filter_AllOut_x4 #-}
filter_AllOut_x4 :: Int -> Int -> IO ()
filter_AllOut_x4 value = withStream value (filterAllOut value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_AllOut_x4
inspect $ 'filter_AllOut_x4 `hasNoType` ''Step
inspect $ 'filter_AllOut_x4 `hasNoType` ''FL.Step
inspect $ 'filter_AllOut_x4 `hasNoType` ''SPEC
#endif

{-# INLINE filterAllIn #-}
filterAllIn :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterAllIn value n = composeN n $ Stream.filter (<= (value + 1))

{-# ANN filter_AllIn_x1 (PermitPatternMatches []) #-}
{-# ANN filter_AllIn_x1 (PermitConstructions []) #-}
{-# ANN filter_AllIn_x1 (PermitTypeClasses []) #-}
{-# NOINLINE filter_AllIn_x1 #-}
filter_AllIn_x1 :: Int -> Int -> IO ()
filter_AllIn_x1 value = withStream value (filterAllIn value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_AllIn_x1
inspect $ 'filter_AllIn_x1 `hasNoType` ''Step
inspect $ 'filter_AllIn_x1 `hasNoType` ''FL.Step
inspect $ 'filter_AllIn_x1 `hasNoType` ''SPEC
#endif

{-# ANN filter_AllIn_x4 (PermitPatternMatches []) #-}
{-# ANN filter_AllIn_x4 (PermitConstructions []) #-}
{-# ANN filter_AllIn_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filter_AllIn_x4 #-}
filter_AllIn_x4 :: Int -> Int -> IO ()
filter_AllIn_x4 value = withStream value (filterAllIn value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_AllIn_x4
inspect $ 'filter_AllIn_x4 `hasNoType` ''Step
inspect $ 'filter_AllIn_x4 `hasNoType` ''FL.Step
inspect $ 'filter_AllIn_x4 `hasNoType` ''SPEC
#endif

{-# INLINE filterMEven #-}
filterMEven :: MonadIO m => Int -> Stream m Int -> m ()
filterMEven n = composeN n $ Stream.filterM (return . even)

{-# ANN filterM_Even_x1 (PermitPatternMatches []) #-}
{-# ANN filterM_Even_x1 (PermitConstructions []) #-}
{-# ANN filterM_Even_x1 (PermitTypeClasses []) #-}
{-# NOINLINE filterM_Even_x1 #-}
filterM_Even_x1 :: Int -> Int -> IO ()
filterM_Even_x1 value = withStream value (filterMEven 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterM_Even_x1
inspect $ 'filterM_Even_x1 `hasNoType` ''Step
inspect $ 'filterM_Even_x1 `hasNoType` ''FL.Step
inspect $ 'filterM_Even_x1 `hasNoType` ''SPEC
#endif

{-# ANN filterM_Even_x4 (PermitPatternMatches []) #-}
{-# ANN filterM_Even_x4 (PermitConstructions []) #-}
{-# ANN filterM_Even_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filterM_Even_x4 #-}
filterM_Even_x4 :: Int -> Int -> IO ()
filterM_Even_x4 value = withStream value (filterMEven 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterM_Even_x4
inspect $ 'filterM_Even_x4 `hasNoType` ''Step
inspect $ 'filterM_Even_x4 `hasNoType` ''FL.Step
inspect $ 'filterM_Even_x4 `hasNoType` ''SPEC
#endif

{-# INLINE filterMAllOut #-}
filterMAllOut :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterMAllOut value n =
    composeN n $ Stream.filterM (\x -> return $ x > (value + 1))

{-# ANN filterM_AllOut_x1 (PermitPatternMatches []) #-}
{-# ANN filterM_AllOut_x1 (PermitConstructions []) #-}
{-# ANN filterM_AllOut_x1 (PermitTypeClasses []) #-}
{-# NOINLINE filterM_AllOut_x1 #-}
filterM_AllOut_x1 :: Int -> Int -> IO ()
filterM_AllOut_x1 value = withStream value (filterMAllOut value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterM_AllOut_x1
inspect $ 'filterM_AllOut_x1 `hasNoType` ''Step
inspect $ 'filterM_AllOut_x1 `hasNoType` ''FL.Step
inspect $ 'filterM_AllOut_x1 `hasNoType` ''SPEC
#endif

{-# ANN filterM_AllOut_x4 (PermitPatternMatches []) #-}
{-# ANN filterM_AllOut_x4 (PermitConstructions []) #-}
{-# ANN filterM_AllOut_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filterM_AllOut_x4 #-}
filterM_AllOut_x4 :: Int -> Int -> IO ()
filterM_AllOut_x4 value = withStream value (filterMAllOut value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterM_AllOut_x4
inspect $ 'filterM_AllOut_x4 `hasNoType` ''Step
inspect $ 'filterM_AllOut_x4 `hasNoType` ''FL.Step
inspect $ 'filterM_AllOut_x4 `hasNoType` ''SPEC
#endif

{-# INLINE filterMAllIn #-}
filterMAllIn :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterMAllIn value n =
    composeN n $ Stream.filterM (\x -> return $ x <= (value + 1))

{-# ANN filterM_AllIn_x1 (PermitPatternMatches []) #-}
{-# ANN filterM_AllIn_x1 (PermitConstructions []) #-}
{-# ANN filterM_AllIn_x1 (PermitTypeClasses []) #-}
{-# NOINLINE filterM_AllIn_x1 #-}
filterM_AllIn_x1 :: Int -> Int -> IO ()
filterM_AllIn_x1 value = withStream value (filterMAllIn value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterM_AllIn_x1
inspect $ 'filterM_AllIn_x1 `hasNoType` ''Step
inspect $ 'filterM_AllIn_x1 `hasNoType` ''FL.Step
inspect $ 'filterM_AllIn_x1 `hasNoType` ''SPEC
#endif

{-# ANN filterM_AllIn_x4 (PermitPatternMatches []) #-}
{-# ANN filterM_AllIn_x4 (PermitConstructions []) #-}
{-# ANN filterM_AllIn_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filterM_AllIn_x4 #-}
filterM_AllIn_x4 :: Int -> Int -> IO ()
filterM_AllIn_x4 value = withStream value (filterMAllIn value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterM_AllIn_x4
inspect $ 'filterM_AllIn_x4 `hasNoType` ''Step
inspect $ 'filterM_AllIn_x4 `hasNoType` ''FL.Step
inspect $ 'filterM_AllIn_x4 `hasNoType` ''SPEC
#endif

{-# INLINE dropOne #-}
dropOne :: MonadIO m => Int -> Stream m Int -> m ()
dropOne n = composeN n $ Stream.drop 1

{-# ANN drop_One_x1 (PermitPatternMatches []) #-}
{-# ANN drop_One_x1 (PermitConstructions []) #-}
{-# ANN drop_One_x1 (PermitTypeClasses []) #-}
{-# NOINLINE drop_One_x1 #-}
drop_One_x1 :: Int -> Int -> IO ()
drop_One_x1 value = withStream value (dropOne 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'drop_One_x1
inspect $ 'drop_One_x1 `hasNoType` ''Step
inspect $ 'drop_One_x1 `hasNoType` ''FL.Step
inspect $ 'drop_One_x1 `hasNoType` ''SPEC
#endif

{-# ANN drop_One_x4 (PermitPatternMatches []) #-}
{-# ANN drop_One_x4 (PermitConstructions []) #-}
{-# ANN drop_One_x4 (PermitTypeClasses []) #-}
{-# NOINLINE drop_One_x4 #-}
drop_One_x4 :: Int -> Int -> IO ()
drop_One_x4 value = withStream value (dropOne 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'drop_One_x4
inspect $ 'drop_One_x4 `hasNoType` ''Step
inspect $ 'drop_One_x4 `hasNoType` ''FL.Step
inspect $ 'drop_One_x4 `hasNoType` ''SPEC
#endif

{-# INLINE dropAll #-}
dropAll :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropAll value n = composeN n $ Stream.drop (value + 1)

{-# ANN drop_All_x1 (PermitPatternMatches []) #-}
{-# ANN drop_All_x1 (PermitConstructions []) #-}
{-# ANN drop_All_x1 (PermitTypeClasses []) #-}
{-# NOINLINE drop_All_x1 #-}
drop_All_x1 :: Int -> Int -> IO ()
drop_All_x1 value = withStream value (dropAll value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'drop_All_x1
inspect $ 'drop_All_x1 `hasNoType` ''Step
inspect $ 'drop_All_x1 `hasNoType` ''FL.Step
inspect $ 'drop_All_x1 `hasNoType` ''SPEC
#endif

{-# ANN drop_All_x4 (PermitPatternMatches []) #-}
{-# ANN drop_All_x4 (PermitConstructions []) #-}
{-# ANN drop_All_x4 (PermitTypeClasses []) #-}
{-# NOINLINE drop_All_x4 #-}
drop_All_x4 :: Int -> Int -> IO ()
drop_All_x4 value = withStream value (dropAll value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'drop_All_x4
inspect $ 'drop_All_x4 `hasNoType` ''Step
inspect $ 'drop_All_x4 `hasNoType` ''FL.Step
inspect $ 'drop_All_x4 `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileTrue #-}
dropWhileTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropWhileTrue value n = composeN n $ Stream.dropWhile (<= (value + 1))

{-# ANN dropWhile_True_x1 (PermitPatternMatches []) #-}
{-# ANN dropWhile_True_x1 (PermitConstructions []) #-}
{-# ANN dropWhile_True_x1 (PermitTypeClasses []) #-}
{-# NOINLINE dropWhile_True_x1 #-}
dropWhile_True_x1 :: Int -> Int -> IO ()
dropWhile_True_x1 value = withStream value (dropWhileTrue value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropWhile_True_x1
inspect $ 'dropWhile_True_x1 `hasNoType` ''Step
inspect $ 'dropWhile_True_x1 `hasNoType` ''Stream.DropWhileState
inspect $ 'dropWhile_True_x1 `hasNoType` ''FL.Step
inspect $ 'dropWhile_True_x1 `hasNoType` ''SPEC
#endif

{-# ANN dropWhile_True_x4 (PermitPatternMatches []) #-}
{-# ANN dropWhile_True_x4 (PermitConstructions []) #-}
{-# ANN dropWhile_True_x4 (PermitTypeClasses []) #-}
{-# NOINLINE dropWhile_True_x4 #-}
dropWhile_True_x4 :: Int -> Int -> IO ()
dropWhile_True_x4 value = withStream value (dropWhileTrue value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropWhile_True_x4
inspect $ 'dropWhile_True_x4 `hasNoType` ''Step
inspect $ 'dropWhile_True_x4 `hasNoType` ''Stream.DropWhileState
inspect $ 'dropWhile_True_x4 `hasNoType` ''FL.Step
inspect $ 'dropWhile_True_x4 `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileMTrue #-}
dropWhileMTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropWhileMTrue value n =
    composeN n $ Stream.dropWhileM (return . (<= (value + 1)))

{-# ANN dropWhileM_True_x4 (PermitPatternMatches []) #-}
{-# ANN dropWhileM_True_x4 (PermitConstructions []) #-}
{-# ANN dropWhileM_True_x4 (PermitTypeClasses []) #-}
{-# NOINLINE dropWhileM_True_x4 #-}
dropWhileM_True_x4 :: Int -> Int -> IO ()
dropWhileM_True_x4 value = withStream value (dropWhileMTrue value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropWhileM_True_x4
inspect $ 'dropWhileM_True_x4 `hasNoType` ''Step
inspect $ 'dropWhileM_True_x4 `hasNoType` ''Stream.DropWhileState
inspect $ 'dropWhileM_True_x4 `hasNoType` ''FL.Step
inspect $ 'dropWhileM_True_x4 `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileFalse #-}
dropWhileFalse :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropWhileFalse value n = composeN n $ Stream.dropWhile (> (value + 1))

{-# ANN dropWhile_False_x1 (PermitPatternMatches []) #-}
{-# ANN dropWhile_False_x1 (PermitConstructions []) #-}
{-# ANN dropWhile_False_x1 (PermitTypeClasses []) #-}
{-# NOINLINE dropWhile_False_x1 #-}
dropWhile_False_x1 :: Int -> Int -> IO ()
dropWhile_False_x1 value = withStream value (dropWhileFalse value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropWhile_False_x1
inspect $ 'dropWhile_False_x1 `hasNoType` ''Step
inspect $ 'dropWhile_False_x1 `hasNoType` ''Stream.DropWhileState
inspect $ 'dropWhile_False_x1 `hasNoType` ''FL.Step
inspect $ 'dropWhile_False_x1 `hasNoType` ''SPEC
#endif

{-# ANN dropWhile_False_x4 (PermitPatternMatches []) #-}
{-# ANN dropWhile_False_x4 (PermitConstructions []) #-}
{-# ANN dropWhile_False_x4 (PermitTypeClasses []) #-}
{-# NOINLINE dropWhile_False_x4 #-}
dropWhile_False_x4 :: Int -> Int -> IO ()
dropWhile_False_x4 value = withStream value (dropWhileFalse value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropWhile_False_x4
inspect $ 'dropWhile_False_x4 `hasNoType` ''Step
inspect $ 'dropWhile_False_x4 `hasNoType` ''Stream.DropWhileState
inspect $ 'dropWhile_False_x4 `hasNoType` ''FL.Step
inspect $ 'dropWhile_False_x4 `hasNoType` ''SPEC
#endif

{-# INLINE findIndices #-}
findIndices :: MonadIO m => Int -> Int -> Stream m Int -> m ()
findIndices value n = composeN n $ Stream.findIndices (== (value + 1))

{-# ANN findIndices_x1 (PermitPatternMatches []) #-}
{-# ANN findIndices_x1 (PermitConstructions []) #-}
{-# ANN findIndices_x1 (PermitTypeClasses []) #-}
{-# NOINLINE findIndices_x1 #-}
findIndices_x1 :: Int -> Int -> IO ()
findIndices_x1 value = withStream value (findIndices value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'findIndices_x1
inspect $ 'findIndices_x1 `hasNoType` ''Step
inspect $ 'findIndices_x1 `hasNoType` ''FL.Step
inspect $ 'findIndices_x1 `hasNoType` ''SPEC
#endif

{-# ANN findIndices_x4 (PermitPatternMatches []) #-}
{-# ANN findIndices_x4 (PermitConstructions []) #-}
{-# ANN findIndices_x4 (PermitTypeClasses []) #-}
{-# NOINLINE findIndices_x4 #-}
findIndices_x4 :: Int -> Int -> IO ()
findIndices_x4 value = withStream value (findIndices value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'findIndices_x4
inspect $ 'findIndices_x4 `hasNoType` ''Step
inspect $ 'findIndices_x4 `hasNoType` ''FL.Step
inspect $ 'findIndices_x4 `hasNoType` ''SPEC
#endif

{-# INLINE elemIndices #-}
elemIndices :: MonadIO m => Int -> Int -> Stream m Int -> m ()
elemIndices value n = composeN n $ Stream.elemIndices (value + 1)

{-# ANN elemIndices_x1 (PermitPatternMatches []) #-}
{-# ANN elemIndices_x1 (PermitConstructions []) #-}
{-# ANN elemIndices_x1 (PermitTypeClasses []) #-}
{-# NOINLINE elemIndices_x1 #-}
elemIndices_x1 :: Int -> Int -> IO ()
elemIndices_x1 value = withStream value (elemIndices value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'elemIndices_x1
inspect $ 'elemIndices_x1 `hasNoType` ''Step
inspect $ 'elemIndices_x1 `hasNoType` ''FL.Step
inspect $ 'elemIndices_x1 `hasNoType` ''SPEC
#endif

{-# ANN elemIndices_x4 (PermitPatternMatches []) #-}
{-# ANN elemIndices_x4 (PermitConstructions []) #-}
{-# ANN elemIndices_x4 (PermitTypeClasses []) #-}
{-# NOINLINE elemIndices_x4 #-}
elemIndices_x4 :: Int -> Int -> IO ()
elemIndices_x4 value = withStream value (elemIndices value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'elemIndices_x4
inspect $ 'elemIndices_x4 `hasNoType` ''Step
inspect $ 'elemIndices_x4 `hasNoType` ''FL.Step
inspect $ 'elemIndices_x4 `hasNoType` ''SPEC
#endif

{-# ANN findIndices_SingleIndex (PermitPatternMatches []) #-}
{-# ANN findIndices_SingleIndex (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN findIndices_SingleIndex (PermitTypeClasses []) #-}
{-# NOINLINE findIndices_SingleIndex #-}
findIndices_SingleIndex :: Int -> Int -> IO (Maybe Int)
findIndices_SingleIndex value =
    withStream value (Stream.head . Stream.findIndices (== (value + 1)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'findIndices_SingleIndex
inspect $ 'findIndices_SingleIndex `hasNoType` ''Step
inspect $ 'findIndices_SingleIndex `hasNoType` ''FL.Step
inspect $ 'findIndices_SingleIndex `hasNoType` ''SPEC
#endif

{-# ANN elemIndices_SingleIndex (PermitPatternMatches []) #-}
{-# ANN elemIndices_SingleIndex (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN elemIndices_SingleIndex (PermitTypeClasses []) #-}
{-# NOINLINE elemIndices_SingleIndex #-}
elemIndices_SingleIndex :: Int -> Int -> IO (Maybe Int)
elemIndices_SingleIndex value =
    withStream value (Stream.head . Stream.elemIndices (value + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'elemIndices_SingleIndex
inspect $ 'elemIndices_SingleIndex `hasNoType` ''Step
inspect $ 'elemIndices_SingleIndex `hasNoType` ''FL.Step
inspect $ 'elemIndices_SingleIndex `hasNoType` ''SPEC
#endif

{-# INLINE deleteBy #-}
deleteBy :: MonadIO m => Int -> Int -> Stream m Int -> m ()
deleteBy value n = composeN n $ Stream.deleteBy (>=) (value + 1)

{-# ANN deleteBy_x1 (PermitPatternMatches []) #-}
{-# ANN deleteBy_x1 (PermitConstructions []) #-}
{-# ANN deleteBy_x1 (PermitTypeClasses []) #-}
{-# NOINLINE deleteBy_x1 #-}
deleteBy_x1 :: Int -> Int -> IO ()
deleteBy_x1 value = withStream value (deleteBy value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'deleteBy_x1
inspect $ 'deleteBy_x1 `hasNoType` ''Step
inspect $ 'deleteBy_x1 `hasNoType` ''FL.Step
inspect $ 'deleteBy_x1 `hasNoType` ''SPEC
#endif

{-# ANN deleteBy_x4 (PermitPatternMatches []) #-}
{-# ANN deleteBy_x4 (PermitConstructions []) #-}
{-# ANN deleteBy_x4 (PermitTypeClasses []) #-}
{-# NOINLINE deleteBy_x4 #-}
deleteBy_x4 :: Int -> Int -> IO ()
deleteBy_x4 value = withStream value (deleteBy value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'deleteBy_x4
inspect $ 'deleteBy_x4 `hasNoType` ''Step
inspect $ 'deleteBy_x4 `hasNoType` ''FL.Step
inspect $ 'deleteBy_x4 `hasNoType` ''SPEC
#endif

-- uniq . uniq == uniq, composeN 2 ~ composeN 1
{-# INLINE uniq #-}
uniq :: MonadIO m => Int -> Stream m Int -> m ()
uniq n = composeN n Stream.uniq

{-# ANN uniq_x1 (PermitPatternMatches []) #-}
{-# ANN uniq_x1 (PermitConstructions []) #-}
{-# ANN uniq_x1 (PermitTypeClasses []) #-}
{-# NOINLINE uniq_x1 #-}
uniq_x1 :: Int -> Int -> IO ()
uniq_x1 value = withStream value (uniq 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'uniq_x1
inspect $ 'uniq_x1 `hasNoType` ''Step
inspect $ 'uniq_x1 `hasNoType` ''FL.Step
inspect $ 'uniq_x1 `hasNoType` ''SPEC
#endif

{-# ANN uniq_x4 (PermitPatternMatches []) #-}
{-# ANN uniq_x4 (PermitConstructions []) #-}
{-# ANN uniq_x4 (PermitTypeClasses []) #-}
{-# NOINLINE uniq_x4 #-}
uniq_x4 :: Int -> Int -> IO ()
uniq_x4 value = withStream value (uniq 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'uniq_x4
inspect $ 'uniq_x4 `hasNoType` ''Step
inspect $ 'uniq_x4 `hasNoType` ''FL.Step
inspect $ 'uniq_x4 `hasNoType` ''SPEC
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

{-# ANN mapMaybe_x1 (PermitPatternMatches []) #-}
{-# ANN mapMaybe_x1 (PermitConstructions []) #-}
{-# ANN mapMaybe_x1 (PermitTypeClasses []) #-}
{-# NOINLINE mapMaybe_x1 #-}
mapMaybe_x1 :: Int -> Int -> IO ()
mapMaybe_x1 value = withStream value (mapMaybe 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapMaybe_x1
inspect $ 'mapMaybe_x1 `hasNoType` ''Step
inspect $ 'mapMaybe_x1 `hasNoType` ''FL.Step
inspect $ 'mapMaybe_x1 `hasNoType` ''SPEC
#endif

{-# ANN mapMaybe_x4 (PermitPatternMatches []) #-}
{-# ANN mapMaybe_x4 (PermitConstructions []) #-}
{-# ANN mapMaybe_x4 (PermitTypeClasses []) #-}
{-# NOINLINE mapMaybe_x4 #-}
mapMaybe_x4 :: Int -> Int -> IO ()
mapMaybe_x4 value = withStream value (mapMaybe 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapMaybe_x4
inspect $ 'mapMaybe_x4 `hasNoType` ''Step
inspect $ 'mapMaybe_x4 `hasNoType` ''FL.Step
inspect $ 'mapMaybe_x4 `hasNoType` ''SPEC
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

{-# ANN mapMaybeM_x1 (PermitPatternMatches []) #-}
{-# ANN mapMaybeM_x1 (PermitConstructions []) #-}
{-# ANN mapMaybeM_x1 (PermitTypeClasses []) #-}
{-# NOINLINE mapMaybeM_x1 #-}
mapMaybeM_x1 :: Int -> Int -> IO ()
mapMaybeM_x1 value = withStream value (mapMaybeM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapMaybeM_x1
inspect $ 'mapMaybeM_x1 `hasNoType` ''Step
inspect $ 'mapMaybeM_x1 `hasNoType` ''FL.Step
inspect $ 'mapMaybeM_x1 `hasNoType` ''SPEC
#endif

{-# ANN mapMaybeM_x4 (PermitPatternMatches []) #-}
{-# ANN mapMaybeM_x4 (PermitConstructions []) #-}
{-# ANN mapMaybeM_x4 (PermitTypeClasses []) #-}
{-# NOINLINE mapMaybeM_x4 #-}
mapMaybeM_x4 :: Int -> Int -> IO ()
mapMaybeM_x4 value = withStream value (mapMaybeM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapMaybeM_x4
inspect $ 'mapMaybeM_x4 `hasNoType` ''Step
inspect $ 'mapMaybeM_x4 `hasNoType` ''FL.Step
inspect $ 'mapMaybeM_x4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Size increasing transformations (insertions)
-------------------------------------------------------------------------------

{-# INLINE intersperse #-}
intersperse :: MonadAsync m => Int -> Int -> Stream m Int -> m ()
intersperse value n = composeN n $ Stream.intersperse (value + 1)

{-# ANN intersperse_x1 (PermitPatternMatches []) #-}
{-# ANN intersperse_x1 (PermitConstructions []) #-}
{-# ANN intersperse_x1 (PermitTypeClasses []) #-}
{-# NOINLINE intersperse_x1 #-}
intersperse_x1 :: Int -> Int -> IO ()
intersperse_x1 value = withStream value (intersperse value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'intersperse_x1
inspect $ 'intersperse_x1 `hasNoType` ''Step
inspect $ 'intersperse_x1 `hasNoType` ''LoopState
inspect $ 'intersperse_x1 `hasNoType` ''FL.Step
inspect $ 'intersperse_x1 `hasNoType` ''SPEC
#endif

{-# ANN intersperse_x4 (PermitPatternMatches
    [''Int,''LoopState,''SPEC]) #-}
{-# ANN intersperse_x4 (PermitConstructions
    [''Int,''LoopState,''SPEC]) #-}
{-# ANN intersperse_x4 (PermitTypeClasses []) #-}
{-# NOINLINE intersperse_x4 #-}
intersperse_x4 :: Int -> Int -> IO ()
intersperse_x4 value = withStream value (intersperse value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'intersperse_x4
inspect $ 'intersperse_x4 `hasNoType` ''Step
-- inspect $ 'intersperse_x4 `hasNoType` ''LoopState
inspect $ 'intersperse_x4 `hasNoType` ''FL.Step
-- inspect $ 'intersperse_x4 `hasNoType` ''SPEC
#endif

{-# INLINE intersperseM #-}
intersperseM :: MonadAsync m => Int -> Int -> Stream m Int -> m ()
intersperseM value n = composeN n $ Stream.intersperseM (return $ value + 1)

{-# ANN intersperseM_x1 (PermitPatternMatches []) #-}
{-# ANN intersperseM_x1 (PermitConstructions []) #-}
{-# ANN intersperseM_x1 (PermitTypeClasses []) #-}
{-# NOINLINE intersperseM_x1 #-}
intersperseM_x1 :: Int -> Int -> IO ()
intersperseM_x1 value = withStream value (intersperseM value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'intersperseM_x1
inspect $ 'intersperseM_x1 `hasNoType` ''Step
inspect $ 'intersperseM_x1 `hasNoType` ''LoopState
inspect $ 'intersperseM_x1 `hasNoType` ''FL.Step
inspect $ 'intersperseM_x1 `hasNoType` ''SPEC
#endif

{-# INLINE insertBy #-}
insertBy :: MonadIO m => Int -> Int -> Stream m Int -> m ()
insertBy value n = composeN n $ Stream.insertBy compare (value + 1)

{-# ANN insertBy_x1 (PermitPatternMatches []) #-}
{-# ANN insertBy_x1 (PermitConstructions []) #-}
{-# ANN insertBy_x1 (PermitTypeClasses []) #-}
{-# NOINLINE insertBy_x1 #-}
insertBy_x1 :: Int -> Int -> IO ()
insertBy_x1 value = withStream value (insertBy value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'insertBy_x1
inspect $ 'insertBy_x1 `hasNoType` ''Step
inspect $ 'insertBy_x1 `hasNoType` ''FL.Step
inspect $ 'insertBy_x1 `hasNoType` ''SPEC
#endif

{-# ANN insertBy_x4 (PermitPatternMatches [''Int]) #-}
{-# ANN insertBy_x4 (PermitConstructions [''Int]) #-}
{-# ANN insertBy_x4 (PermitTypeClasses []) #-}
{-# NOINLINE insertBy_x4 #-}
insertBy_x4 :: Int -> Int -> IO ()
insertBy_x4 value = withStream value (insertBy value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'insertBy_x4
inspect $ 'insertBy_x4 `hasNoType` ''Step
inspect $ 'insertBy_x4 `hasNoType` ''FL.Step
inspect $ 'insertBy_x4 `hasNoType` ''SPEC
#endif

{-# INLINE unfoldEachSepBy #-}
unfoldEachSepBy :: Monad m => Int -> Int -> Stream m Int -> m ()
unfoldEachSepBy value n =
    composeN n $ Stream.unfoldEachSepBy (value + 1) Unfold.identity

{-# ANN unfoldEachSepBy_x1 (PermitPatternMatches []) #-}
{-# ANN unfoldEachSepBy_x1 (PermitConstructions []) #-}
{-# ANN unfoldEachSepBy_x1 (PermitTypeClasses []) #-}
{-# NOINLINE unfoldEachSepBy_x1 #-}
unfoldEachSepBy_x1 :: Int -> Int -> IO ()
unfoldEachSepBy_x1 value = withStream value (unfoldEachSepBy value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldEachSepBy_x1
inspect $ 'unfoldEachSepBy_x1 `hasNoType` ''Step
inspect $ 'unfoldEachSepBy_x1 `hasNoType` ''Stream.InterposeState
inspect $ 'unfoldEachSepBy_x1 `hasNoType` ''FL.Step
inspect $ 'unfoldEachSepBy_x1 `hasNoType` ''SPEC
#endif

{-# INLINE unfoldEachSepBySeq #-}
unfoldEachSepBySeq :: Monad m => Int -> Int -> Stream m Int -> m ()
unfoldEachSepBySeq value n =
    composeN n $ Stream.unfoldEachSepBySeq (value + 1) Unfold.identity

{-# ANN unfoldEachSepBySeq_x1 (PermitPatternMatches []) #-}
{-# ANN unfoldEachSepBySeq_x1 (PermitConstructions []) #-}
{-# ANN unfoldEachSepBySeq_x1 (PermitTypeClasses []) #-}
{-# NOINLINE unfoldEachSepBySeq_x1 #-}
unfoldEachSepBySeq_x1 :: Int -> Int -> IO ()
unfoldEachSepBySeq_x1 value = withStream value (unfoldEachSepBySeq value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldEachSepBySeq_x1
inspect $ 'unfoldEachSepBySeq_x1 `hasNoType` ''Step
inspect $ 'unfoldEachSepBySeq_x1 `hasNoType` ''LoopState
inspect $ 'unfoldEachSepBySeq_x1 `hasNoType` ''Producer.ConcatState
inspect $ 'unfoldEachSepBySeq_x1 `hasNoType` ''FL.Step
inspect $ 'unfoldEachSepBySeq_x1 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Indexing
-------------------------------------------------------------------------------

{-# INLINE indexed #-}
indexed :: MonadIO m => Int -> Stream m Int -> m ()
indexed n = composeN n (fmap snd . Stream.indexed)

{-# ANN indexed_x1 (PermitPatternMatches []) #-}
{-# ANN indexed_x1 (PermitConstructions []) #-}
{-# ANN indexed_x1 (PermitTypeClasses []) #-}
{-# NOINLINE indexed_x1 #-}
indexed_x1 :: Int -> Int -> IO ()
indexed_x1 value = withStream value (indexed 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'indexed_x1
inspect $ 'indexed_x1 `hasNoType` ''Step
inspect $ 'indexed_x1 `hasNoType` ''FL.Step
inspect $ 'indexed_x1 `hasNoType` ''SPEC
#endif

{-# ANN indexed_x4 (PermitPatternMatches []) #-}
{-# ANN indexed_x4 (PermitConstructions []) #-}
{-# ANN indexed_x4 (PermitTypeClasses []) #-}
{-# NOINLINE indexed_x4 #-}
indexed_x4 :: Int -> Int -> IO ()
indexed_x4 value = withStream value (indexed 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'indexed_x4
inspect $ 'indexed_x4 `hasNoType` ''Step
inspect $ 'indexed_x4 `hasNoType` ''FL.Step
inspect $ 'indexed_x4 `hasNoType` ''SPEC
#endif

{-# INLINE indexedR #-}
indexedR :: MonadIO m => Int -> Int -> Stream m Int -> m ()
indexedR value n = composeN n (fmap snd . Stream.indexedR value)

{-# ANN indexedR_x1 (PermitPatternMatches []) #-}
{-# ANN indexedR_x1 (PermitConstructions []) #-}
{-# ANN indexedR_x1 (PermitTypeClasses []) #-}
{-# NOINLINE indexedR_x1 #-}
indexedR_x1 :: Int -> Int -> IO ()
indexedR_x1 value = withStream value (indexedR value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'indexedR_x1
inspect $ 'indexedR_x1 `hasNoType` ''Step
inspect $ 'indexedR_x1 `hasNoType` ''FL.Step
inspect $ 'indexedR_x1 `hasNoType` ''SPEC
#endif

{-# ANN indexedR_x4 (PermitPatternMatches []) #-}
{-# ANN indexedR_x4 (PermitConstructions []) #-}
{-# ANN indexedR_x4 (PermitTypeClasses []) #-}
{-# NOINLINE indexedR_x4 #-}
indexedR_x4 :: Int -> Int -> IO ()
indexedR_x4 value = withStream value (indexedR value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'indexedR_x4
inspect $ 'indexedR_x4 `hasNoType` ''Step
inspect $ 'indexedR_x4 `hasNoType` ''FL.Step
inspect $ 'indexedR_x4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Size conserving transformations (reordering, buffering, etc.)
-------------------------------------------------------------------------------

{-# ANN reverse (PermitPatternMatches [''[],''Int,''SPEC]) #-}
{-# ANN reverse (PermitConstructions [''[],''Int,''SPEC]) #-}
{-# ANN reverse (PermitTypeClasses []) #-}
{-# NOINLINE reverse #-}
reverse :: Int -> Int -> IO ()
reverse value = withStream value (composeN 1 Stream.reverse)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'reverse
inspect $ 'reverse `hasNoType` ''Step
inspect $ 'reverse `hasNoType` ''FL.Step
-- inspect $ 'reverse `hasNoType` ''SPEC
#endif

{-# ANN reverseUnbox (PermitPatternMatches
    [''Int,''Array,''GroupState,''MutByteArray,''State
    ,''Step]) #-}
{-# ANN reverseUnbox (PermitConstructions
    [''Array,''Maybe,''State,''GroupState,''Int
    ,''MutByteArray,''Step,''Bool]) #-}
{-# ANN reverseUnbox (PermitTypeClasses []) #-}
{-# NOINLINE reverseUnbox #-}
reverseUnbox :: Int -> Int -> IO ()
reverseUnbox value = withStream value (composeN 1 Stream.reverseUnbox)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'reverseUnbox
-- inspect $ 'reverseUnbox `hasNoType` ''Step
#endif

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- Benchmark naming: name each benchmark (and its IO action) after the exported
-- function it benchmarks, using combinator_dimension1_dimension2..., where the
-- dimensions are optional variants/type specializations (used esp. when more
-- than one specialization is benchmarked). Keep extra info in parenthetical
-- notes in the description; these also disambiguate benchmarks that reuse a
-- single IO action with different arguments. If the name has a trailing
-- underscore, add one more underscore.
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    [
    -- , benchIOSink value "foldrT" (foldrT 1)
    -- , benchIOSink value "foldrTMap" (foldrTMap 1)

    -- Mapping
      (SpaceO_1, benchIO "sequence_x1" $ sequence_x1 size)
    , (SpaceO_1, benchIO "tap_x1" $ tap_x1 size)
    -- XXX tasty-bench hangs benchmarking this
    -- , benchIOSink value "timestamped" _timestamped
    -- Scanning
    , (SpaceO_1, benchIO "scanl'_x1" $ scanl'_x1 size)
    , (SpaceO_1, benchIO "scanl1'_x1" $ scanl1'_x1 size)
    , (SpaceO_1, benchIO "scanlM'_x1" $ scanlM'_x1 size)
    , (SpaceO_1, benchIO "scanl1M'_x1" $ scanl1M'_x1 size)
    , (SpaceO_1, benchIO "postscanl'_x1" $ postscanl'_x1 size)
    , (SpaceO_1, benchIO "postscanlM'_x1" $ postscanlM'_x1 size)
    , (SpaceO_1, benchIO "scanl_x1" $ scanl_x1 size)
    , (SpaceO_1, benchIO "postscanl_x1" $ postscanl_x1 size)
    , (SpaceO_1, benchIO "trace_x4" $ trace_x4 size)

    , (SpaceO_1, benchIO "scanl'_x4" $ scanl'_x4 size)
    , (SpaceO_1, benchIO "scanl1'_x4" $ scanl1'_x4 size)
    , (SpaceO_1, benchIO "scanlM'_x4" $ scanlM'_x4 size)
    , (SpaceO_1, benchIO "scanl1M'_x4" $ scanl1M'_x4 size)
    , (SpaceO_1, benchIO "postscanl'_x4" $ postscanl'_x4 size)
    , (SpaceO_1, benchIO "postscanlM'_x4" $ postscanlM'_x4 size)
    , (SpaceO_1, benchIO "scanl_x4" $ scanl_x4 size)
    , (SpaceO_1, benchIO "postscanl_x4" $ postscanl_x4 size)
    , (SpaceO_1, benchIO "filter_Even_x1" $ filter_Even_x1 size)
    , (SpaceO_1, benchIO "filter_AllOut_x1" $ filter_AllOut_x1 size)
    , (SpaceO_1, benchIO "filter_AllIn_x1" $ filter_AllIn_x1 size)

    , (SpaceO_1, benchIO "filterM_Even_x1" $ filterM_Even_x1 size)
    , (SpaceO_1, benchIO "filterM_AllOut_x1" $ filterM_AllOut_x1 size)
    , (SpaceO_1, benchIO "filterM_AllIn_x1" $ filterM_AllIn_x1 size)

    , (SpaceO_1, benchIO "drop_One_x1" $ drop_One_x1 size)
    , (SpaceO_1, benchIO "drop_All_x1" $ drop_All_x1 size)
    , (SpaceO_1, benchIO "dropWhile_True_x1" $ dropWhile_True_x1 size)
 -- , (SpaceO_1, benchIO "dropWhileM_True_x1" ...)
    , (SpaceO_1, benchIO "dropWhile_False_x1" $ dropWhile_False_x1 size)
    , (SpaceO_1, benchIO "deleteBy_x1" $ deleteBy_x1 size)

    , (SpaceO_1, benchIO "uniq_x1" $ uniq_x1 size)

    -- Map and filter
    , (SpaceO_1, benchIO "mapMaybe_x1" $ mapMaybe_x1 size)
    , (SpaceO_1, benchIO "mapMaybeM_x1" $ mapMaybeM_x1 size)

    -- Searching (stateful map and filter)
    , (SpaceO_1, benchIO "findIndices_x1" $ findIndices_x1 size)
    , (SpaceO_1, benchIO "elemIndices_x1" $ elemIndices_x1 size)
    , (SpaceO_1, benchIO "findIndices_SingleIndex" $
          findIndices_SingleIndex size)
    , (SpaceO_1, benchIO "elemIndices_SingleIndex" $
          elemIndices_SingleIndex size)
    , (SpaceO_1, benchIO "filter_Even_x4" $ filter_Even_x4 size)
    , (SpaceO_1, benchIO "filter_AllOut_x4" $ filter_AllOut_x4 size)
    , (SpaceO_1, benchIO "filter_AllIn_x4" $ filter_AllIn_x4 size)

    , (SpaceO_1, benchIO "filterM_Even_x4" $ filterM_Even_x4 size)
    , (SpaceO_1, benchIO "filterM_AllOut_x4" $ filterM_AllOut_x4 size)
    , (SpaceO_1, benchIO "filterM_AllIn_x4" $ filterM_AllIn_x4 size)

    , (SpaceO_1, benchIO "drop_One_x4" $ drop_One_x4 size)
    , (SpaceO_1, benchIO "drop_All_x4" $ drop_All_x4 size)
    , (SpaceO_1, benchIO "dropWhile_True_x4" $ dropWhile_True_x4 size)
    , (SpaceO_1, benchIO "dropWhileM_True_x4" $ dropWhileM_True_x4 size)
    -- XXX requires @-fspec-constr-recursive=12@.
    , (SpaceO_1, benchIO "dropWhile_False_x4" $ dropWhile_False_x4 size)
    , (SpaceO_1, benchIO "deleteBy_x4" $ deleteBy_x4 size)

    , (SpaceO_1, benchIO "uniq_x4" $ uniq_x4 size)

    -- map and filter
    , (SpaceO_1, benchIO "mapMaybe_x4" $ mapMaybe_x4 size)
    , (SpaceO_1, benchIO "mapMaybeM_x4" $ mapMaybeM_x4 size)

    -- searching
    , (SpaceO_1, benchIO "findIndices_x4" $ findIndices_x4 size)
    , (SpaceO_1, benchIO "elemIndices_x4" $ elemIndices_x4 size)
    , (SpaceO_1, benchIO "intersperse_x1" $ intersperse_x1 size)
    , (SpaceO_1, benchIO "intersperseM_x1" $ intersperseM_x1 size)
    , (SpaceO_1, benchIO "insertBy_x1" $ insertBy_x1 size)
    , (SpaceO_1, benchIO "unfoldEachSepBy_x1" $ unfoldEachSepBy_x1 size)
    , (SpaceO_1, benchIO "unfoldEachSepBySeq_x1" $ unfoldEachSepBySeq_x1 size)
    -- XXX requires @-fspec-constr-recursive=16@.
    , (SpaceO_1, benchIO "intersperse_x4" $ intersperse_x4 size)
    , (SpaceO_1, benchIO "insertBy_x4" $ insertBy_x4 size)
    , (SpaceO_1, benchIO "indexed_x1" $ indexed_x1 size)
    , (SpaceO_1, benchIO "indexedR_x1" $ indexedR_x1 size)
    , (SpaceO_1, benchIO "indexed_x4" $ indexed_x4 size)
    , (SpaceO_1, benchIO "indexedR_x4" $ indexedR_x4 size)
    -- Reversing a stream
    , (HeapO_n, benchIO "reverse" $ reverse size)
    , (HeapO_n, benchIO "reverseUnbox" $ reverseUnbox size)
    ]
