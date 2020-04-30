-- |
-- Module      : Streamly.Benchmark.Prelude
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Streamly.Benchmark.Prelude.Transformation
    ( o_1_space_serial_pipes
    , o_1_space_serial_pipesX4
    , o_1_space_serial_transformer
    , o_1_space_serial_transformation
    , o_1_space_serial_transformationX4
    , o_1_space_serial_filtering
    , o_1_space_serial_filteringX4
    , o_1_space_serial_mixed
    , o_1_space_serial_mixedX4

    , o_1_space_wSerial_transformation

    , o_1_space_zipSerial_transformation

    , o_n_space_serial

    , o_n_stack_serial_iterated

    , o_1_space_async_transformation

    , o_1_space_wAsync_transformation

    , o_1_space_ahead_transformation

    , o_n_space_parallel_transformation
    ) where

import Control.DeepSeq (NFData(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Strict (StateT, get, put)
import Data.Functor.Identity (Identity)
import Data.IORef (newIORef, modifyIORef')
import GHC.Generics (Generic)
import System.Random (randomRIO)
import Prelude
       (Monad, String, Int, (+), ($), (.), return, even, (>), (<=), (==), (>=),
        undefined, Maybe(..), not, (>>=),
        maxBound, div, IO, compare, subtract, const, Bool(..), Either(..))
import qualified Prelude as P

#ifdef INSPECTION
import Test.Inspection

#endif

import qualified Streamly as S hiding (runStream)
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Prelude as Internal
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Internal.Data.Stream.Parallel as Par
import Streamly.Internal.Data.Time.Units

import Gauge
import Streamly hiding (runStream)

type Stream m a = S.SerialT m a

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

-- unfoldr

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceUnfoldr value n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrM value n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE source #-}
source :: (S.MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
source = sourceUnfoldrM

{-# INLINE sourceUnfoldrMN #-}
sourceUnfoldrMN :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrMN upto start = S.unfoldrM step start
    where
    step cnt =
        if cnt > start + upto
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldrMAction #-}
sourceUnfoldrMAction :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m (m Int)
sourceUnfoldrMAction value n = S.serially $ S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (return cnt, cnt + 1))

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.drain

{-# INLINE toNull #-}
toNull :: Monad m => (t m a -> S.SerialT m a) -> t m a -> m ()
toNull t = runStream . t


{-# INLINE tail #-}
tail :: Monad m => Stream m a -> m ()
tail s = S.tail s >>= P.mapM_ tail


{-# INLINE nullHeadTail #-}
nullHeadTail :: Monad m => Stream m Int -> m ()
nullHeadTail s = do
    r <- S.null s
    when (not r) $ do
        _ <- S.head s
        S.tail s >>= P.mapM_ nullHeadTail

{-# INLINE foldl'ReduceMap #-}
foldl'ReduceMap :: Monad m => Stream m Int -> m Int
foldl'ReduceMap = P.fmap (+ 1) . S.foldl' (+) 0

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Stream m a -> m ()
transform = runStream

{-# INLINE composeN #-}
composeN ::
       MonadIO m
    => Int
    -> (Stream m Int -> Stream m Int)
    -> Stream m Int
    -> m ()
composeN n f =
    case n of
        1 -> transform . f
        2 -> transform . f . f
        3 -> transform . f . f . f
        4 -> transform . f . f . f . f
        _ -> undefined

-- polymorphic stream version of composeN
{-# INLINE composeN' #-}
composeN' ::
       (S.IsStream t, Monad m)
    => Int
    -> (t m Int -> Stream m Int)
    -> t m Int
    -> m ()
composeN' n f =
    case n of
        1 -> transform . f
        2 -> transform . f . S.adapt . f
        3 -> transform . f . S.adapt . f . S.adapt . f
        4 -> transform . f . S.adapt . f . S.adapt . f . S.adapt . f
        _ -> undefined

{-# INLINE scan #-}
scan :: MonadIO m => Int -> Stream m Int -> m ()
scan n = composeN n $ S.scanl' (+) 0

{-# INLINE scanl1' #-}
scanl1' :: MonadIO m => Int -> Stream m Int -> m ()
scanl1' n = composeN n $ S.scanl1' (+)

{-# INLINE fmap #-}
fmap :: MonadIO m => Int -> Stream m Int -> m ()
fmap n = composeN n $ P.fmap (+ 1)

{-# INLINE fmap' #-}
fmap' ::
       (S.IsStream t, S.MonadAsync m, P.Functor (t m))
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
fmap' t n = composeN' n $ t . P.fmap (+ 1)

{-# INLINE map #-}
map :: MonadIO m => Int -> Stream m Int -> m ()
map n = composeN n $ S.map (+ 1)

{-# INLINE map' #-}
map' ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
map' t n = composeN' n $ t . S.map (+ 1)

{-# INLINE mapM #-}
mapM ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
mapM t n = composeN' n $ t . S.mapM return

{-# INLINE tap #-}
tap :: MonadIO m => Int -> Stream m Int -> m ()
tap n = composeN n $ S.tap FL.sum

{-# INLINE tapRate #-}
tapRate :: Int -> Stream IO Int -> IO ()
tapRate n str = do
    cref <- newIORef 0
    composeN n (Internal.tapRate 1 (\c -> modifyIORef' cref (c +))) str

{-# INLINE pollCounts #-}
pollCounts :: Int -> Stream IO Int -> IO ()
pollCounts n str = do
    composeN n (Internal.pollCounts (P.const P.True) f FL.drain) str
  where
    f = Internal.rollingMap (P.-) . Internal.delayPost 1

{-# INLINE tapAsyncS #-}
tapAsyncS :: S.MonadAsync m => Int -> Stream m Int -> m ()
tapAsyncS n = composeN n $ Par.tapAsync S.sum

{-# INLINE tapAsync #-}
tapAsync :: S.MonadAsync m => Int -> Stream m Int -> m ()
tapAsync n = composeN n $ Internal.tapAsync FL.sum

{-# INLINE timestamped #-}
timestamped :: (S.MonadAsync m) => Stream m Int -> m ()
timestamped = transform . Internal.timestamped

{-# INLINE classifySessionsOf #-}
classifySessionsOf :: (S.MonadAsync m) => Stream m Int -> m ()
classifySessionsOf =
      transform
    . Internal.classifySessionsOf
        3 (const (return False)) (P.fmap Right FL.drain)
    . S.map (\(ts,(k,a)) -> (k, a, ts))
    . Internal.timestamped
    . S.concatMap (\x -> S.map (x,) (S.enumerateFromTo 1 (10 :: Int)))

{-# INLINE mapMaybe #-}
mapMaybe :: MonadIO m => Int -> Stream m Int -> m ()
mapMaybe n =
    composeN n $
    S.mapMaybe
        (\x ->
             if P.odd x
                 then Nothing
                 else Just x)

{-# INLINE mapMaybeM #-}
mapMaybeM :: S.MonadAsync m => Int -> Stream m Int -> m ()
mapMaybeM n =
    composeN n $
    S.mapMaybeM
        (\x ->
             if P.odd x
                 then return Nothing
                 else return $ Just x)

{-# INLINE sequence #-}
sequence ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> t m (m Int)
    -> m ()
sequence t = transform . t . S.sequence

{-# INLINE filterEven #-}
filterEven :: MonadIO m => Int -> Stream m Int -> m ()
filterEven n = composeN n $ S.filter even

{-# INLINE filterAllOut #-}
filterAllOut :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterAllOut value n = composeN n $ S.filter (> (value + 1))

{-# INLINE filterAllIn #-}
filterAllIn :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterAllIn value n = composeN n $ S.filter (<= (value + 1))

{-# INLINE _takeOne #-}
_takeOne :: MonadIO m => Int -> Stream m Int -> m ()
_takeOne n = composeN n $ S.take 1

{-# INLINE takeAll #-}
takeAll :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeAll value n = composeN n $ S.take (value + 1)

{-# INLINE takeWhileTrue #-}
takeWhileTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeWhileTrue value n = composeN n $ S.takeWhile (<= (value + 1))

{-# INLINE _takeWhileMTrue #-}
_takeWhileMTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
_takeWhileMTrue value n = composeN n $ S.takeWhileM (return . (<= (value + 1)))

{-# INLINE dropOne #-}
dropOne :: MonadIO m => Int -> Stream m Int -> m ()
dropOne n = composeN n $ S.drop 1

{-# INLINE dropAll #-}
dropAll :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropAll value n = composeN n $ S.drop (value + 1)

{-# INLINE dropWhileTrue #-}
dropWhileTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropWhileTrue value n = composeN n $ S.dropWhile (<= (value + 1))

{-# INLINE _dropWhileMTrue #-}
_dropWhileMTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
_dropWhileMTrue value n = composeN n $ S.dropWhileM (return . (<= (value + 1)))

{-# INLINE dropWhileFalse #-}
dropWhileFalse :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropWhileFalse value n = composeN n $ S.dropWhile (> (value + 1))

{-# INLINE findIndices #-}
findIndices :: MonadIO m => Int -> Int -> Stream m Int -> m ()
findIndices value n = composeN n $ S.findIndices (== (value + 1))

{-# INLINE elemIndices #-}
elemIndices :: MonadIO m => Int -> Int -> Stream m Int -> m ()
elemIndices value n = composeN n $ S.elemIndices (value + 1)

{-# INLINE intersperse #-}
intersperse :: S.MonadAsync m => Int -> Int -> Stream m Int -> m ()
intersperse value n = composeN n $ S.intersperse (value + 1)

{-# INLINE insertBy #-}
insertBy :: MonadIO m => Int -> Int -> Stream m Int -> m ()
insertBy value n = composeN n $ S.insertBy compare (value + 1)

{-# INLINE deleteBy #-}
deleteBy :: MonadIO m => Int -> Int -> Stream m Int -> m ()
deleteBy value n = composeN n $ S.deleteBy (>=) (value + 1)

{-# INLINE foldrS #-}
foldrS :: MonadIO m => Int -> Stream m Int -> m ()
foldrS n = composeN n $ Internal.foldrS S.cons S.nil

{-# INLINE foldrSMap #-}
foldrSMap :: MonadIO m => Int -> Stream m Int -> m ()
foldrSMap n = composeN n $ Internal.foldrS (\x xs -> x + 1 `S.cons` xs) S.nil

{-# INLINE foldrT #-}
foldrT :: MonadIO m => Int -> Stream m Int -> m ()
foldrT n = composeN n $ Internal.foldrT S.cons S.nil

{-# INLINE foldrTMap #-}
foldrTMap :: MonadIO m => Int -> Stream m Int -> m ()
foldrTMap n = composeN n $ Internal.foldrT (\x xs -> x + 1 `S.cons` xs) S.nil

{-# INLINE takeByTime #-}
takeByTime :: NanoSecond64 -> Int -> Stream IO Int -> IO ()
takeByTime i n = composeN n (Internal.takeByTime i)

#ifdef INSPECTION
-- inspect $ hasNoType 'takeByTime ''SPEC
inspect $ hasNoTypeClasses 'takeByTime
-- inspect $ 'takeByTime `hasNoType` ''D.Step
#endif

{-# INLINE dropByTime #-}
dropByTime :: NanoSecond64 -> Int -> Stream IO Int -> IO ()
dropByTime i n = composeN n (Internal.dropByTime i)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropByTime
-- inspect $ 'dropByTime `hasNoType` ''D.Step
#endif

-------------------------------------------------------------------------------
-- Pipes
-------------------------------------------------------------------------------

{-# INLINE transformMapM #-}
transformMapM ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
transformMapM t n = composeN' n $ t . Internal.transform (Pipe.mapM return)

{-# INLINE transformComposeMapM #-}
transformComposeMapM ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
transformComposeMapM t n =
    composeN' n $
    t .
    Internal.transform
        (Pipe.mapM (\x -> return (x + 1)) `Pipe.compose`
         Pipe.mapM (\x -> return (x + 2)))

{-# INLINE transformTeeMapM #-}
transformTeeMapM ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
transformTeeMapM t n =
    composeN' n $
    t .
    Internal.transform
        (Pipe.mapM (\x -> return (x + 1)) `Pipe.tee`
         Pipe.mapM (\x -> return (x + 2)))

{-# INLINE transformZipMapM #-}
transformZipMapM ::
       (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int)
    -> Int
    -> t m Int
    -> m ()
transformZipMapM t n =
    composeN' n $
    t .
    Internal.transform
        (Pipe.zipWith
             (+)
             (Pipe.mapM (\x -> return (x + 1)))
             (Pipe.mapM (\x -> return (x + 2))))


-------------------------------------------------------------------------------
-- Mixed Transformation
-------------------------------------------------------------------------------

{-# INLINE scanMap #-}
scanMap :: MonadIO m => Int -> Stream m Int -> m ()
scanMap n = composeN n $ S.map (subtract 1) . S.scanl' (+) 0

{-# INLINE dropMap #-}
dropMap :: MonadIO m => Int -> Stream m Int -> m ()
dropMap n = composeN n $ S.map (subtract 1) . S.drop 1

{-# INLINE dropScan #-}
dropScan :: MonadIO m => Int -> Stream m Int -> m ()
dropScan n = composeN n $ S.scanl' (+) 0 . S.drop 1

{-# INLINE takeDrop #-}
takeDrop :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeDrop value n = composeN n $ S.drop 1 . S.take (value + 1)

{-# INLINE takeScan #-}
takeScan :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeScan value n = composeN n $ S.scanl' (+) 0 . S.take (value + 1)

{-# INLINE takeMap #-}
takeMap :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeMap value n = composeN n $ S.map (subtract 1) . S.take (value + 1)

{-# INLINE filterDrop #-}
filterDrop :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterDrop value n = composeN n $ S.drop 1 . S.filter (<= (value + 1))

{-# INLINE filterTake #-}
filterTake :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterTake value n = composeN n $ S.take (value + 1) . S.filter (<= (value + 1))

{-# INLINE filterScan #-}
filterScan :: MonadIO m => Int -> Stream m Int -> m ()
filterScan n = composeN n $ S.scanl' (+) 0 . S.filter (<= maxBound)

{-# INLINE filterScanl1 #-}
filterScanl1 :: MonadIO m => Int -> Stream m Int -> m ()
filterScanl1 n = composeN n $ S.scanl1' (+) . S.filter (<= maxBound)

{-# INLINE filterMap #-}
filterMap :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterMap value n = composeN n $ S.map (subtract 1) . S.filter (<= (value + 1))

-------------------------------------------------------------------------------
-- Scan and fold
-------------------------------------------------------------------------------

data Pair a b =
    Pair !a !b
    deriving (Generic, NFData)

{-# INLINE sumProductFold #-}
sumProductFold :: Monad m => Stream m Int -> m (Int, Int)
sumProductFold = S.foldl' (\(s, p) x -> (s + x, p P.* x)) (0, 1)

{-# INLINE sumProductScan #-}
sumProductScan :: Monad m => Stream m Int -> m (Pair Int Int)
sumProductScan =
    S.foldl' (\(Pair _ p) (s0, x) -> Pair s0 (p P.* x)) (Pair 0 1) .
    S.scanl' (\(s, _) x -> (s + x, x)) (0, 0)

-------------------------------------------------------------------------------
-- Iteration
-------------------------------------------------------------------------------

{-# INLINE iterStreamLen #-}
iterStreamLen :: Int
iterStreamLen = 10

{-# INLINE maxIters #-}
maxIters :: Int
maxIters = 10000

{-# INLINE iterateSource #-}
iterateSource ::
       S.MonadAsync m
    => (Stream m Int -> Stream m Int)
    -> Int
    -> Int
    -> Stream m Int
iterateSource g i n = f i (sourceUnfoldrMN iterStreamLen n)
  where
    f (0 :: Int) m = g m
    f x m = g (f (x P.- 1) m)

-- this is quadratic
{-# INLINE iterateScan #-}
iterateScan :: S.MonadAsync m => Int -> Stream m Int
iterateScan = iterateSource (S.scanl' (+) 0) (maxIters `div` 10)

-- this is quadratic
{-# INLINE iterateScanl1 #-}
iterateScanl1 :: S.MonadAsync m => Int -> Stream m Int
iterateScanl1 = iterateSource (S.scanl1' (+)) (maxIters `div` 10)

{-# INLINE iterateMapM #-}
iterateMapM :: S.MonadAsync m => Int -> Stream m Int
iterateMapM = iterateSource (S.mapM return) maxIters

{-# INLINE iterateFilterEven #-}
iterateFilterEven :: S.MonadAsync m => Int -> Stream m Int
iterateFilterEven = iterateSource (S.filter even) maxIters

{-# INLINE iterateTakeAll #-}
iterateTakeAll :: S.MonadAsync m => Int -> Int -> Stream m Int
iterateTakeAll value = iterateSource (S.take (value + 1)) maxIters

{-# INLINE iterateDropOne #-}
iterateDropOne :: S.MonadAsync m => Int -> Stream m Int
iterateDropOne = iterateSource (S.drop 1) maxIters

{-# INLINE iterateDropWhileFalse #-}
iterateDropWhileFalse :: S.MonadAsync m => Int -> Int -> Stream m Int
iterateDropWhileFalse value =
    iterateSource (S.dropWhile (> (value + 1))) maxIters

{-# INLINE iterateDropWhileTrue #-}
iterateDropWhileTrue :: S.MonadAsync m => Int -> Int -> Stream m Int
iterateDropWhileTrue value =
    iterateSource (S.dropWhile (<= (value + 1))) maxIters

-------------------------------------------------------------------------------
-- Monad transformation (hoisting etc.)
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldrState #-}
sourceUnfoldrState :: (S.IsStream t, S.MonadAsync m)
                   => Int -> Int -> t (StateT Int m) Int
sourceUnfoldrState value n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else do
            s <- get
            put (s + 1)
            return (Just (s, cnt + 1))

{-# INLINE evalStateT #-}
evalStateT :: S.MonadAsync m => Int -> Int -> Stream m Int
evalStateT value n = Internal.evalStateT 0 (sourceUnfoldrState value n)

{-# INLINE withState #-}
withState :: S.MonadAsync m => Int -> Int -> Stream m Int
withState value n =
    Internal.evalStateT (0 :: Int) (Internal.liftInner (sourceUnfoldrM value n))

-------------------------------------------------------------------------------
-- Traversable Instance
-------------------------------------------------------------------------------

{-# INLINE traversableTraverse #-}
traversableTraverse :: Stream Identity Int -> IO (Stream Identity Int)
traversableTraverse = P.traverse return

{-# INLINE traversableSequenceA #-}
traversableSequenceA :: Stream Identity Int -> IO (Stream Identity Int)
traversableSequenceA = P.sequenceA . P.fmap return

{-# INLINE traversableMapM #-}
traversableMapM :: Stream Identity Int -> IO (Stream Identity Int)
traversableMapM = P.mapM return

{-# INLINE traversableSequence #-}
traversableSequence :: Stream Identity Int -> IO (Stream Identity Int)
traversableSequence = P.sequence . P.fmap return

-------------------------------------------------------------------------------
-- Benchmark groups
-------------------------------------------------------------------------------

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: (IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f = bench name $ nfIO $ randomRIO (1,1) >>= f . source value

-- XXX We should be using sourceUnfoldrM for fair comparison with IO monad, but
-- we can't use it as it requires MonadAsync constraint.

-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchIOSrc #-}
benchIOSrc
    :: (t IO a -> SerialT IO a)
    -> String
    -> (Int -> t IO a)
    -> Benchmark
benchIOSrc t name f =
    bench name $ nfIO $ randomRIO (1,1) >>= toNull t . f

{-# INLINE benchPureSinkIO #-}
benchPureSinkIO
    :: NFData b
    => Int -> String -> (SerialT Identity Int -> IO b) -> Benchmark
benchPureSinkIO value name f =
    bench name $ nfIO $ randomRIO (1, 1) >>= f . sourceUnfoldr value

-------------------------------------------------------------------------------
-- Benchmark groups
-------------------------------------------------------------------------------

o_1_space_serial_pipes :: Int -> [Benchmark]
o_1_space_serial_pipes value =
    [ bgroup
          "serially"
          [ bgroup
                "pipes"
                [ benchIOSink value "mapM" (transformMapM serially 1)
                , benchIOSink
                      value
                      "compose"
                      (transformComposeMapM serially 1)
                , benchIOSink value "tee" (transformTeeMapM serially 1)
                , benchIOSink value "zip" (transformZipMapM serially 1)
                ]
          ]
    ]

o_1_space_serial_pipesX4 :: Int -> [Benchmark]
o_1_space_serial_pipesX4 value =
    [ bgroup
          "serially"
          [ bgroup
                "pipesX4"
                [ benchIOSink value "mapM" (transformMapM serially 4)
                , benchIOSink
                      value
                      "compose"
                      (transformComposeMapM serially 4)
                , benchIOSink value "tee" (transformTeeMapM serially 4)
                , benchIOSink value "zip" (transformZipMapM serially 4)
                ]
          ]
    ]


o_1_space_serial_transformer :: Int -> [Benchmark]
o_1_space_serial_transformer value =
    [ bgroup
          "serially"
          [ bgroup
                "transformer"
                [ benchIOSrc serially "evalState" (evalStateT value)
                , benchIOSrc serially "withState" (withState value)
                ]
          ]
    ]

o_1_space_serial_transformation :: Int -> [Benchmark]
o_1_space_serial_transformation value =
    [ bgroup
          "serially"
          [ bgroup
                "transformation"
                [ benchIOSink value "scanl" (scan 1)
                , benchIOSink value "scanl1'" (scanl1' 1)
                , benchIOSink value "map" (map 1)
                , benchIOSink value "fmap" (fmap 1)
                , benchIOSink value "mapM" (mapM serially 1)
                , benchIOSink value "mapMaybe" (mapMaybe 1)
                , benchIOSink value "mapMaybeM" (mapMaybeM 1)
                , bench "sequence" $
                  nfIO $
                  randomRIO (1, 1000) >>= \n ->
                      sequence serially (sourceUnfoldrMAction value n)
                , benchIOSink value "findIndices" (findIndices value 1)
                , benchIOSink value "elemIndices" (elemIndices value 1)
                , benchIOSink value "foldrS" (foldrS 1)
                , benchIOSink value "foldrSMap" (foldrSMap 1)
                , benchIOSink value "foldrT" (foldrT 1)
                , benchIOSink value "foldrTMap" (foldrTMap 1)
                , benchIOSink value "tap" (tap 1)
                , benchIOSink value "tapRate 1 second" (tapRate 1)
                , benchIOSink value "pollCounts 1 second" (pollCounts 1)
                , benchIOSink value "tapAsync" (tapAsync 1)
                , benchIOSink value "tapAsyncS" (tapAsyncS 1)
                , benchIOSink value "timestamped" timestamped
                ]
          ]
    ]

o_1_space_serial_transformationX4 :: Int -> [Benchmark]
o_1_space_serial_transformationX4 value =
    [ bgroup
          "serially"
          [ bgroup
                "transformationX4"
                [ benchIOSink value "scan" (scan 4)
                , benchIOSink value "scanl1'" (scanl1' 4)
                , benchIOSink value "map" (map 4)
                , benchIOSink value "fmap" (fmap 4)
                , benchIOSink value "mapM" (mapM serially 4)
                , benchIOSink value "mapMaybe" (mapMaybe 4)
                , benchIOSink value "mapMaybeM" (mapMaybeM 4)
            -- , bench "sequence" $ nfIO $ randomRIO (1,1000) >>= \n ->
                -- sequence serially (sourceUnfoldrMAction n)
                , benchIOSink value "findIndices" (findIndices value 4)
                , benchIOSink value "elemIndices" (elemIndices value 4)
                ]
          ]
    ]

o_1_space_serial_filtering :: Int -> [Benchmark]
o_1_space_serial_filtering value =
    [ bgroup
          "serially"
          [ bgroup
                "filtering"
                [ benchIOSink value "filter-even" (filterEven 1)
                , benchIOSink value "filter-all-out" (filterAllOut value 1)
                , benchIOSink value "filter-all-in" (filterAllIn value 1)
                , benchIOSink value "take-all" (takeAll value 1)
                , benchIOSink
                      value
                      "takeByTime-all"
                      (takeByTime (NanoSecond64 maxBound) 1)
                , benchIOSink value "takeWhile-true" (takeWhileTrue value 1)
            --, benchIOSink value "takeWhileM-true" (_takeWhileMTrue 1)
            -- "drop-one" is dual to "last"
                , benchIOSink value "drop-one" (dropOne 1)
                , benchIOSink value "drop-all" (dropAll value 1)
                , benchIOSink
                      value
                      "dropByTime-all"
                      (dropByTime (NanoSecond64 maxBound) 1)
                , benchIOSink value "dropWhile-true" (dropWhileTrue value 1)
            --, benchIOSink value "dropWhileM-true" (_dropWhileMTrue 1)
                , benchIOSink
                      value
                      "dropWhile-false"
                      (dropWhileFalse value 1)
                , benchIOSink value "deleteBy" (deleteBy value 1)
                , benchIOSink value "intersperse" (intersperse value 1)
                , benchIOSink value "insertBy" (insertBy value 1)
                ]
          ]
    ]

o_1_space_serial_filteringX4 :: Int -> [Benchmark]
o_1_space_serial_filteringX4 value =
    [ bgroup
          "serially"
          [ bgroup
                "filteringX4"
                [ benchIOSink value "filter-even" (filterEven 4)
                , benchIOSink value "filter-all-out" (filterAllOut value 4)
                , benchIOSink value "filter-all-in" (filterAllIn value 4)
                , benchIOSink value "take-all" (takeAll value 4)
                , benchIOSink value "takeWhile-true" (takeWhileTrue value 4)
            --, benchIOSink value "takeWhileM-true" (_takeWhileMTrue 4)
                , benchIOSink value "drop-one" (dropOne 4)
                , benchIOSink value "drop-all" (dropAll value 4)
                , benchIOSink value "dropWhile-true" (dropWhileTrue value 4)
            --, benchIOSink value "dropWhileM-true" (_dropWhileMTrue 4)
                , benchIOSink
                      value
                      "dropWhile-false"
                      (dropWhileFalse value 4)
                , benchIOSink value "deleteBy" (deleteBy value 4)
                , benchIOSink value "intersperse" (intersperse value 4)
                , benchIOSink value "insertBy" (insertBy value 4)
                ]
          ]
    ]


o_1_space_serial_mixed :: Int -> [Benchmark]
o_1_space_serial_mixed value =
    [ bgroup
          "serially"
          -- scanl-map and foldl-map are equivalent to the scan and fold in the foldl
          -- library. If scan/fold followed by a map is efficient enough we may not
          -- need monolithic implementations of these.
          [ bgroup
                "mixed"
                [ benchIOSink value "scanl-map" (scanMap 1)
                , benchIOSink value "foldl-map" foldl'ReduceMap
                , benchIOSink value "sum-product-fold" sumProductFold
                , benchIOSink value "sum-product-scan" sumProductScan
                ]
          ]
    ]

o_1_space_serial_mixedX4 :: Int -> [Benchmark]
o_1_space_serial_mixedX4 value =
    [ bgroup
          "serially"
          [ bgroup
                "mixedX4"
                [ benchIOSink value "scan-map" (scanMap 4)
                , benchIOSink value "drop-map" (dropMap 4)
                , benchIOSink value "drop-scan" (dropScan 4)
                , benchIOSink value "take-drop" (takeDrop value 4)
                , benchIOSink value "take-scan" (takeScan value 4)
                , benchIOSink value "take-map" (takeMap value 4)
                , benchIOSink value "filter-drop" (filterDrop value 4)
                , benchIOSink value "filter-take" (filterTake value 4)
                , benchIOSink value "filter-scan" (filterScan 4)
                , benchIOSink value "filter-scanl1" (filterScanl1 4)
                , benchIOSink value "filter-map" (filterMap value 4)
                ]
          ]
    ]

o_1_space_wSerial_transformation :: Int -> [Benchmark]
o_1_space_wSerial_transformation value =
    [ bgroup
          "wSerially"
          [ bgroup
                "transformation"
                [benchIOSink value "fmap" $ fmap' wSerially 1]
          ]
    ]

o_1_space_zipSerial_transformation :: Int -> [Benchmark]
o_1_space_zipSerial_transformation value =
    [ bgroup
          "zipSerially"
          [ bgroup
                "transformation"
                [benchIOSink value "fmap" $ fmap' zipSerially 1]
            -- XXX needs fixing
            {-
          , bgroup "outer-product"
            [ benchIO "toNullAp"  $ Nested.toNullAp value  zipSerially
            ]
            -}
          ]
    ]


o_n_space_serial :: Int -> [Benchmark]
o_n_space_serial value =
    [ bgroup
          "serially"
        -- Buffering operations using heap proportional to number of elements.
          [ bgroup
                "traversable" -- < 2MB
            -- Traversable instance
                [ benchPureSinkIO value "traverse" traversableTraverse
                , benchPureSinkIO value "sequenceA" traversableSequenceA
                , benchPureSinkIO value "mapM" traversableMapM
                , benchPureSinkIO value "sequence" traversableSequence
                ]
          , benchIOSink (value `div` 10) "classifySessionsOf"
              classifySessionsOf
          ]
    ]


-- Head recursive operations.
o_n_stack_serial_iterated :: Int -> [Benchmark]
o_n_stack_serial_iterated value =
    [ bgroup
          "serially"
          [ bgroup
                "iterated"
                [ benchIOSrc serially "mapMx10K" iterateMapM
                , benchIOSrc serially "scanx100" iterateScan
                , benchIOSrc serially "scanl1x100" iterateScanl1
                , benchIOSrc serially "filterEvenx10K" iterateFilterEven
                , benchIOSrc serially "takeAllx10K" (iterateTakeAll value)
                , benchIOSrc serially "dropOnex10K" iterateDropOne
                , benchIOSrc
                      serially
                      "dropWhileFalsex10K"
                      (iterateDropWhileFalse value)
                , benchIOSrc
                      serially
                      "dropWhileTruex10K"
                      (iterateDropWhileTrue value)
                , benchIOSink value "tail" tail
                , benchIOSink value "nullHeadTail" nullHeadTail
                ]
          ]
    ]


o_1_space_async_transformation :: Int -> [Benchmark]
o_1_space_async_transformation value =
    [ bgroup
          "asyncly"
          [ bgroup
                "transformation"
                [ benchIOSink value "map" $ map' asyncly 1
                , benchIOSink value "fmap" $ fmap' asyncly 1
                , benchIOSink value "mapM" $ mapM asyncly 1
                ]
          ]
    ]


o_1_space_wAsync_transformation :: Int -> [Benchmark]
o_1_space_wAsync_transformation value =
    [ bgroup
          "wAsyncly"
          [ bgroup
                "transformation"
                [ benchIOSink value "map" $ map' wAsyncly 1
                , benchIOSink value "fmap" $ fmap' wAsyncly 1
                , benchIOSink value "mapM" $ mapM wAsyncly 1
                ]
          ]
    ]



o_1_space_ahead_transformation :: Int -> [Benchmark]
o_1_space_ahead_transformation value =
    [ bgroup
          "aheadly"
          [ bgroup
                "transformation"
                [ benchIOSink value "map" $ map' aheadly 1
                , benchIOSink value "fmap" $ fmap' aheadly 1
                , benchIOSink value "mapM" $ mapM aheadly 1
                ]
          ]
    ]



o_n_space_parallel_transformation :: Int -> [Benchmark]
o_n_space_parallel_transformation value =
    [ bgroup
          "parallely"
          [ bgroup
                "transformation"
                [ benchIOSink value "map" $ map' parallely 1
                , benchIOSink value "fmap" $ fmap' parallely 1
                , benchIOSink value "mapM" $ mapM parallely 1
                ]
          ]
    ]
