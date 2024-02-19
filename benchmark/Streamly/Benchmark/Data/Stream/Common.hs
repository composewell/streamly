{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Stream.Common
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

#ifdef USE_PRELUDE
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif

module Stream.Common
    ( MonadAsync

    -- Generation
    , fromListM
    , fromFoldableM

    , append
    , append2

    -- Elimination
    , drain
    , foldl'
    , scanl'

    -- Benchmark stream generation
    , sourceUnfoldr
    , sourceUnfoldrM
    , sourceUnfoldrAction
    , sourceConcatMapId
    , sourceFromFoldable

    -- Benchmark stream elimination
    , benchIOSink
    , benchIOSrc
    , benchIO

    -- Benchmarking functions
    , apDiscardFst
    , apDiscardSnd
    , apLiftA2
    , toNullAp
    , monadThen
    , toNullM
    , toNullM3
    , filterAllOutM
    , filterAllInM
    , filterSome
    , breakAfterSome
    , toListM
    , toListSome
    , composeN
    , mapN
    , mapM
    , transformMapM
    , transformComposeMapM
    , transformTeeMapM
    -- , transformZipMapM
#ifndef USE_PRELUDE
    , scanMapM
    , scanComposeMapM
    , scanTeeMapM
#endif
    )
where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.DeepSeq (NFData)
import Control.Exception (try)
import GHC.Exception (ErrorCall)
import System.Random (randomRIO)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Internal.Data.Scan as Scan

#ifdef USE_PRELUDE
import Streamly.Prelude (foldl', scanl')
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Prelude as Stream
import Streamly.Benchmark.Prelude
    ( composeN, sourceConcatMapId, benchIOSink
    )
#else
import Streamly.Internal.Data.Stream (Stream)
import qualified Streamly.Internal.Data.Stream as D
import qualified Streamly.Internal.Data.Stream as Stream
#endif

import Test.Tasty.Bench
import Prelude hiding (Foldable(..), mapM, replicate)

#ifdef USE_PRELUDE
type Stream = Stream.SerialT
type MonadAsync m = Stream.MonadAsync m
mkCross = id
unCross = id
#else
type MonadAsync = Monad

mkCross :: Stream m a -> Stream.CrossStream m a
mkCross = Stream.mkCross

unCross :: Stream.CrossStream m a -> Stream m a
unCross = Stream.unCross
#endif

#ifdef USE_PRELUDE
{-# INLINE append #-}
append :: Monad m => Stream m a -> Stream m a -> Stream m a
append = Stream.serial
#else
append :: Monad m => Stream m a -> Stream m a -> Stream m a
append = Stream.append
#endif

{-# INLINE append2 #-}
append2 :: Monad m => Stream m a -> Stream m a -> Stream m a
#ifdef USE_PRELUDE
append2 = Stream.append
#else
append2 = D.append
#endif

{-# INLINE drain #-}
drain :: Monad m => Stream m a -> m ()
{-# INLINE toList #-}
toList :: Monad m => Stream m a -> m [a]
#ifdef USE_PRELUDE
drain = Stream.drain
toList = Stream.toList
#else
drain = Stream.fold Fold.drain
toList = Stream.fold Fold.toList
#endif

{-# INLINE fromListM #-}
fromListM :: MonadAsync m => [m a] -> Stream m a
#ifdef USE_PRELUDE
fromListM = Stream.fromListM
#else
fromListM = Stream.sequence . Stream.fromList
#endif

{-# INLINE fromFoldableM #-}
fromFoldableM :: MonadAsync m => [m a] -> Stream m a
#ifdef USE_PRELUDE
fromFoldableM = Stream.fromFoldableM
#else
fromFoldableM = Stream.sequence . Stream.fromFoldable
#endif

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: MonadAsync m => Int -> Int -> Stream m Int
sourceUnfoldrM count start = Stream.unfoldrM step start

    where

    step cnt =
        if cnt > start + count
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: Monad m => Int -> Int -> Stream m Int
sourceUnfoldr count start = Stream.unfoldr step start

    where

    step cnt =
        if cnt > start + count
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceUnfoldrAction #-}
sourceUnfoldrAction :: (Monad m1, Monad m) => Int -> Int -> Stream m (m1 Int)
sourceUnfoldrAction value n = Stream.unfoldr step n

    where

    step cnt =
        if cnt > n + value
        then Nothing
        else Just (return cnt, cnt + 1)

{-# INLINE sourceFromFoldable #-}
sourceFromFoldable :: Monad m => Int -> Int -> Stream m Int
sourceFromFoldable value n = Stream.fromFoldable [n..n+value]

#ifndef USE_PRELUDE
{-# INLINE benchIOSink #-}
benchIOSink
    :: (NFData b)
    => Int -> String -> (Stream IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrM value
#endif

-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchIOSrc #-}
benchIOSrc
    :: String
    -> (Int -> Stream IO a)
    -> Benchmark
benchIOSrc name f =
    bench name $ nfIO $ randomRIO (1,1) >>= drain . f

{-# NOINLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

#ifndef USE_PRELUDE
{-# INLINE sourceConcatMapId #-}
sourceConcatMapId :: (Monad m)
    => Int -> Int -> Stream m (Stream m Int)
sourceConcatMapId value n =
    Stream.fromList $ fmap (D.fromEffect . return) [n..n+value]
#endif

{-# INLINE apDiscardFst #-}
apDiscardFst :: MonadAsync m =>
    Int -> Int -> m ()
apDiscardFst linearCount start = drain $ unCross $
    mkCross (sourceUnfoldrM nestedCount2 start)
        *> mkCross (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE apDiscardSnd #-}
apDiscardSnd :: MonadAsync m => Int -> Int -> m ()
apDiscardSnd linearCount start = drain $ unCross $
    mkCross (sourceUnfoldrM nestedCount2 start)
        <* mkCross (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE apLiftA2 #-}
apLiftA2 :: MonadAsync m => Int -> Int -> m ()
apLiftA2 linearCount start = drain $ unCross $
    liftA2 (+) (mkCross (sourceUnfoldrM nestedCount2 start))
        (mkCross (sourceUnfoldrM nestedCount2 start))

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullAp #-}
toNullAp :: MonadAsync m => Int -> Int -> m ()
toNullAp linearCount start = drain $ unCross $
    (+) <$> mkCross (sourceUnfoldrM nestedCount2 start)
        <*> mkCross (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE monadThen #-}
monadThen :: MonadAsync m => Int -> Int -> m ()
monadThen linearCount start = drain $ unCross $ do
    mkCross (sourceUnfoldrM nestedCount2 start) >>
        mkCross (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullM #-}
toNullM :: MonadAsync m => Int -> Int -> m ()
toNullM linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldrM nestedCount2 start)
    y <- mkCross (sourceUnfoldrM nestedCount2 start)
    return $ x + y

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullM3 #-}
toNullM3 :: MonadAsync m => Int -> Int -> m ()
toNullM3 linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldrM nestedCount3 start)
    y <- mkCross (sourceUnfoldrM nestedCount3 start)
    z <- mkCross (sourceUnfoldrM nestedCount3 start)
    return $ x + y + z
  where
    nestedCount3 = round (fromIntegral linearCount**(1/3::Double))

{-# INLINE filterAllOutM #-}
filterAllOutM :: MonadAsync m => Int -> Int -> m ()
filterAllOutM linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldrM nestedCount2 start)
    y <- mkCross (sourceUnfoldrM nestedCount2 start)
    let s = x + y
    if s < 0
    then return s
    else mkCross Stream.nil
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE filterAllInM #-}
filterAllInM :: MonadAsync m => Int -> Int -> m ()
filterAllInM linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldrM nestedCount2 start)
    y <- mkCross (sourceUnfoldrM nestedCount2 start)
    let s = x + y
    if s > 0
    then return s
    else mkCross Stream.nil
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE filterSome #-}
filterSome :: MonadAsync m => Int -> Int -> m ()
filterSome linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldrM nestedCount2 start)
    y <- mkCross (sourceUnfoldrM nestedCount2 start)
    let s = x + y
    if s > 1100000
    then return s
    else mkCross Stream.nil
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE breakAfterSome #-}
breakAfterSome :: Int -> Int -> IO ()
breakAfterSome linearCount start = do
    (_ :: Either ErrorCall ()) <- try $ drain $ unCross $ do
        x <- mkCross (sourceUnfoldrM nestedCount2 start)
        y <- mkCross (sourceUnfoldrM nestedCount2 start)
        let s = x + y
        if s > 1100000
        then error "break"
        else return s
    return ()
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toListM #-}
toListM :: MonadAsync m => Int -> Int -> m [Int]
toListM linearCount start = toList $ unCross $ do
    x <- mkCross (sourceUnfoldrM nestedCount2 start)
    y <- mkCross (sourceUnfoldrM nestedCount2 start)
    return $ x + y
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

-- Taking a specified number of elements is very expensive in logict so we have
-- a test to measure the same.
{-# INLINE toListSome #-}
toListSome :: MonadAsync m => Int -> Int -> m [Int]
toListSome linearCount start =
    toList $ Stream.take 10000 $ unCross $ do
        x <- mkCross (sourceUnfoldrM nestedCount2 start)
        y <- mkCross (sourceUnfoldrM nestedCount2 start)
        return $ x + y
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

#ifndef USE_PRELUDE
{-# INLINE composeN #-}
composeN ::
       (Monad m)
    => Int
    -> (Stream m Int -> Stream m Int)
    -> Stream m Int
    -> m ()
composeN n f =
    case n of
        1 -> drain . f
        2 -> drain . f . f
        3 -> drain . f . f . f
        4 -> drain . f . f . f . f
        _ -> undefined
#endif

{-# INLINE mapN #-}
mapN ::
       Monad m
    => Int
    -> Stream m Int
    -> m ()
mapN n = composeN n $ fmap (+ 1)

{-# INLINE mapM #-}
mapM ::
       MonadAsync m
    => Int
    -> Stream m Int
    -> m ()
mapM n = composeN n $ Stream.mapM return

#ifndef USE_PRELUDE
foldl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> m b
foldl' f z = Stream.fold (Fold.foldl' f z)

scanl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> Stream m b
scanl' f z = Stream.scan (Fold.foldl' f z)
#endif

{-# INLINE transformMapM #-}
transformMapM ::
       (Monad m)
    => Int
    -> Stream m Int
    -> m ()
#ifndef USE_PRELUDE
transformMapM n = composeN n $ Stream.pipe (Pipe.mapM return)
#else
transformMapM n = composeN n $ Stream.transform (Pipe.mapM return)
#endif

#ifndef USE_PRELUDE
{-# INLINE scanMapM #-}
scanMapM ::
       (Monad m)
    => Int
    -> Stream m Int
    -> m ()
scanMapM n = composeN n $ Stream.runScan (Scan.mapM return)
#endif

{-# INLINE transformComposeMapM #-}
transformComposeMapM ::
       (Monad m)
    => Int
    -> Stream m Int
    -> m ()
transformComposeMapM n =
    composeN n $
#ifndef USE_PRELUDE
    Stream.pipe
#else
    Stream.transform
#endif
        (Pipe.mapM (\x -> return (x + 1)) `Pipe.compose`
         Pipe.mapM (\x -> return (x + 2)))

#ifndef USE_PRELUDE
{-# INLINE scanComposeMapM #-}
scanComposeMapM ::
       (Monad m)
    => Int
    -> Stream m Int
    -> m ()
scanComposeMapM n =
    composeN n $
    Stream.runScan
        (Scan.mapM (\x -> return (x + 1)) `Scan.compose`
         Scan.mapM (\x -> return (x + 2)))
#endif

{-# INLINE transformTeeMapM #-}
transformTeeMapM ::
       (Monad m)
    => Int
    -> Stream m Int
    -> m ()
transformTeeMapM n =
    composeN n $
#ifndef USE_PRELUDE
    Stream.pipe
#else
    Stream.transform
#endif
        (Pipe.mapM (\x -> return (x + 1)) `Pipe.teeMerge`
         Pipe.mapM (\x -> return (x + 2)))

#ifndef USE_PRELUDE
{-# INLINE scanTeeMapM #-}
scanTeeMapM ::
       (Monad m)
    => Int
    -> Stream m Int
    -> m ()
scanTeeMapM n =
    composeN n $
    Stream.runScan
        (Scan.teeWith (+) (Scan.mapM (\x -> return (x + 1)))
         (Scan.mapM (\x -> return (x + 2))))
#endif

{-
{-# INLINE transformZipMapM #-}
transformZipMapM ::
       (Monad m)
    => Int
    -> Stream m Int
    -> m ()
transformZipMapM n =
    composeN n $
    Stream.pipe
        (Pipe.zipWith
             (+)
             (Pipe.mapM (\x -> return (x + 1)))
             (Pipe.mapM (\x -> return (x + 2))))
-}
