-- |
-- Module      : Streamly.Benchmark.Prelude
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Streamly.Benchmark.Prelude
    ( absTimes
    , apDiscardFst
    , apDiscardSnd
    , apLiftA2
    , benchIO
    , benchIOSink
    , benchIOSrc
    , breakAfterSome
    , composeN
    , concatFoldableWith
    , concatForFoldableWith
    , concatPairsWith
    , concatStreamsWith
    , filterAllInM
    , filterAllOutM
    , filterSome
    , fmapN
    , mapM
    , mapN
    , mkAsync
    , monadThen
    , runToList
    , sourceConcatMapId
    , sourceFoldMapM
    , sourceFoldMapWith
    , sourceFoldMapWithM
    , sourceFoldMapWithStream
    , sourceFracFromThenTo
    , sourceFracFromTo
    , sourceFromFoldable
    , sourceFromFoldableM
    , sourceFromList
    , sourceFromListM
    , sourceIntegerFromStep
    , sourceIntFromThenTo
    , sourceIntFromTo
    , sourceUnfoldr
    , sourceUnfoldrAction
    , sourceUnfoldrM
    , sourceUnfoldrMSerial
    , toListM
    , toListSome
    , toNull
    , toNullAp
    , toNullM
    , toNullM3
    , transformComposeMapM
    , transformMapM
    , transformTeeMapM
    , transformZipMapM
    )
where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData(..))
import Control.Exception (try)
import Data.Functor.Identity (Identity)
import GHC.Exception (ErrorCall)
import Prelude hiding (mapM)
import System.Random (randomRIO)

import qualified Data.Foldable as F
import qualified Data.List as List
import qualified Streamly.Prelude  as P
import qualified Streamly.Data.Stream as S
import qualified Streamly.Internal.Data.Stream.IsStream as Internal
import qualified Streamly.Internal.Data.Stream.IsStream.Type as IsStream
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Data.Stream as Stream  (unfoldrM)

import Gauge
import Streamly.Internal.Data.Time.Units

-- Common polymorphic stream APIs used across multiple stream modules

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- enumerate
-------------------------------------------------------------------------------

{-# INLINE sourceIntFromTo #-}
sourceIntFromTo :: (Monad m, P.IsStream t) => Int -> Int -> t m Int
sourceIntFromTo value n = P.enumerateFromTo n (n + value)

{-# INLINE sourceIntFromThenTo #-}
sourceIntFromThenTo :: (Monad m, P.IsStream t) => Int -> Int -> t m Int
sourceIntFromThenTo value n = P.enumerateFromThenTo n (n + 1) (n + value)

{-# INLINE sourceFracFromTo #-}
sourceFracFromTo :: (Monad m, P.IsStream t) => Int -> Int -> t m Double
sourceFracFromTo value n =
    P.enumerateFromTo (fromIntegral n) (fromIntegral (n + value))

{-# INLINE sourceFracFromThenTo #-}
sourceFracFromThenTo :: (Monad m, P.IsStream t) => Int -> Int -> t m Double
sourceFracFromThenTo value n = P.enumerateFromThenTo (fromIntegral n)
    (fromIntegral n + 1.0001) (fromIntegral (n + value))

{-# INLINE sourceIntegerFromStep #-}
sourceIntegerFromStep :: (Monad m, P.IsStream t) => Int -> Int -> t m Integer
sourceIntegerFromStep value n =
    P.take value $ P.enumerateFromThen (fromIntegral n) (fromIntegral n + 1)

-------------------------------------------------------------------------------
-- unfold
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: (Monad m, P.IsStream t) => Int -> Int -> t m Int
sourceUnfoldr count start = P.unfoldr step start
    where
    step cnt =
        if cnt > start + count
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (P.IsStream t, P.MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrM count start = P.unfoldrM step start
    where
    step cnt =
        if cnt > start + count
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldrMSerial #-}
sourceUnfoldrMSerial :: (P.IsStream t, Monad m) => Int -> Int -> t m Int
sourceUnfoldrMSerial count start =
    IsStream.fromSerial $ Stream.unfoldrM step start
    where
    step cnt =
        if cnt > start + count
        then return Nothing
        else return (Just (cnt, cnt + 1))

-------------------------------------------------------------------------------
-- fromList
-------------------------------------------------------------------------------

{-# INLINE sourceFromList #-}
sourceFromList :: (Monad m, P.IsStream t) => Int -> Int -> t m Int
sourceFromList value n = P.fromList [n..n+value]

{-# INLINE sourceFromListM #-}
sourceFromListM :: (P.MonadAsync m, P.IsStream t) => Int -> Int -> t m Int
sourceFromListM value n = P.fromListM (fmap return [n..n+value])

-------------------------------------------------------------------------------
-- fromFoldable
-------------------------------------------------------------------------------

{-# INLINE sourceFromFoldable #-}
sourceFromFoldable :: P.IsStream t => Int -> Int -> t m Int
sourceFromFoldable value n = P.fromFoldable [n..n+value]

{-# INLINE sourceFromFoldableM #-}
sourceFromFoldableM :: (P.IsStream t, P.MonadAsync m) => Int -> Int -> t m Int
sourceFromFoldableM value n = P.fromFoldableM (fmap return [n..n+value])

-------------------------------------------------------------------------------
-- Time enumeration
-------------------------------------------------------------------------------

{-# INLINE absTimes #-}
absTimes :: (P.IsStream t, P.MonadAsync m, Functor (t m))
    => Int -> Int -> t m AbsTime
absTimes value _ = P.take value Internal.absTimes

-------------------------------------------------------------------------------
-- Buffering
-------------------------------------------------------------------------------

{-# INLINE mkAsync #-}
mkAsync :: (P.MonadAsync m, P.IsStream t) => (t m a -> S.Stream m a) -> t m a -> m ()
mkAsync adapter = P.drain . adapter . P.mkAsync

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE toNull #-}
toNull :: Monad m => (t m a -> S.Stream m a) -> t m a -> m ()
toNull t = P.drain . t

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: (P.IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrM value

-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchIOSrc #-}
benchIOSrc
    :: (t IO a -> S.Stream IO a)
    -> String
    -> (Int -> t IO a)
    -> Benchmark
benchIOSrc t name f =
    bench name $ nfIO $ randomRIO (1,1) >>= toNull t . f

{-# NOINLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldrAction #-}
sourceUnfoldrAction :: (P.IsStream t, Monad m, Monad m1)
    => Int -> Int -> t m (m1 Int)
sourceUnfoldrAction value n = P.fromSerial $ S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else Just (return cnt, cnt + 1)

{-# INLINE composeN #-}
composeN ::
       (P.IsStream t, Monad m)
    => Int
    -> (t m Int -> S.Stream m Int)
    -> t m Int
    -> m ()
composeN n f =
    case n of
        1 -> P.drain . f
        2 -> P.drain . f . P.adapt . f
        3 -> P.drain . f . P.adapt . f . P.adapt . f
        4 -> P.drain . f . P.adapt . f . P.adapt . f . P.adapt . f
        _ -> undefined

{-# INLINE fmapN #-}
fmapN ::
       (P.IsStream t, P.MonadAsync m, Functor (t m))
    => (t m Int -> S.Stream m Int)
    -> Int
    -> t m Int
    -> m ()
fmapN t n = composeN n $ t . fmap (+ 1)

{-# INLINE mapN #-}
mapN ::
       (P.IsStream t, P.MonadAsync m)
    => (t m Int -> S.Stream m Int)
    -> Int
    -> t m Int
    -> m ()
mapN t n = composeN n $ t . P.map (+ 1)

{-# INLINE mapM #-}
mapM ::
       (P.IsStream t, P.MonadAsync m)
    => (t m Int -> S.Stream m Int)
    -> Int
    -> t m Int
    -> m ()
mapM t n = composeN n $ t . P.mapM return

-------------------------------------------------------------------------------
-- Pipes
-------------------------------------------------------------------------------

{-# INLINE transformMapM #-}
transformMapM ::
       (P.IsStream t, P.MonadAsync m)
    => (t m Int -> S.Stream m Int)
    -> Int
    -> t m Int
    -> m ()
transformMapM t n = composeN n $ t . Internal.transform (Pipe.mapM return)

{-# INLINE transformComposeMapM #-}
transformComposeMapM ::
       (P.IsStream t, P.MonadAsync m)
    => (t m Int -> S.Stream m Int)
    -> Int
    -> t m Int
    -> m ()
transformComposeMapM t n =
    composeN n $
    t .
    Internal.transform
        (Pipe.mapM (\x -> return (x + 1)) `Pipe.compose`
         Pipe.mapM (\x -> return (x + 2)))

{-# INLINE transformTeeMapM #-}
transformTeeMapM ::
       (P.IsStream t, P.MonadAsync m)
    => (t m Int -> S.Stream m Int)
    -> Int
    -> t m Int
    -> m ()
transformTeeMapM t n =
    composeN n $
    t .
    Internal.transform
        (Pipe.mapM (\x -> return (x + 1)) `Pipe.tee`
         Pipe.mapM (\x -> return (x + 2)))

{-# INLINE transformZipMapM #-}
transformZipMapM ::
       (P.IsStream t, P.MonadAsync m)
    => (t m Int -> S.Stream m Int)
    -> Int
    -> t m Int
    -> m ()
transformZipMapM t n =
    composeN n $
    t .
    Internal.transform
        (Pipe.zipWith
             (+)
             (Pipe.mapM (\x -> return (x + 1)))
             (Pipe.mapM (\x -> return (x + 2))))

-------------------------------------------------------------------------------
-- Streams of streams
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Concat foldable
-------------------------------------------------------------------------------

{-# INLINE sourceFoldMapWith #-}
sourceFoldMapWith :: (P.IsStream t, Semigroup (t m Int))
    => Int -> Int -> t m Int
sourceFoldMapWith value n = P.concatMapFoldableWith (<>) P.fromPure [n..n+value]

{-# INLINE concatForFoldableWith #-}
concatForFoldableWith :: (P.IsStream t, Semigroup (t m Int))
    => Int -> Int -> t m Int
concatForFoldableWith value n =
    P.concatForFoldableWith (<>) [n..n+value] P.fromPure

{-# INLINE concatFoldableWith #-}
concatFoldableWith :: (P.IsStream t, Semigroup (t m Int))
    => Int -> Int -> t m Int
concatFoldableWith value n =
    let step x =
            if x <= n + value
            then Just (P.fromPure x, x + 1)
            else Nothing
        list = List.unfoldr step n
     in P.concatFoldableWith (<>) list

{-# INLINE sourceFoldMapWithStream #-}
sourceFoldMapWithStream :: (P.IsStream t, Semigroup (t m Int))
    => Int -> Int -> t m Int
sourceFoldMapWithStream value n = P.concatMapFoldableWith (<>) P.fromPure
     (P.enumerateFromTo n (n + value) :: S.Stream Identity Int)

{-# INLINE sourceFoldMapWithM #-}
sourceFoldMapWithM :: (P.IsStream t, Monad m, Semigroup (t m Int))
    => Int -> Int -> t m Int
sourceFoldMapWithM value n =
    P.concatMapFoldableWith (<>) (P.fromEffect . return) [n..n+value]

{-# INLINE sourceFoldMapM #-}
sourceFoldMapM :: (P.IsStream t, Monad m, Monoid (t m Int))
    => Int -> Int -> t m Int
sourceFoldMapM value n = F.foldMap (P.fromEffect . return) [n..n+value]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

{-# INLINE sourceConcatMapId #-}
sourceConcatMapId :: (P.IsStream t, Monad m)
    => Int -> Int -> t m (t m Int)
sourceConcatMapId value n =
    P.fromFoldable $ fmap (P.fromEffect . return) [n..n+value]

-- concatMapWith

{-# INLINE concatStreamsWith #-}
concatStreamsWith
    :: (S.Stream IO Int -> S.Stream IO Int -> S.Stream IO Int)
    -> Int
    -> Int
    -> Int
    -> IO ()
concatStreamsWith op outer inner n =
    P.drain $ S.concatMapWith op
        (P.fromSerial . sourceUnfoldrM inner)
        (P.fromSerial $ sourceUnfoldrM outer n)

{-# INLINE concatPairsWith #-}
concatPairsWith
    :: (S.Stream IO Int -> S.Stream IO Int -> S.Stream IO Int)
    -> Int
    -> Int
    -> Int
    -> IO ()
concatPairsWith op outer inner n =
    P.drain $ Internal.concatPairsWith op
        (P.fromSerial . sourceUnfoldrM inner)
        (P.fromSerial $ sourceUnfoldrM outer n)

-------------------------------------------------------------------------------
-- Monadic outer product
-------------------------------------------------------------------------------

{-# INLINE runToList #-}
runToList :: Monad m => S.Stream m a -> m [a]
runToList = P.toList

{-# INLINE apDiscardFst #-}
apDiscardFst
    :: (P.IsStream t, P.MonadAsync m, Applicative (t m))
    => Int -> (t m Int -> S.Stream m Int) -> Int -> m ()
apDiscardFst linearCount t start = P.drain . t $
    P.fromSerial (sourceUnfoldrM nestedCount2 start)
        *> P.fromSerial (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE apDiscardSnd #-}
apDiscardSnd
    :: (P.IsStream t, P.MonadAsync m, Applicative (t m))
    => Int -> (t m Int -> S.Stream m Int) -> Int -> m ()
apDiscardSnd linearCount t start = P.drain . t $
    P.fromSerial (sourceUnfoldrM nestedCount2 start)
        <* P.fromSerial (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE apLiftA2 #-}
apLiftA2
    :: (P.IsStream t, P.MonadAsync m, Applicative (t m))
    => Int -> (t m Int -> S.Stream m Int) -> Int -> m ()
apLiftA2 linearCount t start = P.drain . t $
    liftA2 (+) (P.fromSerial (sourceUnfoldrM nestedCount2 start))
        (P.fromSerial (sourceUnfoldrM nestedCount2 start))

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullAp #-}
toNullAp
    :: (P.IsStream t, P.MonadAsync m, Applicative (t m))
    => Int -> (t m Int -> S.Stream m Int) -> Int -> m ()
toNullAp linearCount t start = P.drain . t $
    (+) <$> P.fromSerial (sourceUnfoldrM nestedCount2 start)
        <*> P.fromSerial (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE monadThen #-}
monadThen
    :: (P.IsStream t, P.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.Stream m Int) -> Int -> m ()
monadThen linearCount t start = P.drain . t $ do
    P.fromSerial (sourceUnfoldrM nestedCount2 start) >>
        P.fromSerial (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullM #-}
toNullM
    :: (P.IsStream t, P.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.Stream m Int) -> Int -> m ()
toNullM linearCount t start = P.drain . t $ do
    x <- P.fromSerial $ sourceUnfoldrM nestedCount2 start
    y <- P.fromSerial $ sourceUnfoldrM nestedCount2 start
    return $ x + y

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullM3 #-}
toNullM3
    :: (P.IsStream t, P.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.Stream m Int) -> Int -> m ()
toNullM3 linearCount t start = P.drain . t $ do
    x <- P.fromSerial $ sourceUnfoldrM nestedCount3 start
    y <- P.fromSerial $ sourceUnfoldrM nestedCount3 start
    z <- P.fromSerial $ sourceUnfoldrM nestedCount3 start
    return $ x + y + z
  where
    nestedCount3 = round (fromIntegral linearCount**(1/3::Double))

{-# INLINE toListM #-}
toListM
    :: (P.IsStream t, P.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.Stream m Int) -> Int -> m [Int]
toListM linearCount t start = runToList . t $ do
    x <- P.fromSerial $ sourceUnfoldrM nestedCount2 start
    y <- P.fromSerial $ sourceUnfoldrM nestedCount2 start
    return $ x + y
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

-- Taking a specified number of elements is very expensive in logict so we have
-- a test to measure the same.
{-# INLINE toListSome #-}
toListSome
    :: (P.IsStream t, P.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.Stream m Int) -> Int -> m [Int]
toListSome linearCount t start =
    runToList . t $ P.take 10000 $ do
        x <- P.fromSerial $ sourceUnfoldrM nestedCount2 start
        y <- P.fromSerial $ sourceUnfoldrM nestedCount2 start
        return $ x + y
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE filterAllOutM #-}
filterAllOutM
    :: (P.IsStream t, P.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.Stream m Int) -> Int -> m ()
filterAllOutM linearCount t start = P.drain . t $ do
    x <- P.fromSerial $ sourceUnfoldrM nestedCount2 start
    y <- P.fromSerial $ sourceUnfoldrM nestedCount2 start
    let s = x + y
    if s < 0
    then return s
    else P.nil
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE filterAllInM #-}
filterAllInM
    :: (P.IsStream t, P.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.Stream m Int) -> Int -> m ()
filterAllInM linearCount t start = P.drain . t $ do
    x <- P.fromSerial $ sourceUnfoldrM nestedCount2 start
    y <- P.fromSerial $ sourceUnfoldrM nestedCount2 start
    let s = x + y
    if s > 0
    then return s
    else P.nil
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE filterSome #-}
filterSome
    :: (P.IsStream t, P.MonadAsync m, Monad (t m))
    => Int -> (t m Int -> S.Stream m Int) -> Int -> m ()
filterSome linearCount t start = P.drain . t $ do
    x <- P.fromSerial $ sourceUnfoldrM nestedCount2 start
    y <- P.fromSerial $ sourceUnfoldrM nestedCount2 start
    let s = x + y
    if s > 1100000
    then return s
    else P.nil
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE breakAfterSome #-}
breakAfterSome
    :: (P.IsStream t, Monad (t IO))
    => Int -> (t IO Int -> S.Stream IO Int) -> Int -> IO ()
breakAfterSome linearCount t start = do
    (_ :: Either ErrorCall ()) <- try $ P.drain . t $ do
        x <- P.fromSerial $ sourceUnfoldrM nestedCount2 start
        y <- P.fromSerial $ sourceUnfoldrM nestedCount2 start
        let s = x + y
        if s > 1100000
        then error "break"
        else return s
    return ()
  where
    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))
