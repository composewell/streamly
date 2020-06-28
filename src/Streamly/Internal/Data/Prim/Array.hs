{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Prim.Array
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Prim.Array
    (

    -- XXX should it be just Array instead? We should be able to replace one
    -- array type with another easily.
      PrimArray(..)

    -- XXX Prim should be exported from Data.Prim module?
    , Prim(..)

    , foldl'
    , foldr

    , length

    , writeN
    , write

    , toStreamD
    , toStreamDRev

    , toStream
    , toStreamRev
    , read
    , readSlice

    , fromListN
    , fromList
    , fromStreamDN
    , fromStreamD

    , fromStreamN
    , fromStream

    , streamFold
    , fold
    )
where

import Prelude hiding (foldr, length, read)
import Control.DeepSeq (NFData(..))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import GHC.IO (unsafePerformIO)
import Data.Primitive.Types (Prim(..))

import Streamly.Internal.Data.Prim.Array.Types
import Streamly.Internal.Data.Unfold.Types (Unfold(..))
import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)
import Streamly.Internal.Data.Stream.Serial (SerialT)

import qualified Streamly.Internal.Data.Stream.StreamD as D

{-# INLINE_NORMAL toStreamD #-}
toStreamD :: (Prim a, Monad m) => PrimArray a -> D.Stream m a
toStreamD arr = D.Stream step 0
  where
    {-# INLINE_LATE step #-}
    step _ i
        | i == sizeofPrimArray arr = return D.Stop
    step _ i = return $ D.Yield (indexPrimArray arr i) (i + 1)

{-# INLINE length #-}
length :: Prim a => PrimArray a -> Int
length = sizeofPrimArray

{-# INLINE_NORMAL toStreamDRev #-}
toStreamDRev :: (Prim a, Monad m) => PrimArray a -> D.Stream m a
toStreamDRev arr = D.Stream step (sizeofPrimArray arr - 1)
  where
    {-# INLINE_LATE step #-}
    step _ i
        | i < 0 = return D.Stop
    step _ i = return $ D.Yield (indexPrimArray arr i) (i - 1)

{-# INLINE_NORMAL foldl' #-}
foldl' :: Prim a => (b -> a -> b) -> b -> PrimArray a -> b
foldl' = foldlPrimArray'

{-# INLINE_NORMAL foldr #-}
foldr :: Prim a => (a -> b -> b) -> b -> PrimArray a -> b
foldr = foldrPrimArray

-- writeN n = S.evertM (fromStreamDN n)
{-# INLINE_NORMAL writeN #-}
writeN :: (MonadIO m, Prim a) => Int -> Fold m a (PrimArray a)
writeN limit = Fold step initial extract
  where
    initial = do
        marr <- liftIO $ newPrimArray limit
        return (marr, 0)
    step (marr, i) x
        | i == limit = return (marr, i)
        | otherwise = do
            liftIO $ writePrimArray marr i x
            return (marr, i + 1)
    extract (marr, _) = liftIO $ unsafeFreezePrimArray marr

{-# INLINE_NORMAL write #-}
write :: (MonadIO m, Prim a) => Fold m a (PrimArray a)
write = Fold step initial extract
  where
    initial = do
        marr <- liftIO $ newPrimArray 0
        return (marr, 0, 0)
    step (marr, i, capacity) x
        | i == capacity =
            let newCapacity = max (capacity * 2) 1
             in do newMarr <- liftIO $ resizeMutablePrimArray marr newCapacity
                   liftIO $ writePrimArray newMarr i x
                   return (newMarr, i + 1, newCapacity)
        | otherwise = do
            liftIO $ writePrimArray marr i x
            return (marr, i + 1, capacity)
    extract (marr, len, _) = do liftIO $ shrinkMutablePrimArray marr len
                                liftIO $ unsafeFreezePrimArray marr

{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN :: (MonadIO m, Prim a) => Int -> D.Stream m a -> m (PrimArray a)
fromStreamDN limit str = do
    marr <- liftIO $ newPrimArray (max limit 0)
    _ <-
        D.foldlM'
            (\i x -> i `seq` liftIO (writePrimArray marr i x) >> return (i + 1))
            0 $
        D.take limit str
    liftIO $ unsafeFreezePrimArray marr

{-# INLINE fromStreamD #-}
fromStreamD :: (MonadIO m, Prim a) => D.Stream m a -> m (PrimArray a)
fromStreamD = D.runFold write

{-# INLINABLE fromListN #-}
fromListN :: Prim a => Int -> [a] -> PrimArray a
fromListN n xs = unsafePerformIO $ fromStreamDN n $ D.fromList xs

{-# INLINABLE fromList #-}
fromList :: Prim a => [a] -> PrimArray a
fromList xs = unsafePerformIO $ fromStreamD $ D.fromList xs

instance Prim a => NFData (PrimArray a) where
    {-# INLINE rnf #-}
    rnf = foldl' (\_ _ -> ()) ()

{-# INLINE fromStreamN #-}
fromStreamN :: (MonadIO m, Prim a) => Int -> SerialT m a -> m (PrimArray a)
fromStreamN n m = do
    when (n < 0) $ error "fromStreamN: negative write count specified"
    fromStreamDN n $ D.toStreamD m

{-# INLINE fromStream #-}
fromStream :: (MonadIO m, Prim a) => SerialT m a -> m (PrimArray a)
fromStream m = fromStreamD $ D.toStreamD m

{-# INLINE_EARLY toStream #-}
toStream :: (Prim a, Monad m, IsStream t) => PrimArray a -> t m a
toStream = D.fromStreamD . toStreamD

{-# INLINE_EARLY toStreamRev #-}
toStreamRev :: (Prim a, Monad m, IsStream t) => PrimArray a -> t m a
toStreamRev = D.fromStreamD . toStreamDRev

{-# INLINE fold #-}
fold :: (Prim a, Monad m) => Fold m a b -> PrimArray a -> m b
fold f arr = D.runFold f (toStreamD arr)

{-# INLINE streamFold #-}
streamFold :: (Prim a, Monad m) => (SerialT m a -> m b) -> PrimArray a -> m b
streamFold f arr = f (toStream arr)

{-# INLINE_NORMAL read #-}
read :: (Prim a, Monad m) => Unfold m (PrimArray a) a
read = Unfold step inject
  where
    inject arr = return (arr, 0)
    step (arr, i)
        | i == length arr = return D.Stop
    step (arr, i) = return $ D.Yield (indexPrimArray arr i) (arr, i + 1)

{-# INLINE_NORMAL readSlice #-}
readSlice :: (Prim a, Monad m) => Int -> Int -> Unfold m (PrimArray a) a
readSlice off len = Unfold step inject
  where
    inject arr = return (arr, off)
    step (arr, i)
        | i == min (off + len) (length arr) = return D.Stop
    step (arr, i) = return $ D.Yield (indexPrimArray arr i) (arr, i + 1)
