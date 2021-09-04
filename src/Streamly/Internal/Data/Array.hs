{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE UnboxedTuples #-}
#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Array
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
module Streamly.Internal.Data.Array
    ( Array(..)

    -- * Construction
    , writeN
    , write

    , fromStreamDN
    , fromStreamD
    , fromStreamN
    , fromStream

    , fromListN
    , fromList

    -- * Elimination
    , length
    , read

    , toStreamD
    , toStreamDRev
    , toStream
    , toStreamRev

    , foldl'
    , foldr
    , streamFold
    , fold

    -- * Random Access
    , getIndexUnsafe
    )
where

#if !MIN_VERSION_primitive(0,7,1)
import Control.DeepSeq (NFData(..))
#endif
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Functor.Identity (runIdentity)
import GHC.Base (Int(..))
import GHC.IO (unsafePerformIO)

import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Stream.Serial (SerialT(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import qualified GHC.Exts as Exts
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Stream.StreamD as D

import Data.Primitive.Array hiding (fromList, fromListN)
import Prelude hiding (foldr, length, read)

{-# NOINLINE bottomElement #-}
bottomElement :: a
bottomElement = undefined

-------------------------------------------------------------------------------
-- Construction - Folds
-------------------------------------------------------------------------------

-- writeN n = S.evertM (fromStreamDN n)
{-# INLINE_NORMAL writeN #-}
writeN :: MonadIO m => Int -> Fold m a (Array a)
writeN limit = Fold step initial extract
  where
    initial = do
        marr <- liftIO $ newArray limit bottomElement
        return $ FL.Partial (Tuple' marr 0)
    step st@(Tuple' marr i) x
        | i == limit = fmap FL.Done $ extract st
        | otherwise = do
            liftIO $ writeArray marr i x
            return $ FL.Partial $ Tuple' marr (i + 1)
    extract (Tuple' marr len) = liftIO $ freezeArray marr 0 len

{-# INLINE_NORMAL write #-}
write :: MonadIO m => Fold m a (Array a)
write = Fold step initial extract
  where
    initial = do
        marr <- liftIO $ newArray 0 bottomElement
        return $ FL.Partial (Tuple3' marr 0 0)
    step (Tuple3' marr i capacity) x
        | i == capacity =
            let newCapacity = max (capacity * 2) 1
             in do newMarr <- liftIO $ newArray newCapacity bottomElement
                   liftIO $ copyMutableArray newMarr 0 marr 0 i
                   liftIO $ writeArray newMarr i x
                   return $ FL.Partial $ Tuple3' newMarr (i + 1) newCapacity
        | otherwise = do
            liftIO $ writeArray marr i x
            return $ FL.Partial $ Tuple3' marr (i + 1) capacity
    extract (Tuple3' marr len _) = liftIO $ freezeArray marr 0 len

-------------------------------------------------------------------------------
-- Construction - from streams
-------------------------------------------------------------------------------

{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN :: MonadIO m => Int -> D.Stream m a -> m (Array a)
fromStreamDN limit str = do
    marr <- liftIO $ newArray (max limit 0) bottomElement
    i <-
        D.foldlM'
            (\i x -> i `seq` liftIO $ writeArray marr i x >> return (i + 1))
            (return 0) $
        D.take limit str
    liftIO $ freezeArray marr 0 i

{-# INLINE fromStreamD #-}
fromStreamD :: MonadIO m => D.Stream m a -> m (Array a)
fromStreamD = D.fold write

{-# INLINE fromStreamN #-}
fromStreamN :: MonadIO m => Int -> SerialT m a -> m (Array a)
fromStreamN n (SerialT m) = do
    when (n < 0) $ error "fromStreamN: negative write count specified"
    fromStreamDN n $ D.fromStreamK m

{-# INLINE fromStream #-}
fromStream :: MonadIO m => SerialT m a -> m (Array a)
fromStream (SerialT m) = fromStreamD $ D.fromStreamK m

{-# INLINABLE fromListN #-}
fromListN :: Int -> [a] -> Array a
fromListN n xs = unsafePerformIO $ fromStreamDN n $ D.fromList xs

{-# INLINABLE fromList #-}
fromList :: [a] -> Array a
fromList xs = unsafePerformIO $ fromStreamD $ D.fromList xs

-------------------------------------------------------------------------------
-- Elimination - Unfolds
-------------------------------------------------------------------------------

{-# INLINE length #-}
length :: Array a -> Int
length = sizeofArray

{-# INLINE_NORMAL read #-}
read :: Monad m => Unfold m (Array a) a
read = Unfold step inject
  where
    inject arr = return (arr, 0)
    step (arr, i)
        | i == length arr = return D.Stop
    step (arr, I# i) =
        return $
        case Exts.indexArray# (array# arr) i of
            (# x #) -> D.Yield x (arr, I# i + 1)

-------------------------------------------------------------------------------
-- Elimination - to streams
-------------------------------------------------------------------------------

{-# INLINE_NORMAL toStreamD #-}
toStreamD :: Monad m => Array a -> D.Stream m a
toStreamD arr = D.Stream step 0
  where
    {-# INLINE_LATE step #-}
    step _ i
        | i == length arr = return D.Stop
    step _ (I# i) =
        return $
        case Exts.indexArray# (array# arr) i of
            (# x #) -> D.Yield x (I# i + 1)

{-# INLINE_NORMAL toStreamDRev #-}
toStreamDRev :: Monad m => Array a -> D.Stream m a
toStreamDRev arr = D.Stream step (length arr - 1)
  where
    {-# INLINE_LATE step #-}
    step _ i
        | i < 0 = return D.Stop
    step _ (I# i) =
        return $
        case Exts.indexArray# (array# arr) i of
            (# x #) -> D.Yield x (I# i - 1)

{-# INLINE_EARLY toStream #-}
toStream :: Monad m => Array a -> SerialT m a
toStream = SerialT . D.toStreamK . toStreamD

{-# INLINE_EARLY toStreamRev #-}
toStreamRev :: Monad m => Array a -> SerialT m a
toStreamRev = SerialT . D.toStreamK . toStreamDRev

-------------------------------------------------------------------------------
-- Elimination - using Folds
-------------------------------------------------------------------------------

{-# INLINE_NORMAL foldl' #-}
foldl' :: (b -> a -> b) -> b -> Array a -> b
foldl' f z arr = runIdentity $ D.foldl' f z $ toStreamD arr

{-# INLINE_NORMAL foldr #-}
foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f z arr = runIdentity $ D.foldr f z $ toStreamD arr

#if !MIN_VERSION_primitive(0,7,1)
instance NFData a => NFData (Array a) where
    {-# INLINE rnf #-}
    rnf = foldl' (\_ x -> rnf x) ()
#endif

{-# INLINE fold #-}
fold :: Monad m => Fold m a b -> Array a -> m b
fold f arr = D.fold f (toStreamD arr)

{-# INLINE streamFold #-}
streamFold :: Monad m => (SerialT m a -> m b) -> Array a -> m b
streamFold f arr = f (toStream arr)

-------------------------------------------------------------------------------
-- Random reads and writes
-------------------------------------------------------------------------------

-- | /O(1)/ Lookup the element at the given index. Index starts from 0. Does
-- not check the bounds.
--
-- @since 0.8.0
{-# INLINE getIndexUnsafe #-}
getIndexUnsafe :: Array a -> Int -> a
getIndexUnsafe = indexArray
