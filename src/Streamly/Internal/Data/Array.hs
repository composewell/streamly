{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP             #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE UnboxedTuples   #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Array
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Array
    ( Array(..)

    , foldl'
    , foldr

    , length

    , writeN
    , write

    , toStreamD
    , toStreamDRev

    , fromListN
    , fromList
    , fromStreamDN
    , fromStreamD
    )
where

import Prelude hiding (foldr, length)
import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (liftIO, MonadIO)
import GHC.IO (unsafePerformIO)
import GHC.Base (Int(..))
import Data.Functor.Identity (runIdentity)
import Data.Primitive.Array hiding (fromList, fromListN)
import qualified GHC.Exts as Exts

import Streamly.Internal.Data.Fold.Types (Fold(..))

import qualified Streamly.Streams.StreamD as D

{-# NOINLINE bottomElement #-}
bottomElement :: a
bottomElement = undefined

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
            (# x #) -> D.Yield x ((I# i) + 1)

{-# INLINE length #-}
length :: Array a -> Int
length arr = sizeofArray arr

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
            (# x #) -> D.Yield x ((I# i) - 1)

{-# INLINE_NORMAL foldl' #-}
foldl' :: (b -> a -> b) -> b -> Array a -> b
foldl' f z arr = runIdentity $ D.foldl' f z $ toStreamD arr

{-# INLINE_NORMAL foldr #-}
foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f z arr = runIdentity $ D.foldr f z $ toStreamD arr

-- writeN n = S.evertM (fromStreamDN n)
{-# INLINE_NORMAL writeN #-}
writeN :: MonadIO m => Int -> Fold m a (Array a)
writeN limit = Fold step initial extract
  where
    initial = do
        marr <- liftIO $ newArray limit bottomElement
        return (marr, 0)
    step (marr, i) x
        | i == limit = return (marr, i)
        | otherwise = do
            liftIO $ writeArray marr i x
            return (marr, i + 1)
    extract (marr, len) = liftIO $ freezeArray marr 0 len

{-# INLINE_NORMAL write #-}
write :: MonadIO m => Fold m a (Array a)
write = Fold step initial extract
  where
    initial = do
        marr <- liftIO $ newArray 0 bottomElement
        return (marr, 0, 0)
    step (marr, i, capacity) x
        | i == capacity =
            let newCapacity = max (capacity * 2) 1
             in do newMarr <- liftIO $ newArray newCapacity bottomElement
                   liftIO $ copyMutableArray newMarr 0 marr 0 i
                   liftIO $ writeArray newMarr i x
                   return (newMarr, i + 1, newCapacity)
        | otherwise = do
            liftIO $ writeArray marr i x
            return (marr, i + 1, capacity)
    extract (marr, len, _) = liftIO $ freezeArray marr 0 len

{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN :: MonadIO m => Int -> D.Stream m a -> m (Array a)
fromStreamDN limit str = do
    marr <- liftIO $ newArray (max limit 0) bottomElement
    i <-
        D.foldlM'
            (\i x -> i `seq` (liftIO $ writeArray marr i x) >> return (i + 1))
            0 $
        D.take limit str
    liftIO $ freezeArray marr 0 i

{-# INLINE fromStreamD #-}
fromStreamD :: MonadIO m => D.Stream m a -> m (Array a)
fromStreamD str = D.runFold write str

{-# INLINABLE fromListN #-}
fromListN :: Int -> [a] -> Array a
fromListN n xs = unsafePerformIO $ fromStreamDN n $ D.fromList xs

{-# INLINABLE fromList #-}
fromList :: [a] -> Array a
fromList xs = unsafePerformIO $ fromStreamD $ D.fromList xs

instance NFData a => NFData (Array a) where
    {-# INLINE rnf #-}
    rnf = foldl' (\_ x -> rnf x) ()
