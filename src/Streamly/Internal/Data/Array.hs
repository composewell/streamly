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
import Control.Monad.ST (stToIO, RealWorld)
import Control.Monad.IO.Class (liftIO, MonadIO)
import GHC.ST (ST(..))
import GHC.IO (unsafePerformIO)
import GHC.Base (Int(..))
import Data.Functor.Identity (runIdentity)
import qualified GHC.Exts as Exts

import Streamly.Internal.Data.Fold.Types (Fold(..))

import qualified Streamly.Streams.StreamD as D

data Array a =
    Array
        { array#   :: Exts.Array# a
        }

{-# NOINLINE bottomElement #-}
bottomElement :: a
bottomElement = undefined

data MutableArray s a =
    MutableArray
        { marray# :: Exts.MutableArray# s a
        }

{-# INLINE newArray #-}
newArray :: Int -> a -> ST s (MutableArray s a)
newArray (I# n#) x = ST
   (\s# -> case Exts.newArray# n# x s# of
             (# s'#, marr# #) ->
               let ma = MutableArray marr#
               in (# s'# , ma #))

{-# INLINE writeArray #-}
writeArray :: MutableArray s a -> Int -> a -> ST s ()
writeArray marr (I# i#) x =
    ST (\s# ->
            case Exts.writeArray# (marray# marr) i# x s# of
                s'# -> (# s'#, () #))

{-# INLINE freezeArray #-}
freezeArray :: MutableArray s a -> Int -> Int -> ST s (Array a)
freezeArray marr (I# start#) (I# len#) =
    ST (\s# ->
            case Exts.freezeArray# (marray# marr) start# len# s# of
                (# s'#, arr# #) -> (# s'#, Array arr# #))

{-# INLINE copyMutableArray #-}
copyMutableArray :: MutableArray s a
                 -- ^ The source array
                 -> Int
                 -- ^ Offset into the source array
                 -> MutableArray s a
                 -- ^ Destination array to copy to
                 -> Int
                 -- ^ Offset into the destination array
                 -> Int
                 -- ^ Number of elements to copy
                 -> ST s ()
copyMutableArray sourceMarr (I# sourceOffset#) destMarr (I# destOffset#) (I# numElements#) =
    ST (\s# ->
            case Exts.copyMutableArray#
                     (marray# sourceMarr)
                     sourceOffset#
                     (marray# destMarr)
                     destOffset#
                     numElements#
                     s# of
                s'# -> (# s'#, () #))

{-# INLINE newArrayIO #-}
newArrayIO :: Int -> a -> IO (MutableArray RealWorld a)
newArrayIO i x = stToIO $ newArray i x

{-# INLINE writeArrayIO #-}
writeArrayIO :: MutableArray RealWorld a -> Int -> a -> IO ()
writeArrayIO marr i x = stToIO $ writeArray marr i x

{-# INLINE freezeArrayIO #-}
freezeArrayIO :: MutableArray RealWorld a -> Int -> Int -> IO (Array a)
freezeArrayIO marr i len = stToIO $ freezeArray marr i len

{-# INLINE length #-}
length :: Array a -> Int
length arr = I# (Exts.sizeofArray# (array# arr))

{-# INLINE copyMutableArrayIO #-}
copyMutableArrayIO ::
       MutableArray RealWorld a
    -> Int
    -> MutableArray RealWorld a
    -> Int
    -> Int
    -> IO ()
copyMutableArrayIO sourceMarr sourceOffset destMarr destOffset numElements =
    stToIO $
    copyMutableArray sourceMarr sourceOffset destMarr destOffset numElements

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
        marr <- liftIO $ newArrayIO limit bottomElement
        return (marr, 0)
    step (marr, i) x
        | i == limit = return (marr, i)
        | otherwise = do
            liftIO $ writeArrayIO marr i x
            return (marr, i + 1)
    extract (marr, len) = liftIO $ freezeArrayIO marr 0 len

{-# INLINE_NORMAL write #-}
write :: MonadIO m => Fold m a (Array a)
write = Fold step initial extract
  where
    initial = do
        marr <- liftIO $ newArrayIO 0 bottomElement
        return (marr, 0, 0)
    step (marr, i, capacity) x
        | i == capacity =
            let newCapacity = max (capacity * 2) 1
             in do newMarr <- liftIO $ newArrayIO newCapacity bottomElement
                   liftIO $ copyMutableArrayIO marr 0 newMarr 0 i
                   liftIO $ writeArrayIO newMarr i x
                   return (newMarr, i + 1, newCapacity)
        | otherwise = do
            liftIO $ writeArrayIO marr i x
            return (marr, i + 1, capacity)
    extract (marr, len, _) = liftIO $ freezeArrayIO marr 0 len

{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN :: MonadIO m => Int -> D.Stream m a -> m (Array a)
fromStreamDN limit str = do
    marr <- liftIO $ newArrayIO (max limit 0) bottomElement
    i <-
        D.foldlM'
            (\i x -> i `seq` (liftIO $ writeArrayIO marr i x) >> return (i + 1))
            0 $
        D.take limit str
    liftIO $ freezeArrayIO marr 0 i

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
