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
    , nil
    , writeN
    , write
    , writeLastN

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
    , strip
    , putIndices
    )
where

#if !MIN_VERSION_primitive(0,7,1)
import Control.DeepSeq (NFData(..))
#endif
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Primitive (PrimMonad)
import Data.Functor.Identity (runIdentity)
import Data.IORef
import GHC.Base (Int(..))
import GHC.IO (unsafePerformIO)

import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import qualified GHC.Exts as Exts
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Ring as RB
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.Type as Stream

import Data.Primitive.Array hiding (fromList, fromListN)
import Prelude hiding (foldr, length, read)

{-# NOINLINE bottomElement #-}
bottomElement :: a
bottomElement = undefined

{-# NOINLINE nil #-}
nil :: Array a
nil = unsafePerformIO $ do
    marr <- liftIO $ newArray 0 bottomElement
    liftIO $ freezeArray marr 0 0

-------------------------------------------------------------------------------
-- Construction - Folds
-------------------------------------------------------------------------------

{-# INLINE_NORMAL writeN #-}
writeN :: MonadIO m => Int -> Fold m a (Array a)
writeN len = Fold step initial extract

    where

    {-# INLINE next #-}
    next marr i = do
        let i1 = i + 1
            st = Tuple' marr i1
        if len > i1
        then return $ FL.Partial st
        else fmap FL.Done $ extract st

    initial = do
        marr <- liftIO $ newArray len bottomElement
        next marr (-1)

    step (Tuple' marr i) x = do
        liftIO $ writeArray marr i x
        next marr i

    extract (Tuple' marr l) = liftIO $ freezeArray marr 0 l

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
fromStreamN n m = do
    when (n < 0) $ error "fromStreamN: negative write count specified"
    fromStreamDN n $ Stream.toStreamD m

{-# INLINE fromStream #-}
fromStream :: MonadIO m => SerialT m a -> m (Array a)
fromStream = fromStreamD . Stream.toStreamD

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
toStream = Stream.fromStreamD . toStreamD

{-# INLINE_EARLY toStreamRev #-}
toStreamRev :: Monad m => Array a -> SerialT m a
toStreamRev = Stream.fromStreamD . toStreamDRev

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

{-# INLINE writeLastN #-}
writeLastN :: MonadIO m => Int -> Fold m a (Array a)
writeLastN n
    | n <= 0 = fmap (const mempty) FL.drain
    | otherwise = Fold step initial done

    where

    initial = do
        rb <- liftIO $ RB.createRing n
        return $ FL.Partial $ Tuple' rb (0 :: Int)

    step (Tuple' rb rh) x = do
        liftIO $ RB.unsafeInsertRing rb rh x
        return $ FL.Partial $ Tuple' rb (rh + 1)

    done (Tuple' rb rh) = do
        arr' <- liftIO $ newArray (min rh n) (undefined :: a)
        ref <- liftIO $ readIORef $ RB.ringHead rb
        if rh < n
        then
            liftIO $ copyMutableArray arr' 0 (RB.arr rb) 0 ref
        else do
            liftIO $ copyMutableArray arr' 0 (RB.arr rb) ref (n - ref)
            liftIO $ copyMutableArray arr' (n - ref) (RB.arr rb) 0 ref
        liftIO $ unsafeFreezeArray arr'

-- XXX This is not efficient as it copies the array. We should support array
-- slicing so that we can just refer to the underlying array memory instead of
-- copying.
--
-- | Truncate the array at the beginning and end as long as the predicate
-- holds true.
strip :: (a -> Bool) -> Array a -> Array a
strip p arr =
    let lastIndex = length arr - 1
        indexR = getIndexR lastIndex -- last predicate failing index
    in if indexR == -1
       then nil
       else
            let indexL = getIndexL 0 -- first predicate failing index
            in if indexL == 0 && indexR == lastIndex
               then arr
               else cloneArray arr indexL (indexR - indexL + 1)

    where

    getIndexR idx
        | idx < 0 = idx
        | otherwise =
            if p (indexArray arr idx) then getIndexR (idx - 1) else idx

    getIndexL idx = if p (indexArray arr idx) then getIndexL (idx + 1) else idx

-- | Write an input stream of (index, value) pairs to an array. Throws an
-- error if any index is out of bounds.
--
-- /Pre-release/
{-# INLINE putIndices #-}
putIndices :: PrimMonad m
    => Array a -> Fold m (Int, a) ()
putIndices arr = FL.rmapM (\ _ -> return ()) (FL.foldlM' step initial)

    where

    initial =  unsafeThawArray arr

    step marr (i, x) = do
        writeArray marr i x
        return marr
