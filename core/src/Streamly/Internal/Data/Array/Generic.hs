-- |
-- Module      : Streamly.Internal.Data.Array.Generic
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
module Streamly.Internal.Data.Array.Generic
    ( Array(..)

    -- * Construction
    , nil
    , writeN
    , write
    , writeWith
    , writeLastN

    , fromStreamN
    , fromStream
    , fromPureStream
    , fromByteStr#

    , fromListN
    , fromList

    , chunksOf

    -- * Elimination
    , length
    , reader

    , toList
    , read
    , readRev

    , foldl'
    , foldr
    , streamFold
    , fold

    -- * Random Access
    , getIndexUnsafe
    , getIndex
    , getSliceUnsafe
    , strip
    )
where

#include "inline.hs"

import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor.Identity (Identity(..))
import Data.Word (Word8)
import GHC.Base (MutableArray#, RealWorld)
import GHC.Exts (Addr#)
import GHC.IO (unsafePerformIO)
import Text.Read (readPrec)

import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Stream.StreamD.Type (Stream)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.System.IO (unsafeInlineIO)

import qualified Streamly.Internal.Data.MutArray.Generic as MArray
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Producer.Type as Producer
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Ring as RB
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamD.Generate as D
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

import Prelude hiding (Foldable(..), read)

-------------------------------------------------------------------------------
-- Array Data Type
-------------------------------------------------------------------------------

data Array a =
    Array
        { arrContents# :: MutableArray# RealWorld a
          -- ^ The internal contents of the array representing the entire array.

        , arrStart :: {-# UNPACK #-}!Int
          -- ^ The starting index of this slice.

        , arrLen :: {-# UNPACK #-}!Int
          -- ^ The length of this slice.
        }

unsafeFreeze :: MArray.MutArray a -> Array a
unsafeFreeze (MArray.MutArray cont# arrS arrL _) = Array cont# arrS arrL

unsafeThaw :: Array a -> MArray.MutArray a
unsafeThaw (Array cont# arrS arrL) = MArray.MutArray cont# arrS arrL arrL

{-# NOINLINE nil #-}
nil :: Array a
nil = unsafePerformIO $ unsafeFreeze <$> MArray.nil

-------------------------------------------------------------------------------
-- Construction - Folds
-------------------------------------------------------------------------------

{-# INLINE_NORMAL writeN #-}
writeN :: MonadIO m => Int -> Fold m a (Array a)
writeN = fmap unsafeFreeze <$> MArray.writeN

{-# INLINE_NORMAL writeWith #-}
writeWith :: MonadIO m => Int -> Fold m a (Array a)
writeWith elemCount = unsafeFreeze <$> MArray.writeWith elemCount

-- | Fold the whole input to a single array.
--
-- /Caution! Do not use this on infinite streams./
--
{-# INLINE write #-}
write :: MonadIO m => Fold m a (Array a)
write = fmap unsafeFreeze MArray.write

fromPureStream :: Stream Identity a -> Array a
fromPureStream x =
    unsafePerformIO $ fmap (unsafeFreeze) (MArray.fromPureStream x)
-- fromPureStream = runIdentity . D.fold (unsafeMakePure write)
-- fromPureStream = fromList . runIdentity . D.toList

fromByteStr# :: Addr# -> Array Word8
fromByteStr# addr = fromPureStream (D.fromByteStr# addr)

-------------------------------------------------------------------------------
-- Stream Ops
-------------------------------------------------------------------------------

{-# INLINE_NORMAL chunksOf #-}
chunksOf :: forall m a. MonadIO m
    => Int -> Stream m a -> Stream m (Array a)
chunksOf n strm = fmap unsafeFreeze $ MArray.chunksOf n strm

-------------------------------------------------------------------------------
-- Construction - from streams
-------------------------------------------------------------------------------

{-# INLINE fromStreamN #-}
fromStreamN :: MonadIO m => Int -> Stream m a -> m (Array a)
fromStreamN n = D.fold (writeN n)

{-# INLINE fromStream #-}
fromStream :: MonadIO m => Stream m a -> m (Array a)
fromStream = D.fold write

-- XXX Consider foldr/build fusion in toList/fromList

{-# INLINABLE fromListN #-}
fromListN :: Int -> [a] -> Array a
fromListN n xs = unsafePerformIO $ fromStreamN n $ D.fromList xs

{-# INLINABLE fromList #-}
fromList :: [a] -> Array a
fromList xs = unsafePerformIO $ fromStream $ D.fromList xs

-------------------------------------------------------------------------------
-- Elimination - Unfolds
-------------------------------------------------------------------------------

{-# INLINE length #-}
length :: Array a -> Int
length = arrLen

{-# INLINE_NORMAL reader #-}
reader :: Monad m => Unfold m (Array a) a
reader =
    Producer.simplify
        $ Producer.translate unsafeThaw unsafeFreeze
        $ MArray.producerWith (return . unsafeInlineIO)

-------------------------------------------------------------------------------
-- Elimination - to streams
-------------------------------------------------------------------------------

{-# INLINE_NORMAL toList #-}
toList :: Array a -> [a]
toList arr = loop 0

    where

    len = length arr
    loop i | i == len = []
    loop i = getIndexUnsafe i arr : loop (i + 1)

{-# INLINE_NORMAL read #-}
read :: Monad m => Array a -> Stream m a
read arr@Array{..} =
    D.map (`getIndexUnsafe` arr) $ D.enumerateFromToIntegral 0 (arrLen - 1)

{-# INLINE_NORMAL readRev #-}
readRev :: Monad m => Array a -> Stream m a
readRev arr@Array{..} =
    D.map (`getIndexUnsafe` arr)
        $ D.enumerateFromThenToIntegral (arrLen - 1) (arrLen - 2) 0

-------------------------------------------------------------------------------
-- Elimination - using Folds
-------------------------------------------------------------------------------

{-# INLINE_NORMAL foldl' #-}
foldl' :: (b -> a -> b) -> b -> Array a -> b
foldl' f z arr = unsafePerformIO $ D.foldl' f z $ read arr

{-# INLINE_NORMAL foldr #-}
foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f z arr = unsafePerformIO $ D.foldr f z $ read arr

{-# INLINE fold #-}
fold :: Monad m => Fold m a b -> Array a -> m b
fold f arr = D.fold f (read arr)

{-# INLINE streamFold #-}
streamFold :: Monad m => (Stream m a -> m b) -> Array a -> m b
streamFold f arr = f (read arr)

-------------------------------------------------------------------------------
-- Random reads and writes
-------------------------------------------------------------------------------

-- | /O(1)/ Lookup the element at the given index. Index starts from 0. Does
-- not check the bounds.
--
-- @since 0.8.0
{-# INLINE getIndexUnsafe #-}
getIndexUnsafe :: Int -> Array a -> a
getIndexUnsafe i arr =
    unsafePerformIO $ MArray.getIndexUnsafe i (unsafeThaw arr)

-- | Lookup the element at the given index. Index starts from 0.
--
{-# INLINE getIndex #-}
getIndex :: Int -> Array a -> Maybe a
getIndex i arr@Array {..} =
    if i >= 0 && i < arrLen
    then Just $ getIndexUnsafe i arr
    else Nothing

{-# INLINE writeLastN #-}
writeLastN :: MonadIO m => Int -> Fold m a (Array a)
writeLastN n = FL.rmapM f (RB.writeLastN n)

    where

    f rb = do
        arr <- RB.toMutArray 0 n rb
        return $ unsafeFreeze arr

{-# INLINE getSliceUnsafe #-}
getSliceUnsafe :: Int -> Int -> Array a -> Array a
getSliceUnsafe offset len (Array cont off1 _) = Array cont (off1 + offset) len

-- XXX This is not efficient as it copies the array. We should support array
-- slicing so that we can just refer to the underlying array memory instead of
-- copying.

-- | Truncate the array at the beginning and end as long as the predicate
-- holds true. Returns a slice of the original array.
{-# INLINE strip #-}
strip :: (a -> Bool) -> Array a -> Array a
strip p arr = unsafeFreeze $ unsafePerformIO $ MArray.strip p (unsafeThaw arr)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Eq a => Eq (Array a) where
    {-# INLINE (==) #-}
    arr1 == arr2 =
        unsafeInlineIO $! unsafeThaw arr1 `MArray.eq` unsafeThaw arr2

instance Ord a => Ord (Array a) where
    {-# INLINE compare #-}
    compare arr1 arr2 =
        unsafeInlineIO $! unsafeThaw arr1 `MArray.cmp` unsafeThaw arr2

    -- Default definitions defined in base do not have an INLINE on them, so we
    -- replicate them here with an INLINE.
    {-# INLINE (<) #-}
    x <  y = case compare x y of { LT -> True;  _ -> False }

    {-# INLINE (<=) #-}
    x <= y = case compare x y of { GT -> False; _ -> True }

    {-# INLINE (>) #-}
    x >  y = case compare x y of { GT -> True;  _ -> False }

    {-# INLINE (>=) #-}
    x >= y = case compare x y of { LT -> False; _ -> True }

    -- These two default methods use '<=' rather than 'compare'
    -- because the latter is often more expensive
    {-# INLINE max #-}
    max x y = if x <= y then y else x

    {-# INLINE min #-}
    min x y = if x <= y then x else y

instance Show a => Show (Array a) where
    {-# INLINE show #-}
    show arr = "fromList " ++ show (toList arr)

instance Read a => Read (Array a) where
    {-# INLINE readPrec #-}
    readPrec = do
        fromListWord <- replicateM 9 ReadPrec.get
        if fromListWord == "fromList "
        then fromList <$> readPrec
        else ReadPrec.pfail
