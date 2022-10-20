{-# OPTIONS_GHC -fno-warn-orphans #-}

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
    , reader

    , toList
    , readStreamD
    , readRevStreamD
    , read
    , readRev

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

import Control.DeepSeq (NFData(..))
import Control.Monad (when, replicateM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import GHC.Base (MutableArray#, RealWorld)
import GHC.IO (unsafePerformIO)
import Text.Read (readPrec)

import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import qualified Streamly.Internal.Data.Array.Mut.Type as MArray
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Ring as RB
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.Type as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

import Data.IORef
import Prelude hiding (foldr, length, read)

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

unsafeFreeze :: MArray.Array a -> Array a
unsafeFreeze (MArray.Array cont# arrS arrL _) = Array cont# arrS arrL

unsafeThaw :: Array a -> MArray.Array a
unsafeThaw (Array cont# arrS arrL) = MArray.Array cont# arrS arrL arrL

{-# NOINLINE nil #-}
nil :: Array a
nil = unsafePerformIO $ do
    marr <- MArray.newArray 0
    return $ unsafeFreeze marr

-------------------------------------------------------------------------------
-- Construction - Folds
-------------------------------------------------------------------------------

{-# INLINE_NORMAL writeN #-}
writeN :: MonadIO m => Int -> Fold m a (Array a)
writeN = fmap unsafeFreeze <$> MArray.writeN

{-# INLINE_NORMAL write #-}
write :: MonadIO m => Fold m a (Array a)
write = Fold step initial extract
  where
    initial = do
        marr <- liftIO $ MArray.newArray 0
        return $ FL.Partial (Tuple3' marr 0 0)
    step (Tuple3' marr i capacity) x
        | i == capacity =
            let newCapacity = max (capacity * 2) 1
             in do newMarr <- liftIO $ MArray.realloc newCapacity marr
                   marr1 <- liftIO $ MArray.snocUnsafe newMarr x
                   return $ FL.Partial $ Tuple3' marr1 (i + 1) newCapacity
        | otherwise = do
            marr1 <- liftIO $ MArray.snocUnsafe marr x
            return $ FL.Partial $ Tuple3' marr1 (i + 1) capacity
    extract (Tuple3' marr _ _) =
        return $ unsafeFreeze marr

-------------------------------------------------------------------------------
-- Construction - from streams
-------------------------------------------------------------------------------

{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN :: MonadIO m => Int -> D.Stream m a -> m (Array a)
fromStreamDN limit str = do
    marr <- liftIO $ MArray.newArray (max limit 0)
    i <-
        D.foldlM'
            (\i x -> i `seq` liftIO $ MArray.putIndexUnsafe marr i x >> return (i + 1))
            (return 0) $
        D.take limit str
    return $ unsafeFreeze $ marr { MArray.arrLen = i }

{-# INLINE fromStreamD #-}
fromStreamD :: MonadIO m => D.Stream m a -> m (Array a)
fromStreamD = D.fold write

{-# INLINE fromStreamN #-}
fromStreamN :: MonadIO m => Int -> Stream m a -> m (Array a)
fromStreamN n m = do
    when (n < 0) $ error "fromStreamN: negative write count specified"
    fromStreamDN n $ Stream.toStreamD m

{-# INLINE fromStream #-}
fromStream :: MonadIO m => Stream m a -> m (Array a)
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
length = arrLen

{-# INLINE_NORMAL reader #-}
reader :: MonadIO m => Unfold m (Array a) a
reader = Unfold.lmap unsafeThaw MArray.reader

-------------------------------------------------------------------------------
-- Elimination - to streams
-------------------------------------------------------------------------------

{-# INLINE_NORMAL toList #-}
toList :: Array a -> [a]
toList arr = loop 0

    where

    len = length arr
    loop i | i == len = []
    loop i = getIndexUnsafe arr i : loop (i + 1)

{-# INLINE_NORMAL readStreamD #-}
readStreamD :: MonadIO m => Array a -> D.Stream m a
readStreamD = MArray.toStreamD . unsafeThaw

{-# INLINE_NORMAL readRevStreamD #-}
readRevStreamD :: Monad m => Array a -> D.Stream m a
readRevStreamD arr@Array{..} =
    D.map (getIndexUnsafe arr)
        $ D.enumerateFromThenToIntegral (arrLen - 1) (arrLen - 2) 0

{-# INLINE_EARLY read #-}
read :: MonadIO m => Array a -> Stream m a
read = Stream.fromStreamD . readStreamD

{-# INLINE_EARLY readRev #-}
readRev :: Monad m => Array a -> Stream m a
readRev = Stream.fromStreamD . readRevStreamD

-------------------------------------------------------------------------------
-- Elimination - using Folds
-------------------------------------------------------------------------------

{-# INLINE_NORMAL foldl' #-}
foldl' :: (b -> a -> b) -> b -> Array a -> b
foldl' f z arr = unsafePerformIO $ D.foldl' f z $ readStreamD arr

{-# INLINE_NORMAL foldr #-}
foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f z arr = unsafePerformIO $ D.foldr f z $ readStreamD arr

instance NFData a => NFData (Array a) where
    {-# INLINE rnf #-}
    rnf = foldl' (\_ x -> rnf x) ()

{-# INLINE fold #-}
fold :: MonadIO m => Fold m a b -> Array a -> m b
fold f arr = D.fold f (readStreamD arr)

{-# INLINE streamFold #-}
streamFold :: MonadIO m => (Stream m a -> m b) -> Array a -> m b
streamFold f arr = f (read arr)

-------------------------------------------------------------------------------
-- Random reads and writes
-------------------------------------------------------------------------------

-- | /O(1)/ Lookup the element at the given index. Index starts from 0. Does
-- not check the bounds.
--
-- @since 0.8.0
{-# INLINE getIndexUnsafe #-}
getIndexUnsafe :: Array a -> Int -> a
getIndexUnsafe arr i =
    unsafePerformIO $ MArray.getIndexUnsafe (unsafeThaw arr) i

{-# INLINE writeLastN #-}
writeLastN :: MonadIO m => Int -> Fold m a (Array a)
writeLastN n
    | n <= 0 = fmap (const nil) FL.drain
    | otherwise = Fold step initial done

    where

    initial = do
        rb <- liftIO $ RB.createRing n
        return $ FL.Partial $ Tuple' rb (0 :: Int)

    step (Tuple' rb rh) x = do
        liftIO $ RB.unsafeInsertRing rb rh x
        return $ FL.Partial $ Tuple' rb (rh + 1)

    done (Tuple' rb rh) = do
        arr' <- liftIO $ MArray.newArray (min rh n)
        ref <- liftIO $ readIORef $ RB.ringHead rb
        if rh < n
        then
            MArray.putSliceUnsafe (RB.arr rb) 0 arr' 0 ref
        else do
            MArray.putSliceUnsafe (RB.arr rb) ref arr' 0 (n - ref)
            MArray.putSliceUnsafe (RB.arr rb) 0 arr' (n - ref) ref
        return $ unsafeFreeze arr'

sliceUnsafe :: Int -> Int -> Array a -> Array a
sliceUnsafe offset len (Array cont off1 _) = Array cont (off1 + offset) len

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
               else unsafeFreeze $ unsafePerformIO $ do
                   let newLen = indexR - indexL + 1
                       arrThawed = unsafeThaw (sliceUnsafe indexL newLen arr)
                   MArray.clone arrThawed

    where

    getIndexR idx
        | idx < 0 = idx
        | otherwise =
            if p (getIndexUnsafe arr idx) then getIndexR (idx - 1) else idx

    getIndexL idx = if p (getIndexUnsafe arr idx)
                    then getIndexL (idx + 1)
                    else idx

-- | Write an input stream of (index, value) pairs to an array. Throws an
-- error if any index is out of bounds.
--
-- /Pre-release/
{-# INLINE putIndices #-}
putIndices :: MonadIO m
    => Array a -> Fold m (Int, a) ()
putIndices arr = FL.rmapM (\ _ -> return ()) (FL.foldlM' step initial)

    where

    initial = return $ unsafeThaw arr

    step marr (i, x) = do
        MArray.putIndexUnsafe marr i x
        return marr

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Eq a => Eq (Array a) where
    a1 == a2 = lenA1 == lenA2 && loop (lenA1 - 1)

        where

        lenA1 = length a1
        lenA2 = length a2

        loop i
            | i < 0 = True
            | otherwise =
                let v1 = getIndexUnsafe a1 i
                    v2 = getIndexUnsafe a2 i
                 in v1 == v2 && loop (i - 1)

instance Ord a => Ord (Array a) where
    compare a1 a2 =
        case compare lenA1 lenA2 of
            LT -> LT
            GT -> GT
            EQ -> loop 0

        where

        lenA1 = length a1
        lenA2 = length a2

        loop i
            | i >= lenA1 = EQ
            | otherwise =
                let v1 = getIndexUnsafe a1 i
                    v2 = getIndexUnsafe a2 i
                 in case compare v1 v2 of
                        LT -> LT
                        GT -> GT
                        EQ -> loop (i + 1)

instance Show a => Show (Array a) where
    show arr = "fromList " ++ show (toList arr)

instance Read a => Read (Array a) where
    readPrec = do
        fromListWord <- replicateM 9 ReadPrec.get
        if fromListWord == "fromList "
        then fromList <$> readPrec
        else ReadPrec.pfail
