-- |
-- Module      : Streamly.Internal.Data.Array.Generic
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
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

    , fromListN
    , fromList

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
    , strip
    )
where

#include "inline.hs"

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import GHC.Base (MutableArray#, RealWorld)
import GHC.IO (unsafePerformIO)
import Text.Read (readPrec)

import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Stream.StreamD.Type (Stream)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.System.IO (unsafeInlineIO)

import qualified Streamly.Internal.Data.Array.Generic.Mut.Type as MArray
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Producer.Type as Producer
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Ring as RB
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamD.Generate as D
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

{-# INLINE writeLastN #-}
writeLastN :: MonadIO m => Int -> Fold m a (Array a)
writeLastN n
    | n <= 0 = fmap (const nil) FL.drain
    | otherwise = Fold step initial done

    where

    initial = do
        rb <- liftIO $ RB.createRing n
        return $ FL.Partial $ Tuple' rb (0 :: Int)

    step (Tuple' rb cnt) x = do
        liftIO $ RB.unsafeInsertRing rb x
        return $ FL.Partial $ Tuple' rb (cnt + 1)

    done (Tuple' rb cnt) = do
        let len = min cnt n
        arr <- liftIO $ MArray.new len
        arr1 <- MArray.uninit arr len
        -- XXX Should use an API for retrieving the ring head
        rhead <- liftIO $ readIORef $ RB.ringHead rb
        if cnt <= n
        then do
            MArray.putSliceUnsafe (RB.ringArr rb) 0 arr1 0 cnt
        else do
            MArray.putSliceUnsafe (RB.ringArr rb) rhead arr1 0 (n - rhead)
            MArray.putSliceUnsafe (RB.ringArr rb) 0 arr1 (n - rhead) rhead
        return $ unsafeFreeze arr1

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
            if p (getIndexUnsafe idx arr) then getIndexR (idx - 1) else idx

    getIndexL idx = if p (getIndexUnsafe idx arr)
                    then getIndexL (idx + 1)
                    else idx

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
                let v1 = getIndexUnsafe i a1
                    v2 = getIndexUnsafe i a2
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
                let v1 = getIndexUnsafe i a1
                    v2 = getIndexUnsafe i a2
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
