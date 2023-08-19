-- |
-- Module      : Streamly.Internal.Data.Ring.Generic
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Internal.Data.Ring.Generic
    ( Ring(..)

    -- * Generation
    , createRing
    , writeLastN

    -- * Modification
    , seek
    , unsafeInsertRingWith

    -- * Conversion
    , toMutArray
    , toStreamWith
    ) where

#include "assert.hs"

import Control.Monad.IO.Class (liftIO, MonadIO)
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.MutArray.Generic
    ( MutArray(..)
    , new
    , uninit
    , putIndexUnsafe
    , putSliceUnsafe
    )
-- import qualified Streamly.Internal.Data.Stream.Type as Stream
import qualified Streamly.Internal.Data.Fold.Type as Fold

-- XXX Use MutableArray rather than keeping a MutArray here.
data Ring a = Ring
    { ringArr :: MutArray a
    -- XXX We can keep the current fill amount, Or we can keep a count of total
    -- elements inserted and compute ring head as well using mod on that,
    -- assuming it won't overflow. But mod could be expensive.
    , ringHead :: !Int -- current index to be over-written
    , ringMax :: !Int  -- first index beyond allocated memory
    }

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-- XXX If we align the ringMax to nearest power of two then computation of the
-- index to write could be cheaper.
{-# INLINE createRing #-}
createRing :: MonadIO m => Int -> m (Ring a)
createRing count = liftIO $ do
    arr <- new count
    arr1 <- uninit arr count
    return (Ring
        { ringArr = arr1
        , ringHead = 0
        , ringMax = count
        })


{-# INLINE writeLastN #-}
writeLastN :: MonadIO m => Int -> Fold m a (Ring a)
writeLastN n = Fold step initial extract

    where

    initial = do
        if n <= 0
        then Fold.Done <$> createRing 0
        else do
            rb <- createRing n
            return $ Fold.Partial $ Tuple' rb (0 :: Int)

    step (Tuple' rb cnt) x = do
        rh1 <- liftIO $ unsafeInsertRingWith rb x
        return $ Fold.Partial $ Tuple' (rb {ringHead = rh1}) (cnt + 1)

    extract (Tuple' rb@Ring{..} cnt) =
        return $
            if cnt < ringMax
            then Ring ringArr 0 ringHead
            else rb

-------------------------------------------------------------------------------
-- Modification
-------------------------------------------------------------------------------

-- XXX This is safe
-- Take the ring head and return the new ring head.
{-# INLINE unsafeInsertRingWith #-}
unsafeInsertRingWith :: Ring a -> a -> IO Int
unsafeInsertRingWith Ring{..} x = do
    assertM(ringMax >= 1)
    assertM(ringHead < ringMax)
    putIndexUnsafe ringHead ringArr x
    let rh1 = ringHead + 1
        next = if rh1 == ringMax then 0 else rh1
    return next

-- | Move the ring head clockwise (+ve adj) or counter clockwise (-ve adj) by
-- the given amount.
{-# INLINE seek #-}
seek :: MonadIO m => Int -> Ring a -> m (Ring a)
seek adj rng@Ring{..}
    | ringMax > 0 = liftIO $ do
        -- XXX try avoiding mod when in bounds
        let idx1 = ringHead + adj
            next = mod idx1 ringMax
        return $ Ring ringArr next ringMax
    | otherwise = pure rng

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------

-- | @toMutArray rignHeadAdjustment lengthToRead ring@.
-- Convert the ring into a boxed mutable array. Note that the returned MutArray
-- may share the same underlying memory as the Ring.
{-# INLINE toMutArray #-}
toMutArray :: MonadIO m => Int -> Int -> Ring a -> m (MutArray a)
toMutArray adj n Ring{..} = do
    let len = min ringMax n
    let idx = mod (ringHead + adj) ringMax
        end = idx + len
    if end <= ringMax
    then
        -- putSliceUnsafe ringArr idx arr1 0 len
        return $ ringArr { arrStart = idx, arrLen = len }
    else do
        -- XXX Just swap the elements in the existing ring and return the
        -- same array without reallocation.
        arr <- liftIO $ new len
        arr1 <- uninit arr len
        putSliceUnsafe ringArr idx arr1 0 (ringMax - idx)
        putSliceUnsafe ringArr 0 arr1 (ringMax - idx) (end - ringMax)
        return arr1

-- This would be theoretically slower than toMutArray because of a branch
-- introduced for each element in the second half of the ring.

-- | Seek by n and then read the entire ring. Use 'take' on the stream to
-- restrict the reads.
toStreamWith :: Int -> Ring a -> Stream m a
toStreamWith = undefined
{-
toStreamWith n Ring{..}
    | ringMax > 0 = concatEffect $ liftIO $ do
        idx <- readIORef ringHead
        let idx1 = idx + adj
            next = mod idx1 ringMax
            s1 = undefined  -- stream initial slice
            s2 = undefined  -- stream next slice
        return (s1 `Stream.append` s2)
    | otherwise = Stream.nil
-}
