-- |
-- Module      : Streamly.Internal.Data.Ring
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A ring array is a circular mutable array.

-- XXX Write benchmarks
-- XXX Make the implementation similar to mutable array
-- XXX Rename this module to Data.RingArray.Storable

module Streamly.Internal.Data.Ring
    ( Ring(..)

    -- * Construction
    , emptyOf
    , createOf

    , advance
    , moveBy

    -- * Random writes
    , unsafeInsert
    , unsafePutIndex
    , slide
    , putIndex
    , modifyIndex

    -- * Unfolds
    , read
    , readRev

    -- * Random reads
    , getIndex
    , unsafeGetIndex
    , getIndexRev

    -- * Size
    , length
    , byteLength
    -- , capacity
    , byteCapacity
    , bytesFree

    -- * Casting
    , cast
    , castUnsafe
    , asBytes
    , fromArray

    -- * Folds
    , unsafeFoldRing
    , unsafeFoldRingM
    , unsafeFoldRingFullM
    , unsafeFoldRingNM

    -- * Stream of Arrays
    , ringsOf

    -- * Fast Byte Comparisons
    , unsafeEqArray
    , unsafeEqArrayN

    , slidingWindow
    , slidingWindowWith
    ) where

#include "ArrayMacros.h"
#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Streamly.Internal.Data.Unbox as Unboxed (Unbox(..))
import Streamly.Internal.Data.MutArray.Type (MutArray)
import Streamly.Internal.Data.Fold.Type (Fold(..), Step(..), lmap)
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Stream.Step (Step(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.System.IO (unsafeInlineIO)
import Streamly.Internal.Data.MutByteArray.Type (MutByteArray)
import Data.Proxy (Proxy(..))

import qualified Streamly.Internal.Data.MutByteArray.Type as MBA

import qualified Streamly.Internal.Data.MutArray.Type as MA
import qualified Streamly.Internal.Data.Array.Type as A

import Prelude hiding (length, concat, read)

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Internal.Data.Ring as Ring

-- | A ring buffer is a mutable array of fixed size. Initially the array is
-- empty, with ringStart pointing at the start of allocated memory. We call the
-- next location to be written in the ring as ringHead. Initially ringHead ==
-- ringStart. When the first item is added, ringHead points to ringStart +
-- sizeof item. When the buffer becomes full ringHead would wrap around to
-- ringStart. When the buffer is full, ringHead always points at the oldest
-- item in the ring and the newest item added always overwrites the oldest
-- item.
--
-- When using it we should keep in mind that a ringBuffer is a mutable data
-- structure. We should not leak out references to it for immutable use.
--
data Ring a = Ring
    { ringContents :: {-# UNPACK #-} !MutByteArray
    , ringCapacity :: {-# UNPACK #-} !Int
    }

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- XXX Should we inline this?
{-# INLINEABLE mod1 #-}
mod1 :: Int -> Int -> Int
mod1 i j | i < j = i
mod1 i j = mod1 i (i - j)

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | @new count@ allocates an empty ring array that can hold 'count' items. The
-- memory of the ring is uninitialized and the allocation is aligned as per
-- the 'Unbox' instance of the type.
--
{-# INLINE emptyOf #-}
emptyOf :: forall a. Unbox a => Int -> IO (Ring a)
emptyOf count = do
    arr <- MBA.new (count * SIZE_OF(a))
    pure $ Ring arr count

-- | Advance the ringHead by 1 item, wrap around if we hit the end of the
-- array.
{-# INLINE advance #-}
advance :: Ring a -> Int -> Int
advance rb ringHead =
    let newHead = ringHead + 1
     in if newHead >= ringCapacity rb
        then 0
        else newHead

-- | Move the ringHead by n items. The direction depends on the sign on whether
-- n is positive or negative. Wrap around if we hit the beginning or end of the
-- array.
{-# INLINE moveBy #-}
moveBy :: Int -> Ring a -> Int -> Int
moveBy by rb ringHead = (ringHead + by) `mod1` ringCapacity rb

-- XXX Move the createOfLast from array module here.
--
-- | @createOf n@ is a rolling fold that keeps the last n elements of the stream
-- in a ring array.
--
-- /Unimplemented/
{-# INLINE createOf #-}
createOf :: -- (Unbox a, MonadIO m) =>
    Int -> Fold m a (Ring a)
createOf = undefined

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

-- | Cast a mutable array to a ring array.
fromArray :: MutArray a -> Ring a
fromArray = undefined

-------------------------------------------------------------------------------
-- Conversion to/from array
-------------------------------------------------------------------------------

-- | Modify a given index of a ring array using a modifier function.
--
-- /Unimplemented/
modifyIndex :: -- forall m a b. (MonadIO m, Unbox a) =>
    Ring a -> Int -> (a -> (a, b)) -> m b
modifyIndex = undefined

-- | /O(1)/ Write the given element at the given index in the ring array.
-- Performs in-place mutation of the array.
--
-- >>> putIndex arr ix val = Ring.modifyIndex arr ix (const (val, ()))
--
{-# INLINE putIndex #-}
putIndex :: forall m a. (MonadIO m, Unbox a) => Ring a -> Int -> a -> m ()
putIndex ring i = unsafePutIndex ring normalizedI
    where
    normalizedI = i `mod1` ringCapacity ring

-- | Unsafe version of "putIndex". Unsafe because it does not check the bounds
-- of the ring array.
--
{-# INLINE unsafePutIndex #-}
unsafePutIndex :: forall m a. (MonadIO m, Unbox a) => Ring a -> Int -> a -> m ()
unsafePutIndex ring i val =
    liftIO $ pokeAt (i * SIZE_OF(a)) (ringContents ring) val

-- | Insert an item at the head of the ring, when the ring is full this
-- replaces the oldest item in the ring with the new item. This is unsafe
-- beause ringHead supplied is not verified to be within the Ring.
{-# INLINE unsafeInsert #-}
unsafeInsert :: forall a. Unbox a => Ring a -> Int -> a -> IO Int
unsafeInsert rb ringHead newVal = do
    pokeAt (ringHead * SIZE_OF(a)) (ringContents rb) newVal
    pure $ advance rb ringHead

-- | Insert an item at the head of the ring, when the ring is full this
-- replaces the oldest item in the ring with the new item.
--
-- /Unimplemented/
slide :: -- forall m a. (MonadIO m, Unbox a) =>
    Ring a -> a -> m (Ring a)
slide = undefined

-------------------------------------------------------------------------------
-- Random reads
-------------------------------------------------------------------------------

-- | Return the element at the specified index without checking the bounds.
--
-- Unsafe because it does not check the bounds of the ring array.
{-# INLINE unsafeGetIndex #-}
unsafeGetIndex :: forall m a. (MonadIO m, Unbox a) => Int -> Ring a -> m a
unsafeGetIndex i ring = liftIO $ peekAt (i * SIZE_OF(a)) (ringContents ring)

-- | /O(1)/ Lookup the element at the given index. Index starts from 0.
--
{-# INLINE getIndex #-}
getIndex :: -- (MonadIO m, Unbox a) =>
    Ring a -> Int -> m a
getIndex = undefined

-- | /O(1)/ Lookup the element at the given index from the end of the array.
-- Index starts from 0.
--
-- Slightly faster than computing the forward index and using getIndex.
--
{-# INLINE getIndexRev #-}
getIndexRev :: -- (MonadIO m, Unbox a) =>
    Ring a -> Int -> m a
getIndexRev = undefined

-------------------------------------------------------------------------------
-- Size
-------------------------------------------------------------------------------

-- | /O(1)/ Get the byte length of the array.
--
-- /Unimplemented/
{-# INLINE byteLength #-}
byteLength :: Ring a -> Int
byteLength = undefined

-- | /O(1)/ Get the length of the array i.e. the number of elements in the
-- array.
--
-- Note that 'byteLength' is less expensive than this operation, as 'length'
-- involves a costly division operation.
--
-- /Unimplemented/
{-# INLINE length #-}
length :: -- forall a. Unbox a =>
    Ring a -> Int
length = undefined

-- | Get the total capacity of an array. An array may have space reserved
-- beyond the current used length of the array.
--
-- /Pre-release/
{-# INLINE byteCapacity #-}
byteCapacity :: Ring a -> Int
byteCapacity = undefined

-- | The remaining capacity in the array for appending more elements without
-- reallocation.
--
-- /Pre-release/
{-# INLINE bytesFree #-}
bytesFree :: Ring a -> Int
bytesFree = undefined

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

-- XXX We can read the ring in a loop and use "take" to restrict the number of
-- elements to be taken.
--
-- | Read n elements from the ring starting at the supplied ring head. If n is
-- more than the ring size it keeps reading the ring in a circular fashion.
--
-- If the ring is not full the user must ensure than n is less than or equal to
-- the number of valid elements in the ring.
--
-- /Internal/
{-# INLINE_NORMAL read #-}
read :: forall m a. (MonadIO m, Unbox a) => Unfold m (Ring a, Int, Int) a
read = Unfold step return

    where

    step (rb, rh, n) = do
        if n <= 0
        then return Stop
        else do
            x <- unsafeGetIndex rh rb
            let rh1 = advance rb rh
            return $ Yield x (rb, rh1, n - 1)

-- | Unfold a ring array into a stream in reverse order.
--
-- /Unimplemented/
{-# INLINE_NORMAL readRev #-}
readRev :: -- forall m a. (MonadIO m, Unbox a) =>
    Unfold m (MutArray a) a
readRev = undefined

-------------------------------------------------------------------------------
-- Stream of arrays
-------------------------------------------------------------------------------

-- XXX Move this module to a lower level Ring/Type module and move ringsOf to a
-- higher level ring module where we can import "scan".

-- | @ringsOf n stream@ groups the input stream into a stream of
-- ring arrays of size n. Each ring is a sliding window of size n.
--
-- /Unimplemented/
{-# INLINE_NORMAL ringsOf #-}
ringsOf :: -- forall m a. (MonadIO m, Unbox a) =>
    Int -> Stream m a -> Stream m (MutArray a)
ringsOf = undefined -- Stream.scan (createOf n)

-------------------------------------------------------------------------------
-- Casting
-------------------------------------------------------------------------------

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The array size must be a multiple of the size of type @b@.
--
-- /Unimplemented/
--
castUnsafe :: Ring a -> Ring b
castUnsafe = undefined

-- | Cast an @Array a@ into an @Array Word8@.
--
-- /Unimplemented/
--
asBytes :: Ring a -> Ring Word8
asBytes = castUnsafe

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The length of the array should be a multiple of the size of the
-- target element otherwise 'Nothing' is returned.
--
-- /Pre-release/
--
cast :: forall a b. Unbox b => Ring a -> Maybe (Ring b)
cast arr =
    let len = byteLength arr
        r = len `mod` SIZE_OF(b)
     in if r /= 0
        then Nothing
        else Just $ castUnsafe arr

-------------------------------------------------------------------------------
-- Equality
-------------------------------------------------------------------------------

-- XXX remove all usage of unsafeInlineIO
--
-- | Like 'unsafeEqArray' but compares only N bytes instead of entire length of
-- the ring buffer. This is unsafe because the ringHead is not checked to
-- be in range.
{-# INLINE unsafeEqArrayN #-}
unsafeEqArrayN :: forall a. Unbox a => Ring a -> Int -> A.Array a -> Int -> Bool
unsafeEqArrayN Ring{..} rh A.Array{..} nBytes
    | nBytes < 0 = error "unsafeEqArrayN: n should be >= 0"
    | nBytes == 0 = True
    | otherwise = unsafeInlineIO $ check (rh * SIZE_OF(a)) 0

    where

    check p i = do
        (relem :: Word8) <- peekAt p ringContents
        aelem <- peekAt i arrContents
        if relem == aelem
        then go (p + 1) (i + 1)
        else return False

    go p i
        | i == nBytes = return True
        | p == (ringCapacity * SIZE_OF(a)) = go 0 i
        | p == (rh * SIZE_OF(a)) = return True
        | otherwise = check p i

-- XXX This is not modular. We should probably just convert the array and the
-- ring buffer to streams and compare the two streams. Need to check perf
-- though.

-- | Byte compare the entire length of ringBuffer with the given array,
-- starting at the supplied ringHead index.  Returns true if the Array and
-- the ringBuffer have identical contents.
--
-- This is unsafe because the ringHead is not checked to be in range. The
-- supplied array must be equal to or bigger than the ringBuffer, ARRAY BOUNDS
-- ARE NOT CHECKED.
{-# INLINE unsafeEqArray #-}
unsafeEqArray :: forall a. Unbox a => Ring a -> Int -> A.Array a -> Bool
unsafeEqArray Ring{..} rh A.Array{..} =
    unsafeInlineIO $ check (rh * SIZE_OF(a)) 0

    where

    check p i = do
        (relem :: Word8) <- peekAt p ringContents
        aelem <- peekAt i arrContents
        if relem == aelem
        then go (p + 1) (i + 1)
        else return False

    go p i
        | p == (ringCapacity * SIZE_OF(a)) = go 0 i
        | p == (rh * SIZE_OF(a)) = return True
        | otherwise = check p i

-------------------------------------------------------------------------------
-- Folding
-------------------------------------------------------------------------------

-- XXX How does repeated multiplication effect performance? Should we track the
-- byte index instead?

-- XXX We can unfold it into a stream and fold the stream instead.
-- XXX use MonadIO
--
-- | Fold the buffer starting from ringStart up to the given index using a pure
-- step function. This is useful to fold the items in the ring when the ring is
-- not full. The supplied index is usually the end of the ring.
--
-- Unsafe because the supplied index is not checked to be in range.
{-# INLINE unsafeFoldRing #-}
unsafeFoldRing :: forall a b. Unbox a
    => Int -> (b -> a -> b) -> b -> Ring a -> b
unsafeFoldRing !len f z rb =
    let !res = unsafeInlineIO $ go z 0
    in res
    where
      go !acc !p
        | p == len = return acc
        | otherwise = do
            x <- unsafeGetIndex p rb
            go (f acc x) (p + 1)

-- | Like unsafeFoldRing but with a monadic step function.
{-# INLINE unsafeFoldRingM #-}
unsafeFoldRingM :: forall m a b. (MonadIO m, Unbox a)
    => Int -> (b -> a -> m b) -> b -> Ring a -> m b
unsafeFoldRingM !len f z rb = go z 0
  where
    go !acc !start
        | start == len = return acc
        | otherwise = do
            x <- unsafeGetIndex start rb
            acc1 <- f acc x
            go acc1 (start + 1)

-- | Fold the entire length of a ring buffer starting at the supplied ringHead
-- index.  Assuming the supplied ringHead index points to the oldest item,
-- this would fold the ring starting from the oldest item to the newest item in
-- the ring.
--
-- Note, this will crash on ring of 0 size.
--
{-# INLINE unsafeFoldRingFullM #-}
unsafeFoldRingFullM :: forall m a b. (MonadIO m, Unbox a)
    => Int -> (b -> a -> m b) -> b -> Ring a -> m b
unsafeFoldRingFullM rh f z rb = go z rh
  where
    go !acc !start = do
        x <- unsafeGetIndex start rb
        acc' <- f acc x
        let ptr = advance rb start
        if ptr == rh
            then return acc'
            else go acc' ptr

-- | Fold @Int@ items in the ring starting at the given index. Won't fold more
-- than the length of the ring.
--
-- Note, this will crash on ring of 0 size.
--
{-# INLINE unsafeFoldRingNM #-}
unsafeFoldRingNM :: forall m a b. (MonadIO m, Unbox a)
    => Int -> Int -> (b -> a -> m b) -> b -> Ring a -> m b
unsafeFoldRingNM count rh f z rb = go count z rh

    where

    go 0 acc _ = return acc
    go !n !acc !start = do
        x <- unsafeGetIndex start rb
        acc' <- f acc x
        let ptr = advance rb start
        if ptr == rh || n == 0
            then return acc'
            else go (n - 1) acc' ptr

data Tuple4' a b c d = Tuple4' !a !b !c !d deriving Show

-- | Like slidingWindow but also provides the entire ring contents as an Array.
-- The array reflects the state of the ring after inserting the incoming
-- element.
--
-- IMPORTANT NOTE: The ring is mutable, therefore, the result of @(m (Array
-- a))@ action depends on when it is executed. It does not capture the sanpshot
-- of the ring at a particular time.
{-# INLINE slidingWindowWith #-}
slidingWindowWith :: forall m a b. (MonadIO m, Unbox a)
    => Int -> Fold m ((a, Maybe a), m (MutArray a)) b -> Fold m a b
slidingWindowWith n (Fold step1 initial1 extract1 final1) =
    Fold step initial extract final

    where

    initial = do
        if n <= 0
        then error "Window size must be > 0"
        else do
            r <- initial1
            rb <- liftIO $ emptyOf n
            return $
                case r of
                    Partial s -> Partial $ Tuple4' rb 0 (0 :: Int) s
                    Done b -> Done b

    toArray foldRing rb rh = do
        -- Using unpinned array here instead of pinned
        arr <- liftIO $ MA.emptyOf n
        let snoc' b a = liftIO $ MA.unsafeSnoc b a
        foldRing rh snoc' arr rb

    step (Tuple4' rb rh i st) a
        | i < n = do
            rh1 <- liftIO $ unsafeInsert rb rh a
            -- NOTE: We use (rh + 1) instead of rh1 in the code below as if we
            -- are at the last element of the ring, rh1 would become 0 and we
            -- would not fold anything.
            let action = toArray unsafeFoldRingM rb (rh + 1)
            r <- step1 st ((a, Nothing), action)
            return $
                case r of
                    Partial s -> Partial $ Tuple4' rb rh1 (i + 1) s
                    Done b -> Done b
        | otherwise = do
            old <- unsafeGetIndex rh rb
            rh1 <- liftIO $ unsafeInsert rb rh a
            r <- step1 st ((a, Just old), toArray unsafeFoldRingFullM rb rh1)
            return $
                case r of
                    Partial s -> Partial $ Tuple4' rb rh1 (i + 1) s
                    Done b -> Done b

    extract (Tuple4' _ _ _ st) = extract1 st

    final (Tuple4' _ _ _ st) = final1 st

-- | @slidingWindow collector@ is an incremental sliding window
-- fold that does not require all the intermediate elements in a computation.
-- This maintains @n@ elements in the window, when a new element comes it slides
-- out the oldest element and the new element along with the old element are
-- supplied to the collector fold.
--
-- The 'Maybe' type is for the case when initially the window is filling and
-- there is no old element.
--
{-# INLINE slidingWindow #-}
slidingWindow :: forall m a b. (MonadIO m, Unbox a)
    => Int -> Fold m (a, Maybe a) b -> Fold m a b
slidingWindow n f = slidingWindowWith n (lmap fst f)
