import Control.Monad (when)
import Control.Monad.Primitive
import Data.Primitive.Types

import Prelude hiding (length, null, last, map, (!!), read, concat)

import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Unfold.Types (Unfold(..))
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

import qualified Streamly.Internal.Data.Stream.Prelude as P
import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.StreamK as K

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Create an 'Array' from the first N elements of a stream. The array is
-- allocated to size N, if the stream terminates before N elements then the
-- array may hold less than N elements.
--
-- /Internal/
{-# INLINE fromStreamN #-}
fromStreamN :: (PrimMonad m, Prim a) => Int -> SerialT m a -> m (Array a)
fromStreamN n m = do
    when (n < 0) $ error "writeN: negative write count specified"
    A.fromStreamDN n $ D.toStreamD m

-- | Create an 'Array' from a stream. This is useful when we want to create a
-- single array from a stream of unknown size. 'writeN' is at least twice
-- as efficient when the size is already known.
--
-- Note that if the input stream is too large memory allocation for the array
-- may fail.  When the stream size is not known, `arraysOf` followed by
-- processing of indvidual arrays in the resulting stream should be preferred.
--
-- /Internal/
{-# INLINE fromStream #-}
fromStream :: (PrimMonad m, Prim a) => SerialT m a -> m (Array a)
fromStream = P.runFold A.write
-- write m = A.fromStreamD $ D.toStreamD m

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- | Convert an 'Array' into a stream.
--
-- /Internal/
{-# INLINE_EARLY toStream #-}
toStream :: (PrimMonad m, K.IsStream t, Prim a) => Array a -> t m a
toStream = D.fromStreamD . A.toStreamD
-- XXX add fallback to StreamK rule
-- {-# RULES "Streamly.Array.read fallback to StreamK" [1]
--     forall a. S.readK (read a) = K.fromArray a #-}

-- | Convert an 'Array' into a stream in reverse order.
--
-- /Internal/
{-# INLINE_EARLY toStreamRev #-}
toStreamRev :: (PrimMonad m, IsStream t, Prim a) => Array a -> t m a
toStreamRev = D.fromStreamD . A.toStreamDRev
-- XXX add fallback to StreamK rule
-- {-# RULES "Streamly.Array.readRev fallback to StreamK" [1]
--     forall a. S.toStreamK (readRev a) = K.revFromArray a #-}

-- | Unfold an array into a stream.
--
-- @since 0.7.0
{-# INLINE_NORMAL read #-}
read :: forall m a. (PrimMonad m, Prim a) => Unfold m (Array a) a
read = Unfold step inject
    where

    inject = return

    {-# INLINE_LATE step #-}
    step (Array _ _ len) | len == 0 = return D.Stop
    step arr@(Array arr# off len) =
            let !x = A.unsafeIndex arr 0
            in return $ D.Yield x (Array arr# (off + 1) (len - 1))

-- | Unfold an array into a stream, does not check the end of the array, the
-- user is responsible for terminating the stream within the array bounds. For
-- high performance application where the end condition can be determined by
-- a terminating fold.
--
-- The following might not be true, not that the representation changed.
-- Written in the hope that it may be faster than "read", however, in the case
-- for which this was written, "read" proves to be faster even though the core
-- generated with unsafeRead looks simpler.
--
-- /Internal/
--
{-# INLINE_NORMAL unsafeRead #-}
unsafeRead :: forall m a. (PrimMonad m, Prim a) => Unfold m (Array a) a
unsafeRead = Unfold step inject
    where

    inject = return

    {-# INLINE_LATE step #-}
    step arr@(Array arr# off len) =
            let !x = A.unsafeIndex arr 0
            in return $ D.Yield x (Array arr# (off + 1) (len - 1))

-- | > null arr = length arr == 0
--
-- /Internal/
{-# INLINE null #-}
null :: Array a -> Bool
null arr = length arr == 0

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- | Fold an array using a 'Fold'.
--
-- /Internal/
{-# INLINE fold #-}
fold :: forall m a b. (PrimMonad m, Prim a) => Fold m a b -> Array a -> m b
fold f arr = P.runFold f (toStream arr :: Serial.SerialT m a)

-- | Fold an array using a stream fold operation.
--
-- /Internal/
{-# INLINE streamFold #-}
streamFold :: (PrimMonad m, Prim a) => (SerialT m a -> m b) -> Array a -> m b
streamFold f arr = f (toStream arr)

-------------------------------------------------------------------------------
-- Random reads
-------------------------------------------------------------------------------

-- | /O(1)/ Lookup the element at the given index, starting from 0.
--
-- /Internal/
{-# INLINE readIndex #-}
readIndex :: Prim a => Array a -> Int -> Maybe a
readIndex arr i =
    if i < 0 || i > length arr - 1
        then Nothing
        else Just $ A.unsafeIndex arr i

-- | > last arr = readIndex arr (length arr - 1)
--
-- /Internal/
{-# INLINE last #-}
last :: Prim a => Array a -> Maybe a
last arr = readIndex arr (length arr - 1)

-------------------------------------------------------------------------------
-- Array stream operations
-------------------------------------------------------------------------------

-- | Convert a stream of arrays into a stream of their elements.
--
-- Same as the following but more efficient:
--
-- > concat = S.concatMap A.read
--
-- @since 0.7.0
{-# INLINE concat #-}
concat :: (IsStream t, PrimMonad m, Prim a) => t m (Array a) -> t m a
-- concat m = D.fromStreamD $ A.flattenArrays (D.toStreamD m)
-- concat m = D.fromStreamD $ D.concatMap A.toStreamD (D.toStreamD m)
concat m = D.fromStreamD $ D.concatMapU read (D.toStreamD m)

-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size in bytes.
--
-- @since 0.7.0
{-# INLINE compact #-}
compact :: (PrimMonad m, Prim a)
    => Int -> SerialT m (Array a) -> SerialT m (Array a)
compact n xs = D.fromStreamD $ A.packArraysChunksOf n (D.toStreamD xs)
